(* Each step child leads its own group; a signal to jig stops every group and worker. *)

(* Unix.waitpid reports signals in OCaml's Sys numbering (negative), not POSIX. *)
let posix_signal_number signal =
  if signal = Sys.sighup then 1
  else if signal = Sys.sigint then 2
  else if signal = Sys.sigquit then 3
  else if signal = Sys.sigabrt then 6
  else if signal = Sys.sigkill then 9
  else if signal = Sys.sigsegv then 11
  else if signal = Sys.sigpipe then 13
  else if signal = Sys.sigalrm then 14
  else if signal = Sys.sigterm then 15
  else abs signal

let termination_grace_seconds = 5.0
let child_poll_interval_seconds = 0.1

(* Step children being waited on, innermost first; a child's pid is its group id. *)
let active_child_process_groups : int list ref = ref []

type worker_process = { worker_pid : int; pipe_from_worker : Unix.file_descr }

(* Forked jig processes (parallel forEach items); each stops its own step on SIGTERM. *)
let worker_processes : worker_process list ref = ref []

(* Forks between fork and registration; the handler must not exit past them. *)
let processes_starting = ref 0

(* Set by a forwarded signal; a polling thread stops everything and exits. *)
let requested_stop_signal : int option ref = ref None

(* A forked worker must skip the at_exit handlers it inherited from the driver. *)
let exit_after_stop = ref exit

(* Before the child's setsid the group does not exist yet, so fall back to the pid. *)
let signal_process_group process_group_id signal =
  try Unix.kill (-process_group_id) signal
  with Unix.Unix_error (Unix.ESRCH, _, _) -> (
    try Unix.kill process_group_id signal
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ())

(* Only after the leader is reaped: its pid may belong to another process by then. *)
let signal_process_group_only process_group_id signal =
  try Unix.kill (-process_group_id) signal
  with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let process_group_has_members process_group_id =
  match Unix.kill (-process_group_id) 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false

let rec poll_child_exit child_pid =
  match Unix.waitpid [ Unix.WNOHANG ] child_pid with
  | 0, _ -> None
  | _, status -> Some status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll_child_exit child_pid

(* None means [stop_waiting] turned true while the child still ran. *)
let rec wait_for_child_until child_pid ~stop_waiting =
  match poll_child_exit child_pid with
  | Some status -> Some status
  | None when stop_waiting () -> None
  | None ->
      Unix.sleepf child_poll_interval_seconds;
      wait_for_child_until child_pid ~stop_waiting

let rec wait_for_empty_group process_group_id ~until =
  if process_group_has_members process_group_id && Unix.gettimeofday () < until
  then (
    Unix.sleepf child_poll_interval_seconds;
    wait_for_empty_group process_group_id ~until)

(** Stops a child's whole process group: SIGTERM, a grace period, then SIGKILL. *)
let terminate_process_group ?(grace_seconds = termination_grace_seconds)
    child_pid =
  let grace_ends_at = Unix.gettimeofday () +. grace_seconds in
  let grace_has_ended () = Unix.gettimeofday () >= grace_ends_at in
  signal_process_group child_pid Sys.sigterm;
  let status =
    match wait_for_child_until child_pid ~stop_waiting:grace_has_ended with
    | Some status -> status
    | None ->
        signal_process_group child_pid Sys.sigkill;
        Option.get
          (wait_for_child_until child_pid ~stop_waiting:(fun () -> false))
  in
  wait_for_empty_group child_pid ~until:grace_ends_at;
  signal_process_group_only child_pid Sys.sigkill;
  status

let terminate_process_group_if_child child_pid =
  try ignore (terminate_process_group child_pid)
  with Unix.Unix_error (Unix.ECHILD, _, _) ->
    signal_process_group_only child_pid Sys.sigkill

let rec wait_for_worker_exit worker_pid =
  match Unix.waitpid [] worker_pid with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      wait_for_worker_exit worker_pid
  | exception Unix.Unix_error _ -> ()

let stop_all_children_and_exit signal =
  List.iter
    (fun group -> signal_process_group group Sys.sigterm)
    !active_child_process_groups;
  List.iter
    (fun worker ->
      (try Unix.kill worker.worker_pid Sys.sigterm
       with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      (* A worker blocked writing to a full pipe gets EPIPE instead of hanging. *)
      try Unix.close worker.pipe_from_worker with Unix.Unix_error _ -> ())
    !worker_processes;
  List.iter terminate_process_group_if_child !active_child_process_groups;
  List.iter (fun worker -> wait_for_worker_exit worker.worker_pid)
    !worker_processes;
  !exit_after_stop (128 + posix_signal_number signal)

(** Exits, after stopping every child and worker, once a forwarded signal arrived. *)
let exit_if_stop_requested () =
  Option.iter stop_all_children_and_exit !requested_stop_signal

let with_process_starting start =
  incr processes_starting;
  Fun.protect ~finally:(fun () -> decr processes_starting) start

(** Waits for a step child; past [deadline], [on_deadline] runs before the group
    is stopped. The flag is true when the deadline, not a signal, stopped it. *)
let wait_for_child child_pid ~deadline ~grace_seconds ~on_deadline =
  let deadline_has_passed () =
    match deadline with
    | Some deadline -> Unix.gettimeofday () >= deadline
    | None -> false
  in
  let stop_waiting () =
    Option.is_some !requested_stop_signal || deadline_has_passed ()
  in
  match wait_for_child_until child_pid ~stop_waiting with
  | Some status -> (status, false)
  | None when Option.is_some !requested_stop_signal ->
      (terminate_process_group child_pid, false)
  | None ->
      (* Even a failed timeout mark must not leave the child running. *)
      (try on_deadline () with _ -> ());
      (terminate_process_group ~grace_seconds child_pid, true)

(** Stops the group of a step child that an earlier jig started: SIGTERM, a
    grace period, then SIGKILL. The caller checks that the pid is still that child. *)
let terminate_recorded_process_group ~grace_seconds process_group_id =
  let grace_ends_at = Unix.gettimeofday () +. grace_seconds in
  signal_process_group process_group_id Sys.sigterm;
  let rec wait_for_exit () =
    let leader_alive =
      match Unix.kill process_group_id 0 with
      | () -> true
      | exception Unix.Unix_error _ -> false
    in
    if
      (leader_alive || process_group_has_members process_group_id)
      && Unix.gettimeofday () < grace_ends_at
    then (
      Unix.sleepf child_poll_interval_seconds;
      wait_for_exit ())
  in
  wait_for_exit ();
  signal_process_group process_group_id Sys.sigkill

(** Forks a tracked step child that runs [in_child] (which must exec), then [wait]s on it. *)
let run_tracked_child ~in_child ~wait =
  let child_pid =
    with_process_starting (fun () ->
        match Unix.fork () with
        | 0 ->
            in_child ();
            Unix._exit 127
        | child_pid ->
            active_child_process_groups :=
              child_pid :: !active_child_process_groups;
            child_pid)
  in
  let untrack () =
    active_child_process_groups :=
      List.filter (( <> ) child_pid) !active_child_process_groups
  in
  match wait child_pid with
  | status ->
      untrack ();
      status
  | exception raised ->
      (* The child must not outlive an error in [on_spawn] or the wait. *)
      terminate_process_group_if_child child_pid;
      untrack ();
      exit_if_stop_requested ();
      raise raised

(** Forks a parallel forEach worker that a forwarded signal reaches; 0 in the worker. *)
let fork_worker_process ~pipe_from_worker =
  with_process_starting (fun () ->
      match Unix.fork () with
      | 0 ->
          worker_processes := [];
          active_child_process_groups := [];
          (* The enclosing [with_process_starting] decrements this to 0 in the worker too. *)
          processes_starting := 1;
          exit_after_stop := Unix._exit;
          (* A caught SIGPIPE turns a write to a closed driver pipe into EPIPE; exec resets it. *)
          Sys.set_signal Sys.sigpipe (Sys.Signal_handle ignore);
          0
      | worker_pid ->
          worker_processes :=
            { worker_pid; pipe_from_worker } :: !worker_processes;
          worker_pid)

let forget_worker_process worker_pid =
  worker_processes :=
    List.filter (fun worker -> worker.worker_pid <> worker_pid)
      !worker_processes

(** On SIGINT, SIGTERM or SIGHUP, stops every step group and worker, then exits 128 + n. *)
let install_signal_forwarding () =
  (* The handler can run on the live view's thread; only polling threads may reap. *)
  let forward_signal signal =
    match (!active_child_process_groups, !worker_processes, !processes_starting) with
    | [], [], 0 -> !exit_after_stop (128 + posix_signal_number signal)
    | _ -> requested_stop_signal := Some signal
  in
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle forward_signal))
    [ Sys.sigint; Sys.sigterm; Sys.sighup ]
