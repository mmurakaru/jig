type outcome = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

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

(* Children [run] waits on, innermost first ([on_spawn] can start a nested run); pid = group id. *)
let active_child_process_groups : int list ref = ref []

(* Forked jig processes (parallel forEach items); each stops its own step on SIGTERM. *)
let worker_process_ids : int list ref = ref []

(* A forked worker must skip the at_exit handlers it inherited from the driver. *)
let exit_after_stop = ref exit

(* Set by a forwarded signal; the waiting thread stops the group and exits. *)
let requested_stop_signal : int option ref = ref None

let termination_grace_seconds = 5.0
let child_poll_interval_seconds = 0.1

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
let terminate_process_group child_pid =
  let grace_ends_at = Unix.gettimeofday () +. termination_grace_seconds in
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

let wait_for_child child_pid ~deadline ~on_deadline =
  let deadline_has_passed () =
    match deadline with
    | Some deadline -> Unix.gettimeofday () >= deadline
    | None -> false
  in
  let stop_waiting () =
    Option.is_some !requested_stop_signal || deadline_has_passed ()
  in
  match wait_for_child_until child_pid ~stop_waiting with
  | Some status -> status
  | None ->
      if Option.is_none !requested_stop_signal then on_deadline ();
      terminate_process_group child_pid

let rec wait_for_worker_exit worker_pid =
  match Unix.waitpid [] worker_pid with
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_worker_exit worker_pid
  | exception Unix.Unix_error _ -> ()

let stop_all_children_and_exit signal =
  List.iter
    (fun group -> signal_process_group group Sys.sigterm)
    !active_child_process_groups;
  List.iter
    (fun worker_pid ->
      try Unix.kill worker_pid Sys.sigterm
      with Unix.Unix_error (Unix.ESRCH, _, _) -> ())
    !worker_process_ids;
  List.iter
    (fun group ->
      try ignore (terminate_process_group group)
      with Unix.Unix_error (Unix.ECHILD, _, _) ->
        signal_process_group_only group Sys.sigkill)
    !active_child_process_groups;
  List.iter wait_for_worker_exit !worker_process_ids;
  !exit_after_stop (128 + posix_signal_number signal)

(** Exits, after stopping every child and worker, once a forwarded signal arrived. *)
let exit_if_stop_requested () =
  Option.iter stop_all_children_and_exit !requested_stop_signal

(** Tracks a forked parallel forEach worker, so a forwarded signal reaches it. *)
let register_worker_process worker_pid =
  worker_process_ids := worker_pid :: !worker_process_ids

let unregister_worker_process worker_pid =
  worker_process_ids := List.filter (( <> ) worker_pid) !worker_process_ids

(** Called first in a forked parallel forEach worker. *)
let become_worker_process () =
  worker_process_ids := [];
  active_child_process_groups := [];
  exit_after_stop := Unix._exit

(* The parent's environment with each [overrides] name replaced or added,
   as execve expects it: "NAME=value" strings. *)
let environment_with_overrides overrides =
  let overridden entry =
    List.exists
      (fun (name, _) -> String.starts_with ~prefix:(name ^ "=") entry)
      overrides
  in
  let inherited =
    List.filter (fun entry -> not (overridden entry))
      (Array.to_list (Unix.environment ()))
  in
  Array.of_list
    (inherited @ List.map (fun (name, value) -> name ^ "=" ^ value) overrides)

let read_and_remove path =
  let content = In_channel.with_open_text path In_channel.input_all in
  Sys.remove path;
  content

(* Capture into temp files instead of pipes: a child that fills a pipe buffer
   on one stream while we drain the other would deadlock. Fork/exec by hand
   because Unix.create_process cannot change the child's working directory.
   [on_spawn] fires in the parent once the child is running, with its pid
   and the capture paths - a live view can tail them while this call waits.
   [environment] adds or replaces variables for the child only.
   The child leads its own process group; past [time_limit_seconds] that
   group is stopped. *)
let run ?cwd ?on_spawn ?(environment = []) ?time_limit_seconds ~argv () =
  match argv with
  | [] -> Error "subprocess: empty command"
  | program :: _ -> (
      let argv_array = Array.of_list argv in
      let child_environment =
        match environment with
        | [] -> None
        | overrides -> Some (environment_with_overrides overrides)
      in
      try
        let stdout_path = Filename.temp_file "jig-stdout" ".log" in
        let stderr_path = Filename.temp_file "jig-stderr" ".log" in
        let stdin_fd = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
        let stdout_fd =
          Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
        in
        let stderr_fd =
          Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
        in
        let child_pid = Unix.fork () in
        if child_pid = 0 then (
          ignore (Unix.setsid ());
          (try Option.iter Unix.chdir cwd with _ -> Unix._exit 126);
          Unix.dup2 stdin_fd Unix.stdin;
          Unix.dup2 stdout_fd Unix.stdout;
          Unix.dup2 stderr_fd Unix.stderr;
          Unix.close stdin_fd;
          Unix.close stdout_fd;
          Unix.close stderr_fd;
          try
            match child_environment with
            | None -> Unix.execvp program argv_array
            | Some child_environment ->
                Unix.execvpe program argv_array child_environment
          with _ -> Unix._exit 127)
        else (
          Unix.close stdin_fd;
          Unix.close stdout_fd;
          Unix.close stderr_fd;
          active_child_process_groups :=
            child_pid :: !active_child_process_groups;
          let status =
            Fun.protect
              ~finally:(fun () ->
                active_child_process_groups :=
                  List.filter (( <> ) child_pid) !active_child_process_groups)
              (fun () ->
                Option.iter
                  (fun notify -> notify ~child_pid ~stdout_path ~stderr_path)
                  on_spawn;
                wait_for_child child_pid
                  ~deadline:
                    (Option.map
                       (fun limit -> Unix.gettimeofday () +. limit)
                       time_limit_seconds)
                  ~on_deadline:ignore)
          in
          let stdout = read_and_remove stdout_path in
          let stderr = read_and_remove stderr_path in
          exit_if_stop_requested ();
          let exit_code =
            match status with
            | Unix.WEXITED code -> code
            | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                128 + posix_signal_number signal
          in
          Ok { exit_code; stdout; stderr })
      with
      | Unix.Unix_error (error, _, _) ->
          Error
            (Printf.sprintf "subprocess: failed to run %s: %s" program
               (Unix.error_message error))
      | Sys_error message -> Error (Printf.sprintf "subprocess: %s" message))

(** On SIGINT, SIGTERM or SIGHUP, stops the running step's group and exits 128 + n. *)
let install_signal_forwarding () =
  (* The handler can run on the live view's thread; only [run]'s thread may reap. *)
  let forward_signal signal =
    match (!active_child_process_groups, !worker_process_ids) with
    | [], [] -> !exit_after_stop (128 + posix_signal_number signal)
    | _ -> requested_stop_signal := Some signal
  in
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle forward_signal))
    [ Sys.sigint; Sys.sigterm; Sys.sighup ]
