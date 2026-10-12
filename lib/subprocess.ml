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

(* The process group of the child [run] is waiting on, if any; the signal
   forwarding handlers stop it before jig exits. *)
let current_child_process_group : int option ref = ref None

let termination_grace_seconds = 5.0
let child_poll_interval_seconds = 0.1

(* Before the child's setsid the group does not exist yet, so fall back to the pid. *)
let signal_process_group process_group_id signal =
  try Unix.kill (-process_group_id) signal
  with Unix.Unix_error (Unix.ESRCH, _, _) -> (
    try Unix.kill process_group_id signal
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ())

let rec poll_child_exit child_pid =
  match Unix.waitpid [ Unix.WNOHANG ] child_pid with
  | 0, _ -> None
  | _, status -> Some status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll_child_exit child_pid

let rec wait_for_child_until child_pid ~deadline =
  match poll_child_exit child_pid with
  | Some status -> Some status
  | None
    when Option.fold ~none:false
           ~some:(fun deadline -> Unix.gettimeofday () >= deadline)
           deadline ->
      None
  | None ->
      Unix.sleepf child_poll_interval_seconds;
      wait_for_child_until child_pid ~deadline

(** Stops a child's whole process group: SIGTERM, a grace period, then SIGKILL.
    The child must lead its own group, as every [run] child does. *)
let terminate_process_group ?(grace_seconds = termination_grace_seconds)
    child_pid =
  signal_process_group child_pid Sys.sigterm;
  let status =
    match
      wait_for_child_until child_pid
        ~deadline:(Some (Unix.gettimeofday () +. grace_seconds))
    with
    | Some status -> status
    | None ->
        signal_process_group child_pid Sys.sigkill;
        Option.get (wait_for_child_until child_pid ~deadline:None)
  in
  (* Descendants that ignored SIGTERM or outlived the leader. *)
  signal_process_group child_pid Sys.sigkill;
  status

(* A poll loop, not a blocking waitpid, so signal handlers run within one
   tick; past [deadline], [on_deadline] runs before the group is stopped. *)
let wait_for_child child_pid ~deadline ~on_deadline =
  match wait_for_child_until child_pid ~deadline with
  | Some status -> status
  | None ->
      on_deadline ();
      terminate_process_group child_pid

let read_and_remove path =
  let content = In_channel.with_open_text path In_channel.input_all in
  Sys.remove path;
  content

(* Capture into temp files instead of pipes: a child that fills a pipe buffer
   on one stream while we drain the other would deadlock. Fork/exec by hand
   because Unix.create_process cannot change the child's working directory.
   [on_spawn] fires in the parent once the child is running, with the capture
   paths - a live view can tail them while this call waits. The child leads
   its own process group, so jig can stop it with all its descendants; past
   [time_limit_seconds] that group is terminated. *)
let run ?cwd ?on_spawn ?time_limit_seconds ~argv () =
  match argv with
  | [] -> Error "subprocess: empty command"
  | program :: _ -> (
      let argv_array = Array.of_list argv in
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
          try Unix.execvp program argv_array with _ -> Unix._exit 127)
        else (
          Unix.close stdin_fd;
          Unix.close stdout_fd;
          Unix.close stderr_fd;
          current_child_process_group := Some child_pid;
          let status =
            Fun.protect
              ~finally:(fun () -> current_child_process_group := None)
              (fun () ->
                Option.iter
                  (fun notify -> notify ~stdout_path ~stderr_path)
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

(** Forwards SIGINT, SIGTERM and SIGHUP to the running child's process group,
    stops that group, then exits with 128 + the signal number. *)
let install_signal_forwarding () =
  let forwarded_signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ] in
  let forward_and_exit signal =
    (* A repeated Ctrl-C must not cut the grace period short. *)
    List.iter
      (fun forwarded -> Sys.set_signal forwarded Sys.Signal_ignore)
      forwarded_signals;
    Option.iter
      (fun process_group_id ->
        (* Another thread may reap the leader first; the group still needs stopping. *)
        try ignore (terminate_process_group process_group_id)
        with Unix.Unix_error (Unix.ECHILD, _, _) ->
          signal_process_group process_group_id Sys.sigkill)
      !current_child_process_group;
    exit (128 + posix_signal_number signal)
  in
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle forward_and_exit))
    forwarded_signals
