type outcome = {
  exit_code : int;
  stdout : string;
  stderr : string;
  (* True when the time limit, not the child, ended it; the exit code is only evidence. *)
  timed_out : bool;
}

(** A wall-clock time limit; [deadline] is in seconds since the epoch. *)
type time_limit = {
  deadline : float;
  grace_seconds : float;
  on_deadline : unit -> unit;
}

(** A time limit that ends [seconds] from now, with nothing to do at the deadline. *)
let time_limit_from_now ~seconds ~grace_seconds =
  {
    deadline = Unix.gettimeofday () +. seconds;
    grace_seconds;
    on_deadline = ignore;
  }

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
   The child leads its own process group; past [time_limit] that group is
   stopped. *)
let run ?cwd ?on_spawn ?(environment = []) ?time_limit ~argv () =
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
        let status, timed_out =
          Process_group.run_tracked_child
            ~in_child:(fun () ->
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
            ~wait:(fun child_pid ->
              Unix.close stdin_fd;
              Unix.close stdout_fd;
              Unix.close stderr_fd;
              Option.iter
                (fun notify -> notify ~child_pid ~stdout_path ~stderr_path)
                on_spawn;
              Process_group.wait_for_child child_pid
                ~deadline:(Option.map (fun limit -> limit.deadline) time_limit)
                ~grace_seconds:
                  (match time_limit with
                  | Some limit -> limit.grace_seconds
                  | None -> Process_group.termination_grace_seconds)
                ~on_deadline:(fun () ->
                  Option.iter (fun limit -> limit.on_deadline ()) time_limit))
        in
        let stdout = read_and_remove stdout_path in
        let stderr = read_and_remove stderr_path in
        Process_group.exit_if_stop_requested ();
        let exit_code =
          match status with
          | Unix.WEXITED code -> code
          | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
              128 + Process_group.posix_signal_number signal
        in
        Ok { exit_code; stdout; stderr; timed_out }
      with
      | Unix.Unix_error (error, _, _) ->
          Error
            (Printf.sprintf "subprocess: failed to run %s: %s" program
               (Unix.error_message error))
      | Sys_error message -> Error (Printf.sprintf "subprocess: %s" message))
