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
   and the capture paths - a live view can tail them while this call blocks
   in waitpid.
   [environment] adds or replaces variables for the child only. *)
let run ?cwd ?on_spawn ?(environment = []) ~argv () =
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
          Option.iter
            (fun notify -> notify ~child_pid ~stdout_path ~stderr_path)
            on_spawn;
          let _, status = Unix.waitpid [] child_pid in
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
