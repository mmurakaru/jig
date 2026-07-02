type exec_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

module type S = sig
  val execute :
    command:string list -> prompt:string -> (exec_result, string) result
end

module Local : S = struct
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

  let read_and_remove path =
    let content = In_channel.with_open_text path In_channel.input_all in
    Sys.remove path;
    content

  (* Capture into temp files instead of pipes: a harness that fills a pipe
     buffer on one stream while we drain the other would deadlock the run. *)
  let execute ~command ~prompt =
    match command with
    | [] -> Error "executor: harness command is empty"
    | program :: _ -> (
        let argv = Array.of_list (command @ [ prompt ]) in
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
          let child_pid =
            Unix.create_process program argv stdin_fd stdout_fd stderr_fd
          in
          Unix.close stdin_fd;
          Unix.close stdout_fd;
          Unix.close stderr_fd;
          let _, status = Unix.waitpid [] child_pid in
          let stdout = read_and_remove stdout_path in
          let stderr = read_and_remove stderr_path in
          let exit_code =
            match status with
            | Unix.WEXITED code -> code
            | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                128 + posix_signal_number signal
          in
          Ok { exit_code; stdout; stderr }
        with
        | Unix.Unix_error (error, _, _) ->
            Error
              (Printf.sprintf "executor: failed to run %s: %s" program
                 (Unix.error_message error))
        | Sys_error message -> Error (Printf.sprintf "executor: %s" message))
end
