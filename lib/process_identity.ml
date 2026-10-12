(* ps -o lstart= reads the same on macOS and Linux under a fixed TZ and locale. *)
let start_time pid =
  match
    Subprocess.run
      ~environment:[ ("TZ", "UTC"); ("LC_ALL", "C") ]
      ~argv:[ "ps"; "-o"; "lstart="; "-p"; string_of_int pid ]
      ()
  with
  | Ok { Subprocess.exit_code = 0; stdout; _ } when String.trim stdout <> "" ->
      Some (String.trim stdout)
  | Ok _ | Error _ -> None

let pid_exists pid =
  pid > 0
  &&
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.EPERM, _, _) -> true
  | exception Unix.Unix_error _ -> false

(* A different start time means the pid was reused by another process. *)
let is_running ~pid ~recorded_start_time =
  pid_exists pid
  &&
  match (recorded_start_time, start_time pid) with
  | Some recorded, Some current -> recorded = current
  | _ -> true

(* Two harnesses must never edit one workspace. *)
let refuse_while_attempts_run (run : Run.t) =
  let host = Unix.gethostname () in
  let problem (attempt : Run.attempt) =
    match (attempt.Run.child_pid, attempt.Run.child_host) with
    | None, _ -> None
    | Some pid, Some child_host when child_host <> host ->
        Some
          (Printf.sprintf
             "attempt %s ran as pid %d on host %s, so this host cannot tell whether it still runs; \
              resume the run on %s"
             attempt.Run.attempt_id pid child_host child_host)
    | Some pid, _ ->
        if is_running ~pid ~recorded_start_time:attempt.Run.child_start_time then
          Some
            (Printf.sprintf
               "attempt %s is still running as pid %d; stop it or wait for it to exit, then try \
                again"
               attempt.Run.attempt_id pid)
        else None
  in
  match
    List.filter_map problem (List.map snd (Run.Step_key_map.bindings run.Run.attempts_in_flight))
  with
  | [] -> Ok ()
  | problems -> Error (Printf.sprintf "run %s: %s" run.Run.id (String.concat "; " problems))
