type exec_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  (* Decides a timeout; the exit code can be 0 when a wrapper hides the kill. *)
  timed_out : bool;
}

module type S = sig
  (* Past [time_limit]: on_deadline, then stop the group, then return timed_out. *)
  val execute :
    ?on_spawn:(child_pid:int -> stdout_path:string -> stderr_path:string -> unit) ->
    ?environment:(string * string) list ->
    ?time_limit:Subprocess.time_limit ->
    command:string list ->
    cwd:string ->
    prompt:string ->
    unit ->
    (exec_result, string) result
end

module Local : S = struct
  let execute ?on_spawn ?environment ?time_limit ~command ~cwd ~prompt () =
    match command with
    | [] -> Error "executor: harness command is empty"
    | _ ->
        Result.map
          (fun outcome ->
            {
              exit_code = outcome.Subprocess.exit_code;
              stdout = outcome.Subprocess.stdout;
              stderr = outcome.Subprocess.stderr;
              timed_out = outcome.Subprocess.timed_out;
            })
          (Result.map_error
             (fun message -> "executor: " ^ message)
             (Subprocess.run ~cwd ?on_spawn ?environment ?time_limit
                ~argv:(command @ [ prompt ]) ()))
end
