type exec_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

module type S = sig
  val execute :
    command:string list ->
    cwd:string ->
    prompt:string ->
    (exec_result, string) result
end

module Local : S = struct
  let execute ~command ~cwd ~prompt =
    match command with
    | [] -> Error "executor: harness command is empty"
    | _ ->
        Result.map
          (fun outcome ->
            {
              exit_code = outcome.Subprocess.exit_code;
              stdout = outcome.Subprocess.stdout;
              stderr = outcome.Subprocess.stderr;
            })
          (Result.map_error
             (fun message -> "executor: " ^ message)
             (Subprocess.run ~cwd ~argv:(command @ [ prompt ]) ()))
end
