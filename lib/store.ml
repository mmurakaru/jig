module type S = sig
  val save : runs_dir:string -> Run.t -> (string, string) result
  val load : runs_dir:string -> id:string -> (Run.t, string) result
end

module Filesystem : S = struct
  let ensure_directory path =
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let save ~runs_dir run =
    try
      ensure_directory runs_dir;
      let path = Filename.concat runs_dir (run.Run.id ^ ".json") in
      Out_channel.with_open_text path (fun channel ->
          Yojson.Safe.pretty_to_channel channel (Run.to_json run));
      Ok path
    with
    | Sys_error message -> Error (Printf.sprintf "store: %s" message)
    | Unix.Unix_error (error, _, _) ->
        Error (Printf.sprintf "store: %s" (Unix.error_message error))

  let load ~runs_dir ~id =
    let path = Filename.concat runs_dir (id ^ ".json") in
    Result.bind
      (Result.map_error (fun message -> "store: " ^ message) (File.read path))
      (fun content ->
        let parsed =
          match Yojson.Safe.from_string content with
          | json -> Run.of_json json
          | exception Yojson.Json_error message ->
              Error (Printf.sprintf "not valid json: %s" message)
        in
        Result.map_error
          (fun message ->
            Printf.sprintf
              "store: failed to read %s: %s (the record may have been \
               written by a different jig version)"
              path message)
          parsed)
end
