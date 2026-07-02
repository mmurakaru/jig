module type S = sig
  val save : runs_dir:string -> Run.t -> (string, string) result
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
end
