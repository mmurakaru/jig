module type S = sig
  val save : runs_dir:string -> Run.t -> (string, string) result
  val load : runs_dir:string -> id:string -> (Run.t, string) result

  (* Newest-first by start time; an empty list when nothing has run. *)
  val list_runs : runs_dir:string -> (Run.t list, string) result
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

  let list_runs ~runs_dir =
    if not (Sys.file_exists runs_dir) then Ok []
    else
      let ids =
        Sys.readdir runs_dir |> Array.to_list
        |> List.filter_map (fun file ->
               if Filename.check_suffix file ".json" then
                 Some (Filename.chop_suffix file ".json")
               else None)
      in
      List.fold_left
        (fun accumulator id ->
          Result.bind accumulator (fun runs ->
              Result.map (fun run -> run :: runs) (load ~runs_dir ~id)))
        (Ok []) ids
      |> Result.map
           (List.sort (fun a b ->
                (* Same-second ties break on the id's sequence suffix:
                   with equal prefixes a longer id is a later run. *)
                match String.compare b.Run.started_at a.Run.started_at with
                | 0 -> (
                    match
                      compare (String.length b.Run.id) (String.length a.Run.id)
                    with
                    | 0 -> String.compare b.Run.id a.Run.id
                    | order -> order)
                | order -> order))
end
