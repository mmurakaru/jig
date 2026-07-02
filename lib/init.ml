let rec ensure_directory path =
  if not (Sys.file_exists path) then (
    ensure_directory (Filename.dirname path);
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

(* Scaffold the embedded starter set into <root>/.jig. Refuses to touch an
   existing .jig - init bootstraps, it never merges or overwrites. *)
let scaffold ~root =
  let jig_dir = Filename.concat root ".jig" in
  if Sys.file_exists jig_dir then
    Error
      (Printf.sprintf
         "%s already exists; jig init only bootstraps a repository that has \
          no .jig yet"
         jig_dir)
  else (
    try
      List.iter
        (fun (relative_path, content) ->
          let path = Filename.concat jig_dir relative_path in
          ensure_directory (Filename.dirname path);
          Out_channel.with_open_text path (fun channel ->
              Out_channel.output_string channel content))
        Template_data.files;
      Ok (List.map fst Template_data.files)
    with
    | Sys_error message -> Error (Printf.sprintf "init: %s" message)
    | Unix.Unix_error (error, _, _) ->
        Error (Printf.sprintf "init: %s" (Unix.error_message error)))
