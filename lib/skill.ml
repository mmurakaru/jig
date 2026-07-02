(* Skills resolve repo-first: .jig/skills/ always wins, then each configured
   skill_paths directory in order - a repo can shadow an external skill by
   name, and omitting skill_paths keeps resolution repo-local. *)
let search_directories ~jig_dir ~skill_paths =
  Filename.concat jig_dir "skills" :: skill_paths

let candidate ~directory ~name =
  Filename.concat (Filename.concat directory name) "SKILL.md"

let resolve_path ~jig_dir ~skill_paths ~name =
  let directories = search_directories ~jig_dir ~skill_paths in
  match
    List.find_opt
      (fun directory -> Sys.file_exists (candidate ~directory ~name))
      directories
  with
  | Some directory -> Ok (candidate ~directory ~name)
  | None ->
      Error
        (Printf.sprintf "skill: %s not found; searched %s" name
           (String.concat ", " directories))

let load ~jig_dir ~skill_paths ~name =
  Result.bind (resolve_path ~jig_dir ~skill_paths ~name) (fun path ->
      Result.map_error (fun message -> "skill: " ^ message) (File.read path))
