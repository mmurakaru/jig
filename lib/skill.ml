let load ~jig_dir ~name =
  let path =
    Filename.concat
      (Filename.concat (Filename.concat jig_dir "skills") name)
      "SKILL.md"
  in
  Result.map_error (fun message -> "skill: " ^ message) (File.read path)
