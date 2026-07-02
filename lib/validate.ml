let skill_exists ~jig_dir name =
  Sys.file_exists
    (Filename.concat
       (Filename.concat (Filename.concat jig_dir "skills") name)
       "SKILL.md")

(* Structural validation happens at parse time (Workflow.of_string rejects
   anything outside the frozen schema); this checks the workflow against the
   project it will run in. *)
let workflow ~jig_dir (workflow : Workflow.t) =
  let missing =
    List.filter
      (fun skill -> not (skill_exists ~jig_dir skill))
      (Workflow.referenced_skills workflow)
  in
  match missing with
  | [] -> Ok ()
  | missing ->
      Error
        (Printf.sprintf
           "workflow %S references skills that do not exist under \
            .jig/skills/: %s"
           workflow.Workflow.name
           (String.concat ", " missing))
