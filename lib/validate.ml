(* Structural validation happens at parse time (Workflow.of_string rejects
   anything outside the frozen schema); this checks the workflow against the
   project it will run in, resolving skills exactly as the runner does. *)
let workflow ~jig_dir ~skill_paths (workflow : Workflow.t) =
  let missing =
    List.filter_map
      (fun skill ->
        match Skill.resolve_path ~jig_dir ~skill_paths ~name:skill with
        | Ok _ -> None
        | Error message -> Some message)
      (Workflow.referenced_skills workflow)
  in
  match missing with
  | [] -> Ok ()
  | problems ->
      Error
        (Printf.sprintf "workflow %S: %s" workflow.Workflow.name
           (String.concat "; " problems))
