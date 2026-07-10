(* Structural validation happens at parse time (Workflow.of_string rejects
   anything outside the frozen schema); this checks the workflow against the
   project it will run in, resolving skills exactly as the runner does.

   forEach items are an early tripwire only: a file that loads is
   column-checked here, but a file that cannot be loaded is not a validation
   error - an earlier step may produce it. The forEach entry re-loads and
   re-checks before the fan-out spends anything, so a genuinely missing or
   malformed file still fails the run, just at entry rather than up front. *)
let for_each_problems ~workflow_dir (workflow : Workflow.t) =
  List.filter_map
    (function
      | Workflow.Step _ | Workflow.Retry _ -> None
      | Workflow.For_each for_each -> (
          let path = Filename.concat workflow_dir for_each.Workflow.items_file in
          match Items.load ~path with
          | Error _ -> None
          | Ok items -> (
              match
                Items.check_columns ~path
                  ~required:(Workflow.for_each_columns for_each)
                  items
              with
              | Error message -> Some message
              | Ok () -> None)))
    workflow.Workflow.entries

let workflow ~workflow_dir ~jig_dir ~skill_paths (workflow : Workflow.t) =
  let missing =
    List.filter_map
      (fun skill ->
        match Skill.resolve_path ~jig_dir ~skill_paths ~name:skill with
        | Ok _ -> None
        | Error message -> Some message)
      (Workflow.referenced_skills workflow)
  in
  match missing @ for_each_problems ~workflow_dir workflow with
  | [] -> Ok ()
  | problems ->
      Error
        (Printf.sprintf "workflow %S: %s" workflow.Workflow.name
           (String.concat "; " problems))
