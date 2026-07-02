type workflow_listing = {
  workflow_name : string;
  skill_count : int;
}

type listing_entry =
  | Listed of workflow_listing
  | Unparseable of { file : string; problem : string }

let workflows_dir ~jig_dir = Filename.concat jig_dir "workflows"

(* Read-only discovery of what is runnable in this project. Invalid files
   are reported, not hidden - a typo in one workflow should not make it
   silently disappear from the list. *)
let list_workflows ~jig_dir =
  let directory = workflows_dir ~jig_dir in
  if not (Sys.file_exists directory && Sys.is_directory directory) then
    Error (Printf.sprintf "%s not found" directory)
  else
    let files =
      Sys.readdir directory |> Array.to_list
      |> List.filter (fun file -> Filename.check_suffix file ".yaml")
      |> List.sort String.compare
    in
    Ok
      (List.map
         (fun file ->
           match Workflow.load ~path:(Filename.concat directory file) with
           | Ok workflow ->
               Listed
                 {
                   workflow_name = workflow.Workflow.name;
                   skill_count =
                     List.length (Workflow.referenced_skills workflow);
                 }
           | Error problem -> Unparseable { file; problem })
         files)
