type workflow_listing = {
  workflow_name : string;
  skill_count : int;
}

type listing_entry =
  | Listed of workflow_listing
  | Unparseable of { file : string; problem : string }

let workflows_dir ~jig_dir = Filename.concat jig_dir "workflows"

(* Run records live under .jig so they inherit the same ignore rules as the
   rest of jig's state and never pollute the repo's git status. *)
let runs_dir ~root = Filename.concat (Filename.concat root ".jig") "runs"

(* A workflow is either a module directory (<name>/workflow.yaml, preferred:
   it co-locates AGENTS.md and items) or a flat file (<name>.yaml).
   Resolve to the yaml path and the workflow's directory - the base for its
   AGENTS.md and forEach items. *)
let resolve_workflow ~jig_dir ~name =
  let dir = workflows_dir ~jig_dir in
  let module_form = Filename.concat (Filename.concat dir name) "workflow.yaml" in
  let flat_form = Filename.concat dir (name ^ ".yaml") in
  if Sys.file_exists module_form then
    Ok (module_form, Filename.dirname module_form)
  else if Sys.file_exists flat_form then Ok (flat_form, dir)
  else
    Error
      (Printf.sprintf "workflow %S not found (looked for %s and %s)" name
         module_form flat_form)

(* Read-only discovery of what is runnable in this project. Invalid files
   are reported, not hidden - a typo in one workflow should not make it
   silently disappear from the list. *)
let list_workflows ~jig_dir =
  let directory = workflows_dir ~jig_dir in
  if not (Sys.file_exists directory && Sys.is_directory directory) then
    Error (Printf.sprintf "%s not found" directory)
  else
    (* Both forms: flat <name>.yaml files, and module <name>/workflow.yaml
       directories. Each entry is (display-file, yaml-path). *)
    let entries =
      Sys.readdir directory |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun entry ->
             let path = Filename.concat directory entry in
             if Filename.check_suffix entry ".yaml" then Some (entry, path)
             else
               let module_yaml = Filename.concat path "workflow.yaml" in
               if Sys.is_directory path && Sys.file_exists module_yaml then
                 Some (entry ^ "/workflow.yaml", module_yaml)
               else None)
    in
    Ok
      (List.map
         (fun (file, path) ->
           match Workflow.load ~path with
           | Ok workflow ->
               Listed
                 {
                   workflow_name = workflow.Workflow.name;
                   skill_count =
                     List.length (Workflow.referenced_skills workflow);
                 }
           | Error problem -> Unparseable { file; problem })
         entries)
