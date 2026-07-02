open Cmdliner

let report_run (run : Jig_core.Run.t) path =
  Printf.printf "run %s: %s\n" run.Jig_core.Run.id
    (Jig_core.Run.string_of_status run.Jig_core.Run.status);
  List.iter
    (fun step ->
      Printf.printf "  %s: %s\n" step.Jig_core.Run.skill
        (Jig_core.Run.string_of_outcome step.Jig_core.Run.outcome))
    run.Jig_core.Run.steps;
  Printf.printf "run record: %s\n" path;
  match run.Jig_core.Run.status with
  | Jig_core.Run.Completed -> ()
  | Jig_core.Run.Paused ->
      Printf.printf
        "paused: a human needs to look at the last handoff, then jig run \
         --resume %s\n"
        run.Jig_core.Run.id;
      exit 2
  | _ -> exit 1

let run_workflow workflow resume task guidance =
  let root = Sys.getcwd () in
  let result =
    match (workflow, resume, task) with
    | Some workflow_name, None, Some task ->
        Jig_core.Runner.Default.execute_run ~root ~workflow_name ~task
    | None, Some run_id, None ->
        Jig_core.Runner.Default.resume_run ~root ~run_id ~guidance
    | Some _, Some _, _ ->
        Error "pass either a workflow or --resume, not both"
    | Some _, None, None -> Error "running a workflow requires --task"
    | None, Some _, Some _ -> Error "--task belongs to the original run; use --guidance when resuming"
    | None, None, _ -> Error "pass a workflow name or --resume <run-id>"
  in
  match result with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok (run, path) -> report_run run path

let validate_workflow workflow =
  let root = Sys.getcwd () in
  let jig_dir = Filename.concat root ".jig" in
  let path =
    Filename.concat (Filename.concat jig_dir "workflows") (workflow ^ ".yaml")
  in
  let result =
    Result.bind (Jig_core.Workflow.load ~path) (fun parsed ->
        Result.map
          (fun () -> parsed.Jig_core.Workflow.name)
          (Jig_core.Validate.workflow ~jig_dir parsed))
  in
  match result with
  | Ok name -> Printf.printf "workflow %s: ok\n" name
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1

let show_status run_id json_output =
  let root = Sys.getcwd () in
  match
    Jig_core.Store.Filesystem.load
      ~runs_dir:(Filename.concat root "runs")
      ~id:run_id
  with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok run when json_output ->
      print_endline (Yojson.Safe.pretty_to_string (Jig_core.Run.to_json run))
  | Ok run ->
      Printf.printf "run %s: %s\n" run.Jig_core.Run.id
        (Jig_core.Run.string_of_status run.Jig_core.Run.status);
      Printf.printf "workflow: %s\ntask: %s\n" run.Jig_core.Run.workflow
        run.Jig_core.Run.task;
      (match run.Jig_core.Run.error with
      | Some message -> Printf.printf "error: %s\n" message
      | None -> ());
      Printf.printf "steps:\n";
      List.iter
        (fun step ->
          Printf.printf "  %s: %s\n" step.Jig_core.Run.skill
            (Jig_core.Run.string_of_outcome step.Jig_core.Run.outcome))
        run.Jig_core.Run.steps;
      (match Jig_core.Run.last_handoff run with
      | Some handoff ->
          Printf.printf "last handoff:\n%s\n" (Jig_core.Handoff.render handoff)
      | None -> ());
      (match run.Jig_core.Run.status with
      | Jig_core.Run.Paused ->
          Printf.printf
            "paused: resume with jig run --resume %s [--guidance \"...\"]\n"
            run.Jig_core.Run.id
      | _ -> ())

let list_workflows json_output =
  let jig_dir = Filename.concat (Sys.getcwd ()) ".jig" in
  match Jig_core.Project.list_workflows ~jig_dir with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok entries when json_output ->
      let listed =
        List.filter_map
          (function
            | Jig_core.Project.Listed listing ->
                Some
                  (`Assoc
                     [
                       ("name", `String listing.Jig_core.Project.workflow_name);
                       ("skills", `Int listing.Jig_core.Project.skill_count);
                     ])
            | Jig_core.Project.Unparseable _ -> None)
          entries
      in
      print_endline (Yojson.Safe.pretty_to_string (`List listed))
  | Ok entries ->
      if entries = [] then print_endline "no workflows under .jig/workflows/"
      else
        List.iter
          (function
            | Jig_core.Project.Listed listing ->
                Printf.printf "%s (%d skills)\n"
                  listing.Jig_core.Project.workflow_name
                  listing.Jig_core.Project.skill_count
            | Jig_core.Project.Unparseable { file; problem } ->
                Printf.printf "%s: INVALID - %s\n" file problem)
          entries

let optional_workflow_arg =
  let doc = "Name of the workflow under .jig/workflows/ to execute." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"WORKFLOW" ~doc)

let required_workflow_arg =
  let doc = "Name of the workflow under .jig/workflows/ to validate." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKFLOW" ~doc)

let task_arg =
  let doc = "Description of the task to run the workflow against." in
  Arg.(value & opt (some string) None & info [ "task" ] ~docv:"TASK" ~doc)

let resume_arg =
  let doc = "Continue a paused (or interrupted) run from where it stopped." in
  Arg.(value & opt (some string) None & info [ "resume" ] ~docv:"RUN-ID" ~doc)

let guidance_arg =
  let doc =
    "Human guidance injected into the first step executed after a resume."
  in
  Arg.(value & opt (some string) None & info [ "guidance" ] ~docv:"TEXT" ~doc)

let run_cmd =
  let doc = "Execute a workflow against a task, or resume a paused run." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(
      const run_workflow $ optional_workflow_arg $ resume_arg $ task_arg
      $ guidance_arg)

let validate_cmd =
  let doc = "Lint a workflow against the schema and the project's skills." in
  Cmd.v (Cmd.info "validate" ~doc)
    Term.(const validate_workflow $ required_workflow_arg)

let json_flag =
  let doc = "Print machine-readable JSON instead of the human summary." in
  Arg.(value & flag & info [ "json" ] ~doc)

let run_id_arg =
  let doc = "Run id to inspect (the runs/<id>.json record)." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"RUN-ID" ~doc)

let status_cmd =
  let doc = "Inspect a run's progress, outcomes, and last handoff." in
  Cmd.v (Cmd.info "status" ~doc) Term.(const show_status $ run_id_arg $ json_flag)

let list_what_arg =
  let doc = "What to list; only \"workflows\" is supported." in
  Arg.(required & pos 0 (some (enum [ ("workflows", ()) ])) None & info [] ~docv:"WHAT" ~doc)

let list_cmd =
  let doc = "Discover what is runnable in this repository." in
  Cmd.v (Cmd.info "list" ~doc)
    Term.(const (fun () json -> list_workflows json) $ list_what_arg $ json_flag)

let () =
  let doc = "A minimal, agnostic runner for AI-driven development workflows." in
  let info = Cmd.info "jig" ~version:"0.1.0" ~doc in
  exit (Cmd.eval (Cmd.group info [ run_cmd; validate_cmd; status_cmd; list_cmd ]))
