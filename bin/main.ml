open Cmdliner

let run_workflow workflow task =
  match
    Jig_core.Runner.Default.execute_run ~root:(Sys.getcwd ())
      ~workflow_name:workflow ~task
  with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok (run, path) ->
      Printf.printf "run %s: %s\n" run.Jig_core.Run.id
        (Jig_core.Run.string_of_status run.Jig_core.Run.status);
      List.iter
        (fun step ->
          Printf.printf "  %s: %s\n" step.Jig_core.Run.skill
            (Jig_core.Run.string_of_outcome step.Jig_core.Run.outcome))
        run.Jig_core.Run.steps;
      Printf.printf "run record: %s\n" path;
      (match run.Jig_core.Run.status with
      | Jig_core.Run.Escalated ->
          Printf.printf "escalated: a human needs to look at the last handoff\n"
      | _ -> ());
      if run.Jig_core.Run.status <> Jig_core.Run.Completed then exit 1

let workflow_arg =
  let doc = "Name of the workflow under .jig/workflows/ to execute." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"WORKFLOW" ~doc)

let task_arg =
  let doc = "Description of the task to run the workflow against." in
  Arg.(required & opt (some string) None & info [ "task" ] ~docv:"TASK" ~doc)

let run_cmd =
  let doc = "Execute a workflow against a task." in
  Cmd.v (Cmd.info "run" ~doc) Term.(const run_workflow $ workflow_arg $ task_arg)

let () =
  let doc = "A minimal, agnostic runner for AI-driven development workflows." in
  let info = Cmd.info "jig" ~version:"0.1.0" ~doc in
  exit (Cmd.eval (Cmd.group info [ run_cmd ]))
