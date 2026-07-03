open Cmdliner

let cost_suffix step =
  match step.Jig_core.Run.cost with
  | Jig_core.Metering.Cost_usd value -> Printf.sprintf " ($%.4f)" value
  | Jig_core.Metering.Unknown_cost -> " (cost unknown)"

let print_steps_with_costs (run : Jig_core.Run.t) =
  List.iter
    (fun step ->
      Printf.printf "  %s: %s%s\n" step.Jig_core.Run.skill
        (Jig_core.Run.string_of_outcome step.Jig_core.Run.outcome)
        (cost_suffix step))
    run.Jig_core.Run.steps;
  let total, unknown = Jig_core.Run.cost_summary run in
  if total > 0.0 || unknown > 0 then
    Printf.printf "total cost: $%.4f%s\n" total
      (if unknown > 0 then
         Printf.sprintf " (+%d step%s with unknown cost)" unknown
           (if unknown = 1 then "" else "s")
       else "")

let report_run (run : Jig_core.Run.t) path =
  Printf.printf "run %s: %s\n" run.Jig_core.Run.id
    (Jig_core.Run.string_of_status run.Jig_core.Run.status);
  print_steps_with_costs run;
  Printf.printf "run record: %s\n" path;
  match run.Jig_core.Run.status with
  | Jig_core.Run.Completed -> ()
  | Jig_core.Run.Paused ->
      (match Jig_core.Run.last_handoff run with
      | Some handoff ->
          Printf.printf "last handoff:\n%s\n" (Jig_core.Handoff.render handoff)
      | None -> ());
      Printf.printf
        "paused: answer with  jig run --resume %s --guidance \"...\"\n\
        \        or reopen the step's session:  jig attach %s\n"
        run.Jig_core.Run.id run.Jig_core.Run.id;
      exit 2
  | _ -> exit 1

let print_step_live step =
  Printf.printf "  %s: %s%s\n%!" step.Jig_core.Run.skill
    (Jig_core.Run.string_of_outcome step.Jig_core.Run.outcome)
    (cost_suffix step)

let run_workflow workflow resume task guidance skip isolated =
  let root = Sys.getcwd () in
  let result =
    match (workflow, resume, task) with
    | _, None, _ when skip -> Error "--skip belongs to --resume"
    | Some workflow_name, None, Some task ->
        Jig_core.Runner.Default.execute_run ~on_step:print_step_live ~root
          ~workflow_name ~task ~isolated ()
    | None, Some _, None when isolated ->
        Error "--isolated belongs to the original run; a resume reuses its workspace"
    | None, Some run_id, None ->
        Jig_core.Runner.Default.resume_run ~on_step:print_step_live ~root
          ~run_id ~guidance ~skip ()
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
        Result.bind (Jig_core.Config.load_skill_paths ~jig_dir)
          (fun skill_paths ->
            Result.map
              (fun () -> parsed.Jig_core.Workflow.name)
              (Jig_core.Validate.workflow ~jig_dir ~skill_paths parsed)))
  in
  match result with
  | Ok name -> Printf.printf "workflow %s: ok\n" name
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1

let latest_run_id ~runs_dir =
  match Jig_core.Store.Filesystem.list_runs ~runs_dir with
  | Error message -> Error message
  | Ok [] -> Error "no runs yet - nothing under runs/"
  | Ok (newest :: _) -> Ok newest.Jig_core.Run.id

let show_status run_id latest json_output =
  let root = Sys.getcwd () in
  let runs_dir = Filename.concat root "runs" in
  let resolved =
    match (run_id, latest) with
    | Some _, true -> Error "pass a run id or --latest, not both"
    | Some id, false -> Ok id
    | None, true -> latest_run_id ~runs_dir
    | None, false -> Error "pass a run id or --latest"
  in
  match
    Result.bind resolved (fun id ->
        Jig_core.Store.Filesystem.load ~runs_dir ~id)
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
      print_steps_with_costs run;
      (match Jig_core.Run.last_handoff run with
      | Some handoff ->
          Printf.printf "last handoff:\n%s\n" (Jig_core.Handoff.render handoff)
      | None -> ());
      (match run.Jig_core.Run.status with
      | Jig_core.Run.Paused ->
          Printf.printf
            "paused: resume with jig run --resume %s [--guidance \"...\"]\n\
             or reopen the step's session: jig attach %s\n"
            run.Jig_core.Run.id run.Jig_core.Run.id
      | _ -> ())

let attach_run run_id latest step_number =
  let ( let* ) = Result.bind in
  let root = Sys.getcwd () in
  let runs_dir = Filename.concat root "runs" in
  let session_of_step (step : Jig_core.Run.step_record) =
    match step.Jig_core.Run.session_id with
    | Some session -> Ok session
    | None ->
        Error
          (Printf.sprintf
             "step %s recorded no session id (the harness output carried \
              none)"
             step.Jig_core.Run.skill)
  in
  let result =
    let* id =
      match (run_id, latest) with
      | Some _, true -> Error "pass a run id or --latest, not both"
      | Some id, false -> Ok id
      | None, true -> latest_run_id ~runs_dir
      | None, false -> Error "pass a run id or --latest"
    in
    let* run = Jig_core.Store.Filesystem.load ~runs_dir ~id in
    let* config =
      Jig_core.Config.load ~jig_dir:(Filename.concat root ".jig")
    in
    let steps = run.Jig_core.Run.steps in
    let* session_id =
      match step_number with
      | Some number -> (
          match List.nth_opt steps (number - 1) with
          | Some step -> session_of_step step
          | None ->
              Error
                (Printf.sprintf "run %s has %d steps, no step %d" id
                   (List.length steps) number))
      | None -> (
          let newest_with_session =
            List.fold_left
              (fun found (step : Jig_core.Run.step_record) ->
                match step.Jig_core.Run.session_id with
                | Some _ as session -> session
                | None -> found)
              None steps
          in
          match newest_with_session with
          | Some session -> Ok session
          | None ->
              Error
                (Printf.sprintf "no step of run %s recorded a session id" id))
    in
    let workspace = Option.value run.Jig_core.Run.workspace ~default:root in
    let* () =
      if Sys.file_exists workspace then Ok ()
      else
        Error
          (Printf.sprintf "run %s used workspace %s, which no longer exists"
             id workspace)
    in
    let* command =
      Jig_core.Attach.command ~attach:config.Jig_core.Config.attach
        ~session_id
    in
    Ok (command, workspace)
  in
  match result with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok (command, workspace) -> (
      Unix.chdir workspace;
      try Unix.execvp (List.hd command) (Array.of_list command)
      with Unix.Unix_error (error, _, _) ->
        Printf.eprintf "jig: attach: %s: %s\n" (List.hd command)
          (Unix.error_message error);
        exit 1)

let list_runs json_output =
  let runs_dir = Filename.concat (Sys.getcwd ()) "runs" in
  match Jig_core.Store.Filesystem.list_runs ~runs_dir with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok [] when not json_output -> print_endline "no runs yet"
  | Ok runs when json_output ->
      let summaries =
        List.map
          (fun run ->
            `Assoc
              [
                ("id", `String run.Jig_core.Run.id);
                ("workflow", `String run.Jig_core.Run.workflow);
                ( "status",
                  `String (Jig_core.Run.string_of_status run.Jig_core.Run.status)
                );
                ("started_at", `String run.Jig_core.Run.started_at);
              ])
          runs
      in
      print_endline (Yojson.Safe.pretty_to_string (`List summaries))
  | Ok runs ->
      List.iter
        (fun run ->
          Printf.printf "%s  %s  %s  %s\n" run.Jig_core.Run.id
            run.Jig_core.Run.workflow
            (Jig_core.Run.string_of_status run.Jig_core.Run.status)
            run.Jig_core.Run.started_at)
        runs

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

let init_project preset skill_paths =
  match
    Jig_core.Init.scaffold ~root:(Sys.getcwd ()) ~preset ~skill_paths
  with
  | Error message ->
      Printf.eprintf "jig: %s\n" message;
      exit 1
  | Ok written ->
      List.iter (fun path -> Printf.printf "  .jig/%s\n" path) written;
      (match preset with
      | Jig_core.Init.Custom ->
          print_endline
            "scaffolded - edit .jig/config.yaml to point at your harness, \
             then: jig run bugfix --task \"...\""
      | _ ->
          print_endline
            "scaffolded with a harness preset - review .jig/config.yaml \
             (scope the tool allowlist), then: jig run bugfix --task \"...\"")

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

let isolated_flag =
  let doc = "Run in a git worktree named by the run id instead of the current directory." in
  Arg.(value & flag & info [ "isolated" ] ~doc)

let skip_flag =
  let doc =
    "With --resume: record the paused entry as completed by a human (pair \
     with --guidance to tell the next step what was done) and continue from \
     the entry after it."
  in
  Arg.(value & flag & info [ "skip" ] ~doc)

let run_cmd =
  let doc = "Execute a workflow against a task, or resume a paused run." in
  Cmd.v (Cmd.info "run" ~doc)
    Term.(
      const run_workflow $ optional_workflow_arg $ resume_arg $ task_arg
      $ guidance_arg $ skip_flag $ isolated_flag)

let validate_cmd =
  let doc = "Lint a workflow against the schema and the project's skills." in
  Cmd.v (Cmd.info "validate" ~doc)
    Term.(const validate_workflow $ required_workflow_arg)

let json_flag =
  let doc = "Print machine-readable JSON instead of the human summary." in
  Arg.(value & flag & info [ "json" ] ~doc)

let run_id_arg =
  let doc = "Run id to inspect (the runs/<id>.json record)." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"RUN-ID" ~doc)

let latest_flag =
  let doc = "Inspect the newest run." in
  Arg.(value & flag & info [ "latest" ] ~doc)

let status_cmd =
  let doc = "Inspect a run's progress, outcomes, and last handoff." in
  Cmd.v (Cmd.info "status" ~doc)
    Term.(const show_status $ run_id_arg $ latest_flag $ json_flag)

let attach_step_arg =
  let doc =
    "Attach to this step number (1-based). Default: the newest step that \
     recorded a session id."
  in
  Arg.(value & opt (some int) None & info [ "step" ] ~docv:"N" ~doc)

let attach_cmd =
  let doc =
    "Reopen a step's recorded harness session interactively, in the run's \
     workspace."
  in
  Cmd.v (Cmd.info "attach" ~doc)
    Term.(const attach_run $ run_id_arg $ latest_flag $ attach_step_arg)

type listable = Workflows | Runs

let list_what_arg =
  let doc = "What to list: workflows or runs." in
  Arg.(
    required
    & pos 0 (some (enum [ ("workflows", Workflows); ("runs", Runs) ])) None
    & info [] ~docv:"WHAT" ~doc)

let list_cmd =
  let doc = "Discover what is runnable, and what has run, in this repository." in
  Cmd.v (Cmd.info "list" ~doc)
    Term.(
      const (fun what json ->
          match what with
          | Workflows -> list_workflows json
          | Runs -> list_runs json)
      $ list_what_arg $ json_flag)

let harness_preset_arg =
  let doc =
    "Write a known-good harness preset into the scaffolded config: claude, \
     codex, or custom (the commented template, the default)."
  in
  Arg.(
    value
    & opt
        (enum
           [
             ("claude", Jig_core.Init.Claude);
             ("codex", Jig_core.Init.Codex);
             ("custom", Jig_core.Init.Custom);
           ])
        Jig_core.Init.Custom
    & info [ "harness" ] ~docv:"HARNESS" ~doc)

let skill_paths_arg =
  let doc =
    "Directory to add to the config's skill_paths (repeatable, kept in \
     order)."
  in
  Arg.(value & opt_all string [] & info [ "skill-paths" ] ~docv:"DIR" ~doc)

let init_cmd =
  let doc = "Scaffold a starter .jig/ (workflow, skills, config) into this repository." in
  Cmd.v (Cmd.info "init" ~doc)
    Term.(const init_project $ harness_preset_arg $ skill_paths_arg)

let () =
  let doc = "A minimal, agnostic runner for AI-driven development workflows." in
  let info = Cmd.info "jig" ~version:"0.1.0" ~doc in
  exit
    (Cmd.eval
       (Cmd.group info
          [ init_cmd; run_cmd; validate_cmd; status_cmd; attach_cmd; list_cmd ]))
