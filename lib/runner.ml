open Result_syntax

(* The handoff contract is the runner's protocol, so the runner states it -
   skill files carry pure instructions and are passed byte-for-byte. *)
let handoff_protocol =
  "Protocol: end your reply with a fenced handoff block - it is how the run \
   continues, and a reply without one fails the step.\n\n\
   ```handoff\n\
   status: <pass | fail | escalate>\n\
   artifacts:\n\
  \  - paths/you/produced\n\
   summary: |\n\
  \  Written for the next agent: what happened, what to look at, what to\n\
  \  do differently.\n\
   ```\n\n\
   status: pass only when this step's goal is verifiably met; fail when it \
   is not (a retry reads your summary to do better); escalate when a human \
   must decide. Reference artifacts by path; never paste their contents."

let build_prompt ~task ~skill_body ~previous_handoff ~guidance =
  let guidance_section =
    match guidance with
    | Some text -> Printf.sprintf "Human guidance:\n%s\n\n" text
    | None -> ""
  in
  let handoff_section =
    match previous_handoff with
    | Some handoff ->
        Printf.sprintf "Previous handoff:\n%s\n\n" (Handoff.render handoff)
    | None -> ""
  in
  Printf.sprintf "Task: %s\n\n%s%s%s\n\n%s" task guidance_section
    handoff_section skill_body handoff_protocol

module Make
    (Executor_port : Executor.S)
    (Model_provider_port : Model_provider.S)
    (Metering_port : Metering.S)
    (Store_port : Store.S) =
struct
  (* Threaded through execution; the run record is re-saved after every step
     so killing the process mid-run leaves a resumable record. *)
  type engine = {
    root : string;
    jig_dir : string;
    config : Config.t;
    task : string;
    entries : Workflow.entry list;
    workspace : string;
    on_step : Run.step_record -> unit;
  }

  type progress = {
    run : Run.t;
    last_handoff : Handoff.t option;
    guidance : string option;
  }

  (* How an entry (or the whole run) ended: either the run continues to the
     next entry, or it stops in a terminal-or-paused state. *)
  type verdict = Proceed of progress | Stop of Run.status * progress

  let runs_dir engine = Filename.concat engine.root "runs"

  let persist engine progress ~status ~finished =
    let run =
      {
        progress.run with
        Run.status;
        finished_at =
          (if finished then Some (Run.iso8601 (Unix.gettimeofday ()))
           else None);
      }
    in
    let* path = Store_port.save ~runs_dir:(runs_dir engine) run in
    Ok ({ progress with run }, path)

  let outcome_of_exec exec_result =
    if exec_result.Executor.exit_code <> 0 then (Run.Fail, None, None)
    else
      match Handoff.parse exec_result.Executor.stdout with
      | Ok handoff -> (
          match handoff.Handoff.status with
          | Handoff.Pass -> (Run.Pass, Some handoff, None)
          | Handoff.Fail -> (Run.Fail, Some handoff, None)
          | Handoff.Escalate -> (Run.Escalate, Some handoff, None))
      | Error message -> (Run.Invalid_handoff, None, Some message)

  (* Guidance rides on exactly one step - the first one executed after a
     resume - then is consumed. *)
  let execute_step engine progress (step : Workflow.step) =
    let* skill_body =
      Skill.load ~jig_dir:engine.jig_dir
        ~skill_paths:engine.config.Config.skill_paths
        ~name:step.Workflow.skill
    in
    let* command =
      Model_provider_port.resolve ~config:engine.config
        ~skill:step.Workflow.skill
    in
    let started_at = Run.iso8601 (Unix.gettimeofday ()) in
    let* exec_result =
      Executor_port.execute ~command ~cwd:engine.workspace
        ~prompt:
          (build_prompt ~task:engine.task ~skill_body
             ~previous_handoff:progress.last_handoff
             ~guidance:progress.guidance)
    in
    let cost, usage = Metering.parse_cost exec_result.Executor.stdout in
    let* () =
      Metering_port.record ~runs_dir:(runs_dir engine)
        ~event:
          {
            Metering.run_id = progress.run.Run.id;
            skill = step.Workflow.skill;
            command;
            cost;
            usage;
            recorded_at = Run.iso8601 (Unix.gettimeofday ());
          }
    in
    let outcome, handoff, handoff_error = outcome_of_exec exec_result in
    let step_record =
      {
        Run.skill = step.Workflow.skill;
        outcome;
        exit_code = exec_result.Executor.exit_code;
        cost;
        stdout = exec_result.Executor.stdout;
        stderr = exec_result.Executor.stderr;
        handoff;
        handoff_error;
        started_at;
        finished_at = Run.iso8601 (Unix.gettimeofday ());
      }
    in
    let progress =
      {
        run =
          {
            progress.run with
            Run.steps = progress.run.Run.steps @ [ step_record ];
          };
        last_handoff = (match handoff with Some _ -> handoff | None -> progress.last_handoff);
        guidance = None;
      }
    in
    let* progress, _ = persist engine progress ~status:Run.Running ~finished:false in
    engine.on_step step_record;
    Ok (step_record, progress)

  let stop_for_failure ~on_fail progress =
    match on_fail with
    | Some Workflow.Escalate -> Stop (Run.Paused, progress)
    | Some Workflow.Abort -> Stop (Run.Aborted, progress)
    | None -> Stop (Run.Failed, progress)

  let execute_plain_entry engine progress step =
    let* step_record, progress = execute_step engine progress step in
    match step_record.Run.outcome with
    | Run.Pass -> Ok (Proceed progress)
    | Run.Escalate -> Ok (Stop (Run.Paused, progress))
    | Run.Invalid_handoff -> Ok (Stop (Run.Failed, progress))
    | Run.Fail -> Ok (stop_for_failure ~on_fail:step.Workflow.on_fail progress)

  (* One retry iteration: run the group in order. Pass on an until-step
     completes the group early; any fail (or invalid handoff) fails the
     iteration, and its handoff threads into the next attempt - that is the
     self-correction loop. Escalate always pauses the whole run. *)
  type iteration_result =
    | Group_done of progress
    | Iteration_failed of progress
    | Interrupted of Run.status * progress

  let execute_iteration engine progress (retry : Workflow.retry) =
    let rec loop progress remaining =
      match remaining with
      | [] -> Ok (Group_done progress)
      | (step : Workflow.step) :: rest -> (
          let* step_record, progress = execute_step engine progress step in
          match step_record.Run.outcome with
          | Run.Pass ->
              if step.Workflow.until_pass then Ok (Group_done progress)
              else loop progress rest
          | Run.Escalate -> Ok (Interrupted (Run.Paused, progress))
          | Run.Fail | Run.Invalid_handoff ->
              Ok (Iteration_failed progress))
    in
    loop progress retry.Workflow.retry_steps

  let execute_retry_entry engine progress (retry : Workflow.retry) =
    let update_iterations progress iterations_used =
      {
        progress with
        run =
          {
            progress.run with
            Run.position =
              { progress.run.Run.position with Run.iterations_used };
          };
      }
    in
    let rec attempt progress iterations_used =
      if iterations_used >= retry.Workflow.max_iterations then
        match retry.Workflow.on_exhausted with
        | Workflow.Escalate -> Ok (Stop (Run.Paused, progress))
        | Workflow.Abort -> Ok (Stop (Run.Aborted, progress))
      else
        let* result = execute_iteration engine progress retry in
        match result with
        | Group_done progress -> Ok (Proceed progress)
        | Interrupted (status, progress) -> Ok (Stop (status, progress))
        | Iteration_failed progress ->
            attempt
              (update_iterations progress (iterations_used + 1))
              (iterations_used + 1)
    in
    attempt progress progress.run.Run.position.Run.iterations_used

  let set_entry_index progress entry_index =
    {
      progress with
      run =
        {
          progress.run with
          Run.position = { Run.entry_index; iterations_used = progress.run.Run.position.Run.iterations_used };
        };
    }

  let execute_entries engine progress =
    let entry_count = List.length engine.entries in
    let rec loop progress entry_index =
      if entry_index >= entry_count then Ok (Run.Completed, progress)
      else
        let entry = List.nth engine.entries entry_index in
        let progress = set_entry_index progress entry_index in
        let* verdict =
          match entry with
          | Workflow.Step step -> execute_plain_entry engine progress step
          | Workflow.Retry retry -> execute_retry_entry engine progress retry
        in
        match verdict with
        | Proceed progress ->
            let progress =
              {
                progress with
                run =
                  {
                    progress.run with
                    Run.position =
                      { Run.entry_index; iterations_used = 0 };
                  };
              }
            in
            loop progress (entry_index + 1)
        | Stop (status, progress) -> Ok (status, progress)
    in
    loop progress progress.run.Run.position.Run.entry_index

  (* Terminal runs clean their worktree up; paused runs keep it so a resume
     finds the work in progress exactly where the agent left it. *)
  let finish engine progress status =
    let finished = status <> Run.Running && status <> Run.Paused in
    let* progress, path = persist engine progress ~status ~finished in
    let* () =
      match (finished, progress.run.Run.workspace) with
      | true, Some workspace_path ->
          Workspace.remove ~root:engine.root ~path:workspace_path
      | _ -> Ok ()
    in
    Ok (progress.run, path)

  let load_project ~root ~workflow_name =
    let jig_dir = Filename.concat root ".jig" in
    let workflow_path =
      Filename.concat
        (Filename.concat jig_dir "workflows")
        (workflow_name ^ ".yaml")
    in
    let* workflow = Workflow.load ~path:workflow_path in
    let* config = Config.load ~jig_dir in
    let* () =
      Validate.workflow ~jig_dir ~skill_paths:config.Config.skill_paths
        workflow
    in
    Ok (jig_dir, workflow, config)

  (* An infrastructure error (unreadable skill, spawn failure, store failure)
     still persists what executed, then propagates as an Error. *)
  let drive engine progress =
    (* Persist before the first step so status/list see the in-flight run;
       a resume likewise flips its stored status back to Running here. *)
    let* progress, _ =
      persist engine progress ~status:Run.Running ~finished:false
    in
    match execute_entries engine progress with
    | Ok (status, progress) -> finish engine progress status
    | Error message ->
        let progress =
          {
            progress with
            run = { progress.run with Run.error = Some message };
          }
        in
        (match finish engine progress Run.Failed with
        | Ok _ | Error _ -> ());
        Error message

  let execute_run ?(on_step = fun _ -> ()) ~root ~workflow_name ~task
      ~isolated () =
    let* jig_dir, workflow, config = load_project ~root ~workflow_name in
    let started = Unix.gettimeofday () in
    let run_id =
      Run.make_id ~workflow:workflow.Workflow.name ~time:started
        ~pid:(Unix.getpid ())
    in
    let* workspace =
      if isolated then
        let* path = Workspace.create ~root ~run_id in
        Ok (Some path)
      else Ok None
    in
    let engine =
      {
        root;
        jig_dir;
        config;
        task;
        entries = workflow.Workflow.entries;
        workspace = Option.value workspace ~default:root;
        on_step;
      }
    in
    let run =
      {
        Run.id = run_id;
        workflow = workflow.Workflow.name;
        task;
        status = Run.Running;
        error = None;
        position = { Run.entry_index = 0; iterations_used = 0 };
        workspace;
        steps = [];
        started_at = Run.iso8601 started;
        finished_at = None;
      }
    in
    drive engine { run; last_handoff = None; guidance = None }

  let resume_run ?(on_step = fun _ -> ()) ~root ~run_id ~guidance () =
    let runs_directory = Filename.concat root "runs" in
    let* existing = Store_port.load ~runs_dir:runs_directory ~id:run_id in
    let* () =
      match existing.Run.status with
      | Run.Paused | Run.Running -> Ok ()
      | status ->
          Error
            (Printf.sprintf
               "run %s is %s; only paused (or interrupted running) runs can \
                be resumed"
               run_id
               (Run.string_of_status status))
    in
    let* jig_dir, workflow, config =
      load_project ~root ~workflow_name:existing.Run.workflow
    in
    let* () =
      match existing.Run.workspace with
      | Some path when not (Sys.file_exists path) ->
          Error
            (Printf.sprintf
               "run %s used isolated workspace %s, which no longer exists"
               run_id path)
      | _ -> Ok ()
    in
    let engine =
      {
        root;
        jig_dir;
        config;
        task = existing.Run.task;
        entries = workflow.Workflow.entries;
        workspace = Option.value existing.Run.workspace ~default:root;
        on_step;
      }
    in
    let last_handoff = Run.last_handoff existing in
    let run = { existing with Run.status = Run.Running; finished_at = None } in
    drive engine { run; last_handoff; guidance }
end

module Default =
  Make (Executor.Local) (Model_provider.Default) (Metering.Jsonl)
    (Store.Filesystem)
