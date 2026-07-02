open Result_syntax

let build_prompt ~task ~skill_body ~previous_handoff =
  match previous_handoff with
  | None -> Printf.sprintf "Task: %s\n\n%s" task skill_body
  | Some handoff ->
      Printf.sprintf "Task: %s\n\nPrevious handoff:\n%s\n\n%s" task
        (Handoff.render handoff) skill_body

module Make
    (Executor_port : Executor.S)
    (Model_provider_port : Model_provider.S)
    (Metering_port : Metering.S)
    (Store_port : Store.S) =
struct
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

  (* Retry blocks and on_fail handlers validate but do not execute yet;
     refusing up front beats silently ignoring declared semantics. *)
  let executable_steps workflow =
    let rec collect entries =
      match entries with
      | [] -> Ok []
      | Workflow.Retry _ :: _ ->
          Error
            "workflow uses a retry block, which is not executable yet \
             (retry lifecycle is not implemented); jig validate accepts it"
      | Workflow.Step step :: rest ->
          if step.Workflow.on_fail <> None then
            Error
              "workflow uses on_fail, which is not executable yet (escalation \
               lifecycle is not implemented); jig validate accepts it"
          else
            let* remaining = collect rest in
            Ok (step :: remaining)
    in
    collect workflow.Workflow.entries

  let execute_step ~config ~run_id ~task ~jig_dir ~previous_handoff
      (step : Workflow.step) =
    let* skill_body = Skill.load ~jig_dir ~name:step.Workflow.skill in
    let* command =
      Model_provider_port.resolve ~config ~skill:step.Workflow.skill
    in
    let started_at = Run.iso8601 (Unix.gettimeofday ()) in
    let* exec_result =
      Executor_port.execute ~command
        ~prompt:(build_prompt ~task ~skill_body ~previous_handoff)
    in
    Metering_port.record ~run_id ~skill:step.Workflow.skill ~exec_result;
    let outcome, handoff, handoff_error = outcome_of_exec exec_result in
    Ok
      {
        Run.skill = step.Workflow.skill;
        outcome;
        exit_code = exec_result.Executor.exit_code;
        stdout = exec_result.Executor.stdout;
        stderr = exec_result.Executor.stderr;
        handoff;
        handoff_error;
        started_at;
        finished_at = Run.iso8601 (Unix.gettimeofday ());
      }

  (* Always returns the step records that actually executed, so a run record
     can be persisted even when a step errors out mid-run. Threads each step's
     handoff into the next step's prompt; only Pass continues the run. *)
  let execute_steps ~config ~run_id ~task ~jig_dir steps =
    let rec loop completed previous_handoff remaining =
      match remaining with
      | [] -> (List.rev completed, None)
      | step :: rest -> (
          match
            execute_step ~config ~run_id ~task ~jig_dir ~previous_handoff step
          with
          | Error message -> (List.rev completed, Some message)
          | Ok step_record -> (
              match step_record.Run.outcome with
              | Run.Pass ->
                  loop (step_record :: completed) step_record.Run.handoff rest
              | Run.Fail | Run.Escalate | Run.Invalid_handoff ->
                  (List.rev (step_record :: completed), None)))
    in
    loop [] None steps

  let status_of ~step_error steps =
    if step_error <> None then Run.Failed
    else
      match List.rev steps with
      | [] -> Run.Failed
      | last :: _ -> (
          match last.Run.outcome with
          | Run.Escalate -> Run.Escalated
          | Run.Pass -> Run.Completed
          | Run.Fail | Run.Invalid_handoff -> Run.Failed)

  let execute_run ~root ~workflow_name ~task =
    let jig_dir = Filename.concat root ".jig" in
    let workflow_path =
      Filename.concat
        (Filename.concat jig_dir "workflows")
        (workflow_name ^ ".yaml")
    in
    let* workflow = Workflow.load ~path:workflow_path in
    let* () = Validate.workflow ~jig_dir workflow in
    let* plain_steps = executable_steps workflow in
    let* config = Config.load ~jig_dir in
    let started = Unix.gettimeofday () in
    let run_id =
      Run.make_id ~workflow:workflow.Workflow.name ~time:started
        ~pid:(Unix.getpid ())
    in
    let steps, step_error =
      execute_steps ~config ~run_id ~task ~jig_dir plain_steps
    in
    let run =
      {
        Run.id = run_id;
        workflow = workflow.Workflow.name;
        task;
        status = status_of ~step_error steps;
        error = step_error;
        steps;
        started_at = Run.iso8601 started;
        finished_at = Run.iso8601 (Unix.gettimeofday ());
      }
    in
    let* path = Store_port.save ~runs_dir:(Filename.concat root "runs") run in
    match step_error with
    | Some message -> Error message
    | None -> Ok (run, path)
end

module Default =
  Make (Executor.Local) (Model_provider.Default) (Metering.Noop)
    (Store.Filesystem)
