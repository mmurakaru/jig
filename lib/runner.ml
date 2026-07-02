open Result_syntax

let build_prompt ~task ~skill_body =
  Printf.sprintf "Task: %s\n\n%s" task skill_body

module Make
    (Executor_port : Executor.S)
    (Model_provider_port : Model_provider.S)
    (Metering_port : Metering.S)
    (Store_port : Store.S) =
struct
  let execute_step ~config ~run_id ~task ~jig_dir (step : Workflow.step) =
    let* skill_body = Skill.load ~jig_dir ~name:step.Workflow.skill in
    let* command =
      Model_provider_port.resolve ~config ~skill:step.Workflow.skill
    in
    let started_at = Run.iso8601 (Unix.gettimeofday ()) in
    let* exec_result =
      Executor_port.execute ~command ~prompt:(build_prompt ~task ~skill_body)
    in
    Metering_port.record ~run_id ~skill:step.Workflow.skill ~exec_result;
    let outcome =
      if exec_result.Executor.exit_code = 0 then Run.Pass else Run.Fail
    in
    Ok
      {
        Run.skill = step.Workflow.skill;
        outcome;
        exit_code = exec_result.Executor.exit_code;
        stdout = exec_result.Executor.stdout;
        stderr = exec_result.Executor.stderr;
        started_at;
        finished_at = Run.iso8601 (Unix.gettimeofday ());
      }

  (* Always returns the step records that actually executed, so a run record
     can be persisted even when a step errors out mid-run. *)
  let execute_steps ~config ~run_id ~task ~jig_dir steps =
    let rec loop completed remaining =
      match remaining with
      | [] -> (List.rev completed, None)
      | step :: rest -> (
          match execute_step ~config ~run_id ~task ~jig_dir step with
          | Error message -> (List.rev completed, Some message)
          | Ok step_record -> (
              match step_record.Run.outcome with
              | Run.Fail -> (List.rev (step_record :: completed), None)
              | Run.Pass -> loop (step_record :: completed) rest))
    in
    loop [] steps

  let execute_run ~root ~workflow_name ~task =
    let jig_dir = Filename.concat root ".jig" in
    let workflow_path =
      Filename.concat
        (Filename.concat jig_dir "workflows")
        (workflow_name ^ ".yaml")
    in
    let* workflow = Workflow.load ~path:workflow_path in
    let* config = Config.load ~jig_dir in
    let started = Unix.gettimeofday () in
    let run_id =
      Run.make_id ~workflow:workflow.Workflow.name ~time:started
        ~pid:(Unix.getpid ())
    in
    let steps, step_error =
      execute_steps ~config ~run_id ~task ~jig_dir workflow.Workflow.steps
    in
    let status =
      if
        step_error = None
        && List.for_all (fun step -> step.Run.outcome = Run.Pass) steps
      then Run.Completed
      else Run.Failed
    in
    let run =
      {
        Run.id = run_id;
        workflow = workflow.Workflow.name;
        task;
        status;
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
