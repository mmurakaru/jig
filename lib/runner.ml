open Result_syntax

(* The handoff contract is the runner's protocol, so the runner states it -
   skill files carry pure instructions and are passed byte-for-byte. *)
let handoff_protocol =
  "Workspace: the runner owns isolation. Do the work in the current \
   directory; do not create worktrees or otherwise relocate it.\n\n\
   Protocol: end your reply with a fenced handoff block - it is how the run \
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
   must decide. Reference artifacts by path, and omit the artifacts key \
   when there are none; never paste their contents."

(* One line of orientation: which workflow, how far along, what follows.
   The full step list stays out - it bloats every prompt and tempts a step
   into doing later steps' work early. *)
let rec entry_lead_skill = function
  | Workflow.Step step -> Workflow.step_label step
  | Workflow.Retry retry -> (
      match retry.Workflow.retry_steps with
      | first :: _ -> Workflow.step_label first
      | [] -> "retry")
  | Workflow.For_each for_each -> (
      match for_each.Workflow.body with
      | first :: _ -> entry_lead_skill first
      | [] -> "forEach")

(* The item a forEach body step is currently bound to. The key (the item's
   first column) is its identity for humans; the index drives the cursor. *)
type item_context = {
  var : string;
  bindings : (string * string) list;
  item_index : int;
  item_count : int;
}

(* Step-lifecycle events, for callers that render progress as a run
   unfolds. `on_step` still fires on finish; these add the start of a step
   and the moment a forEach resolves its items, each located precisely in
   the plan by position + skill (+ item key inside a forEach). *)
type run_event =
  | Step_started of {
      skill : string;
      position : Run.position;
      item_key : string option;
      (* The effective tier the step will run on; None is the default
         harness. A live view shows it next to the running step. *)
      tier : string option;
    }
  | Items_resolved of { entry_index : int; item_keys : string list }
  (* The running step's captured-output files, the moment its subprocess
     spawns - a live view can tail them while the step executes. *)
  | Step_output of { stdout_path : string; stderr_path : string }
  | Step_finished of Run.step_record

let position_line ~workflow_name ~entries ~entry_index ~skill ~item =
  let item_part =
    match item with
    | Some context ->
        Printf.sprintf ", item %d of %d (%s)" (context.item_index + 1)
          context.item_count
          (Items.key context.bindings)
    | None -> ""
  in
  let continuation =
    match List.nth_opt entries (entry_index + 1) with
    | Some entry -> Printf.sprintf "next: %s." (entry_lead_skill entry)
    | None -> "this is the final step."
  in
  Printf.sprintf "Workflow: %s - step %d of %d (%s)%s; %s" workflow_name
    (entry_index + 1) (List.length entries) skill item_part continuation

(* Sent through attach_headless after an interactive attach ends; the
   session already carries the full protocol from its original envelope. *)
let elicit_handoff_prompt =
  "The interactive session is over. Emit your handoff block for the current \
   state of this step."

let build_prompt ~context ~task ~position ~inputs ~skill_body
    ~previous_handoff ~guidance =
  let context_section =
    match context with
    | Some text when String.trim text <> "" ->
        Printf.sprintf "Context:\n%s\n\n" text
    | _ -> ""
  in
  let inputs_section =
    match inputs with
    | [] -> ""
    | pairs ->
        Printf.sprintf
          "Step inputs (bound by the workflow; treat each value as a \
           literal):\n\
           %s\n\n"
          (String.concat "\n"
             (List.map
                (fun (key, value) -> Printf.sprintf "%s: %s" key value)
                pairs))
  in
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
  Printf.sprintf "%sTask: %s\n\n%s\n\n%s%s%s%s\n\n%s" context_section task
    position inputs_section guidance_section handoff_section skill_body
    handoff_protocol

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
    workflow_dir : string;
    config : Config.t;
    task : string;
    context : string option;
    entries : Workflow.entry list;
    workspace : string;
    on_step : Run.step_record -> unit;
    on_event : run_event -> unit;
  }

  type progress = {
    run : Run.t;
    last_handoff : Handoff.t option;
    guidance : string option;
  }

  (* How an entry (or the whole run) ended: either the run continues to the
     next entry, or it stops in a terminal-or-paused state. *)
  type verdict = Proceed of progress | Stop of Run.status * progress

  let runs_dir engine = Project.runs_dir ~root:engine.root

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

  (* A step that ends without a handoff block still said something; thread
     its final text into the next attempt so a retry does not start blind.
     Tail-truncated: the end of a long reply carries the freshest state. *)
  let degraded_text_limit = 4000

  let degraded_handoff stdout =
    let text = Handoff.agent_text stdout in
    let text =
      let length = String.length text in
      if length <= degraded_text_limit then text
      else
        "[earlier output truncated]\n"
        ^ String.sub text (length - degraded_text_limit) degraded_text_limit
    in
    {
      Handoff.status = Handoff.Fail;
      artifacts = [];
      summary =
        "The previous attempt ended without a handoff block. Its final \
         message:\n" ^ text;
    }

  (* Guidance rides on exactly one step - the first one executed after a
     resume - then is consumed. *)
  let execute_step engine progress ~item (step : Workflow.step) =
    let label = Workflow.step_label step in
    let item_key = Option.map (fun context -> Items.key context.bindings) item in
    let interpolate value =
      match item with
      | None -> value
      | Some context ->
          Workflow.interpolate ~var:context.var ~bindings:context.bindings value
    in
    (* The skill is loaded before the start event so the event can carry
       the effective tier (the skill's frontmatter is one of its sources). *)
    let* loaded_skill =
      match step.Workflow.action with
      | Workflow.Skill_step name ->
          let* skill =
            Skill.load ~jig_dir:engine.jig_dir
              ~skill_paths:engine.config.Config.skill_paths ~name
          in
          Ok (Some skill)
      | Workflow.Command_step _ -> Ok None
    in
    (* Precedence: the workflow step's tier wins over the skill's own
       default; neither set means the default harness. *)
    let tier =
      match (step.Workflow.tier, loaded_skill) with
      | (Some _ as step_tier), _ -> step_tier
      | None, Some skill -> skill.Skill.tier
      | None, None -> None
    in
    engine.on_event
      (Step_started
         { skill = label; position = progress.run.Run.position; item_key; tier });
    let started_at = Run.iso8601 (Unix.gettimeofday ()) in
    let announce_output ~stdout_path ~stderr_path =
      Current.write ~runs_dir:(runs_dir engine)
        ~run_id:progress.run.Run.id
        { Current.skill = label; item_key; tier; stdout_path; stderr_path;
          started_at };
      engine.on_event (Step_output { stdout_path; stderr_path })
    in
    let* outcome, handoff, handoff_error, cost, stdout, stderr, exit_code,
         session_id, tier
        =
      match (step.Workflow.action, loaded_skill) with
      | Workflow.Skill_step name, Some skill ->
          let* command =
            Model_provider_port.resolve ~config:engine.config ~skill:name ~tier
          in
          let inputs =
            List.map (fun (key, value) -> (key, interpolate value))
              step.Workflow.inputs
          in
          let* exec_result =
            Executor_port.execute ~on_spawn:announce_output ~command
              ~cwd:engine.workspace
              ~prompt:
                (build_prompt ~context:engine.context ~task:engine.task
                   ~position:
                     (position_line ~workflow_name:progress.run.Run.workflow
                        ~entries:engine.entries
                        ~entry_index:progress.run.Run.position.Run.entry_index
                        ~skill:label ~item)
                   ~inputs ~skill_body:skill.Skill.body
                   ~previous_handoff:progress.last_handoff
                   ~guidance:progress.guidance)
              ()
          in
          let cost, usage = Metering.parse_cost exec_result.Executor.stdout in
          let* () =
            Metering_port.record ~runs_dir:(runs_dir engine)
              ~event:
                {
                  Metering.run_id = progress.run.Run.id;
                  skill = name;
                  command;
                  tier;
                  cost;
                  usage;
                  recorded_at = Run.iso8601 (Unix.gettimeofday ());
                }
          in
          let outcome, handoff, handoff_error = outcome_of_exec exec_result in
          Ok
            ( outcome,
              handoff,
              handoff_error,
              cost,
              exec_result.Executor.stdout,
              exec_result.Executor.stderr,
              exec_result.Executor.exit_code,
              Metering.parse_session_id exec_result.Executor.stdout,
              tier )
      | Workflow.Skill_step name, None ->
          Error
            (Printf.sprintf "run: skill %s failed to load - unreachable" name)
      | Workflow.Command_step command, _ ->
          (* A command step is deterministic mechanical work: run the shell
             command, let its exit status be the outcome (0 = pass), record
             it at $0 (no model). *)
          let command = interpolate command in
          let* result =
            Result.map_error
              (fun message -> "command step: " ^ message)
              (Subprocess.run ~cwd:engine.workspace ~on_spawn:announce_output
                 ~argv:[ "sh"; "-c"; command ] ())
          in
          let exit_code = result.Subprocess.exit_code in
          let stdout = result.Subprocess.stdout in
          let stderr = result.Subprocess.stderr in
          let cost = Metering.Cost_usd 0.0 in
          let* () =
            Metering_port.record ~runs_dir:(runs_dir engine)
              ~event:
                {
                  Metering.run_id = progress.run.Run.id;
                  skill = label;
                  command = [ "sh"; "-c"; command ];
                  tier = None;
                  cost;
                  usage = None;
                  recorded_at = Run.iso8601 (Unix.gettimeofday ());
                }
          in
          let outcome = if exit_code = 0 then Run.Pass else Run.Fail in
          (* On failure, thread the command's output onward so a retry (or
             a following skill) can act on what went wrong. *)
          let handoff =
            if outcome = Run.Fail then
              let combined = stdout ^ stderr in
              let tail =
                let length = String.length combined in
                if length <= degraded_text_limit then combined
                else
                  "[earlier output truncated]\n"
                  ^ String.sub combined
                      (length - degraded_text_limit)
                      degraded_text_limit
              in
              Some
                {
                  Handoff.status = Handoff.Fail;
                  artifacts = [];
                  summary =
                    Printf.sprintf "Command failed (exit %d): %s\n%s" exit_code
                      command tail;
                }
            else None
          in
          Ok (outcome, handoff, None, cost, stdout, stderr, exit_code, None, None)
    in
    Current.remove ~runs_dir:(runs_dir engine) ~run_id:progress.run.Run.id;
    let step_record =
      {
        Run.skill = label;
        outcome;
        exit_code;
        cost;
        tier;
        stdout;
        stderr;
        handoff;
        handoff_error;
        session_id;
        item_index = Option.map (fun context -> context.item_index) item;
        item_key;
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
        last_handoff =
          (match (handoff, outcome) with
          | Some _, _ -> handoff
          | None, Run.Invalid_handoff -> Some (degraded_handoff stdout)
          | None, _ -> progress.last_handoff);
        guidance = None;
      }
    in
    let* progress, _ = persist engine progress ~status:Run.Running ~finished:false in
    engine.on_step step_record;
    engine.on_event (Step_finished step_record);
    Ok (step_record, progress)

  let stop_for_failure ~on_fail progress =
    match on_fail with
    | Some Workflow.Escalate -> Stop (Run.Paused, progress)
    | Some Workflow.Abort -> Stop (Run.Aborted, progress)
    | None -> Stop (Run.Failed, progress)

  let execute_plain_entry engine progress ~item step =
    let* step_record, progress = execute_step engine progress ~item step in
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

  let execute_iteration engine progress ~item (retry : Workflow.retry) =
    let rec loop progress remaining =
      match remaining with
      | [] -> Ok (Group_done progress)
      | (step : Workflow.step) :: rest -> (
          let* step_record, progress =
            execute_step engine progress ~item step
          in
          match step_record.Run.outcome with
          | Run.Pass ->
              if step.Workflow.until_pass then Ok (Group_done progress)
              else loop progress rest
          | Run.Escalate -> Ok (Interrupted (Run.Paused, progress))
          | Run.Fail | Run.Invalid_handoff ->
              Ok (Iteration_failed progress))
    in
    loop progress retry.Workflow.retry_steps

  let execute_retry_entry engine progress ~item (retry : Workflow.retry) =
    (* A record update, so entry_index and any forEach cursor survive - a
       pause inside a retry inside a forEach must keep all three. *)
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
        let* result = execute_iteration engine progress ~item retry in
        match result with
        | Group_done progress -> Ok (Proceed progress)
        | Interrupted (status, progress) -> Ok (Stop (status, progress))
        | Iteration_failed progress ->
            attempt
              (update_iterations progress (iterations_used + 1))
              (iterations_used + 1)
    in
    attempt progress progress.run.Run.position.Run.iterations_used

  let set_position progress position =
    { progress with run = { progress.run with Run.position } }

  let set_entry_index progress entry_index =
    set_position progress
      { progress.run.Run.position with Run.entry_index }

  (* One forEach entry: the body runs once per item, in order. The item
     snapshot and cursor live in the persisted position, so a resume
     re-enters exactly the item and body entry that paused; the items file
     is read once, when the entry first starts. *)
  let execute_for_each_entry engine progress (for_each : Workflow.for_each) =
    (* A resume re-enters with a snapshot and a seeded handoff mid-item;
       a fresh entry has neither. Only a fresh entry isolates its first
       item from the step before the forEach. *)
    let fresh_entry = Option.is_none progress.run.Run.position.Run.for_each in
    let* state =
      match progress.run.Run.position.Run.for_each with
      | Some state -> Ok state
      | None ->
          let path =
            Filename.concat engine.workflow_dir for_each.Workflow.items_file
          in
          let* items = Items.load ~path in
          let* () =
            Items.check_columns ~path
              ~required:(Workflow.for_each_columns for_each)
              items
          in
          Ok { Run.item_index = 0; body_index = 0; items }
    in
    let items = state.Run.items in
    engine.on_event
      (Items_resolved
         {
           entry_index = progress.run.Run.position.Run.entry_index;
           item_keys = List.map Items.key items;
         });
    let item_count = List.length items in
    let body = for_each.Workflow.body in
    let body_count = List.length body in
    let set_cursor progress item_index body_index =
      set_position progress
        {
          progress.run.Run.position with
          Run.for_each = Some { Run.item_index; body_index; items };
        }
    in
    let reset_iterations progress =
      set_position progress
        { progress.run.Run.position with Run.iterations_used = 0 }
    in
    (* Each item is an independent unit of the fan-out: it must not inherit
       the previous item's (or the pre-forEach step's) handoff. Threading
       within an item - step-to-step and retry - is untouched. *)
    let clear_handoff progress = { progress with last_handoff = None } in
    let progress = if fresh_entry then clear_handoff progress else progress in
    let rec loop progress item_index body_index =
      if item_index >= item_count then Ok (Proceed progress)
      else if body_index >= body_count then
        loop (clear_handoff progress) (item_index + 1) 0
      else
        let progress = set_cursor progress item_index body_index in
        let bindings = List.nth items item_index in
        let item =
          Some
            { var = for_each.Workflow.var; bindings; item_index; item_count }
        in
        let* verdict =
          match List.nth body body_index with
          | Workflow.Step step ->
              execute_plain_entry engine progress ~item step
          | Workflow.Retry retry ->
              execute_retry_entry engine progress ~item retry
          | Workflow.For_each _ ->
              Error "run: forEach bodies cannot contain forEach"
        in
        match verdict with
        | Proceed progress ->
            loop (reset_iterations progress) item_index (body_index + 1)
        | Stop (status, progress) -> Ok (Stop (status, progress))
    in
    loop progress state.Run.item_index state.Run.body_index

  let execute_entries engine progress =
    let entry_count = List.length engine.entries in
    let rec loop progress entry_index =
      if entry_index >= entry_count then Ok (Run.Completed, progress)
      else
        let entry = List.nth engine.entries entry_index in
        let progress = set_entry_index progress entry_index in
        let* verdict =
          match entry with
          | Workflow.Step step ->
              execute_plain_entry engine progress ~item:None step
          | Workflow.Retry retry ->
              execute_retry_entry engine progress ~item:None retry
          | Workflow.For_each for_each ->
              execute_for_each_entry engine progress for_each
        in
        match verdict with
        | Proceed progress ->
            (* Entry complete: reset the retry counter and clear any forEach
               cursor, so finished runs carry no snapshot. *)
            let progress =
              set_position progress
                { Run.entry_index; iterations_used = 0; for_each = None }
            in
            loop progress (entry_index + 1)
        | Stop (status, progress) -> Ok (status, progress)
    in
    loop progress progress.run.Run.position.Run.entry_index

  (* Best-effort announcement that the run stopped executing; a notify
     failure never affects the run's outcome. *)
  let notify engine run =
    match engine.config.Config.notify with
    | [] -> ()
    | template ->
        let argv =
          List.map
            (fun part ->
              part
              |> Attach.substitute ~placeholder:"{run_id}" ~value:run.Run.id
              |> Attach.substitute ~placeholder:"{status}"
                   ~value:(Run.string_of_status run.Run.status))
            template
        in
        ignore (Subprocess.run ~cwd:engine.root ~argv ())

  (* Terminal runs clean their worktree up; paused runs keep it so a resume
     finds the work in progress exactly where the agent left it. *)
  let finish engine progress status =
    (* A step that died in infrastructure never reached its own removal. *)
    Current.remove ~runs_dir:(runs_dir engine) ~run_id:progress.run.Run.id;
    let finished = status <> Run.Running && status <> Run.Paused in
    let* progress, path = persist engine progress ~status ~finished in
    let* () =
      match (finished, progress.run.Run.workspace) with
      | true, Some workspace_path ->
          Workspace.remove ~root:engine.root ~path:workspace_path
      | _ -> Ok ()
    in
    notify engine progress.run;
    Ok (progress.run, path)

  let load_project ~root ~workflow_name =
    let jig_dir = Filename.concat root ".jig" in
    let* workflow_path, workflow_dir =
      Project.resolve_workflow ~jig_dir ~name:workflow_name
    in
    let* workflow = Workflow.load ~path:workflow_path in
    let* config = Config.load ~jig_dir in
    let* () =
      Validate.workflow ~workflow_dir ~jig_dir
        ~skill_paths:config.Config.skill_paths workflow
    in
    Ok (jig_dir, workflow_dir, workflow, config)

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

  let execute_run ?(on_step = fun _ -> ()) ?(on_event = fun _ -> ())
      ?run_id ~root ~workflow_name ~task
      ~isolated () =
    let* jig_dir, workflow_dir, workflow, config = load_project ~root ~workflow_name in
    let started = Unix.gettimeofday () in
    (* A detaching caller pre-issues the id so it can print it and name
       the log file before forking. *)
    let run_id =
      match run_id with
      | Some id -> id
      | None ->
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
        workflow_dir;
        config;
        task;
        context = workflow.Workflow.context;
        entries = workflow.Workflow.entries;
        workspace = Option.value workspace ~default:root;
        on_step;
        on_event;
      }
    in
    let run =
      {
        Run.id = run_id;
        workflow = workflow.Workflow.name;
        task;
        status = Run.Running;
        error = None;
        position = { Run.entry_index = 0; iterations_used = 0; for_each = None };
        workspace;
        steps = [];
        started_at = Run.iso8601 started;
        finished_at = None;
      }
    in
    drive engine { run; last_handoff = None; guidance = None }

  let retry_gate_skill (retry : Workflow.retry) =
    let gate =
      List.find_opt
        (fun step -> step.Workflow.until_pass)
        retry.Workflow.retry_steps
    in
    match (gate, retry.Workflow.retry_steps) with
    | Some step, _ -> Workflow.step_label step
    | None, first :: _ -> Workflow.step_label first
    | None, [] -> "retry"

  (* Inside a forEach the human-completed unit is the current body entry of
     the current item; elsewhere it is the whole entry (a retry group is
     skipped as a group, gate included). *)
  let skip_target_skill ~entries ~(position : Run.position) =
    match List.nth_opt entries position.Run.entry_index with
    | Some (Workflow.Step step) -> Ok (Workflow.step_label step)
    | Some (Workflow.Retry retry) -> Ok (retry_gate_skill retry)
    | Some (Workflow.For_each for_each) ->
        Ok
          (match position.Run.for_each with
          | Some state -> (
              match
                List.nth_opt for_each.Workflow.body state.Run.body_index
              with
              | Some (Workflow.Step step) -> Workflow.step_label step
              | Some (Workflow.Retry retry) -> retry_gate_skill retry
              | Some (Workflow.For_each _) | None ->
                  entry_lead_skill (Workflow.For_each for_each))
          | None -> entry_lead_skill (Workflow.For_each for_each))
    | None ->
        Error "run: position is past the workflow's entries; nothing to skip"

  (* The item the position is currently bound to, for step provenance. *)
  let position_item (position : Run.position) =
    match position.Run.for_each with
    | Some state -> (
        match List.nth_opt state.Run.items state.Run.item_index with
        | Some bindings ->
            (Some state.Run.item_index, Some (Items.key bindings))
        | None -> (Some state.Run.item_index, None))
    | None -> (None, None)

  (* Advance past the body entry a human just completed: the next body
     entry of the same item, else the first body entry of the next item,
     else the next workflow entry. One helper for --skip and attach, so the
     re-entry paths cannot drift. *)
  let advance_after_human_pass ~entries (position : Run.position) =
    let next_entry =
      {
        Run.entry_index = position.Run.entry_index + 1;
        iterations_used = 0;
        for_each = None;
      }
    in
    match
      (List.nth_opt entries position.Run.entry_index, position.Run.for_each)
    with
    | Some (Workflow.For_each for_each), Some state ->
        let body_count = List.length for_each.Workflow.body in
        let item_count = List.length state.Run.items in
        if state.Run.body_index + 1 < body_count then
          {
            position with
            Run.iterations_used = 0;
            for_each =
              Some { state with Run.body_index = state.Run.body_index + 1 };
          }
        else if state.Run.item_index + 1 < item_count then
          {
            position with
            Run.iterations_used = 0;
            for_each =
              Some
                {
                  state with
                  Run.item_index = state.Run.item_index + 1;
                  body_index = 0;
                };
          }
        else next_entry
    | _ -> next_entry

  (* A human can complete the paused entry themselves: the pass is recorded
     with no cost - the absent harness spend is the audit trail - and a
     human-authored handoff threads onward. *)
  let human_skip ~entries ~guidance run =
    let* skill = skip_target_skill ~entries ~position:run.Run.position in
    let item_index, item_key = position_item run.Run.position in
    let now = Run.iso8601 (Unix.gettimeofday ()) in
    let summary =
      match guidance with
      | Some text -> "A human completed this step manually.\n" ^ text
      | None -> "A human completed this step manually."
    in
    let record =
      {
        Run.skill;
        outcome = Run.Pass;
        exit_code = 0;
        cost = Metering.Unknown_cost;
        tier = None;
        stdout = "";
        stderr = "";
        handoff =
          Some { Handoff.status = Handoff.Pass; artifacts = []; summary };
        handoff_error = None;
        session_id = None;
        item_index;
        item_key;
        started_at = now;
        finished_at = now;
      }
    in
    Ok
      ( record,
        {
          run with
          Run.steps = run.Run.steps @ [ record ];
          position = advance_after_human_pass ~entries run.Run.position;
        } )

  let resume_run ?(on_step = fun _ -> ()) ?(on_event = fun _ -> ())
      ?(skip = false) ~root ~run_id
      ~guidance () =
    let runs_directory = Project.runs_dir ~root in
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
    let* jig_dir, workflow_dir, workflow, config =
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
        workflow_dir;
        config;
        task = existing.Run.task;
        context = workflow.Workflow.context;
        entries = workflow.Workflow.entries;
        workspace = Option.value existing.Run.workspace ~default:root;
        on_step;
        on_event;
      }
    in
    let last_handoff = Run.last_handoff existing in
    let run = { existing with Run.status = Run.Running; finished_at = None } in
    if skip then
      let* record, run =
        human_skip ~entries:workflow.Workflow.entries ~guidance run
      in
      on_step record;
      drive engine { run; last_handoff = record.Run.handoff; guidance = None }
    else drive engine { run; last_handoff; guidance }

  (* After an interactive attach, the same session is resumed headlessly and
     asked for its handoff; that result completes the paused entry as if the
     step had just executed. An unparseable reply changes nothing - the run
     stays paused on disk and the caller reports the error. *)
  let continue_attached ?(on_step = fun _ -> ()) ?(on_event = fun _ -> ())
      ~root ~run_id
      ~(exec_result : Executor.exec_result) () =
    let runs_directory = Project.runs_dir ~root in
    let* existing = Store_port.load ~runs_dir:runs_directory ~id:run_id in
    let* () =
      match existing.Run.status with
      | Run.Paused | Run.Running -> Ok ()
      | status ->
          Error
            (Printf.sprintf
               "run %s is %s; only paused runs can continue from an attach"
               run_id
               (Run.string_of_status status))
    in
    let* jig_dir, workflow_dir, workflow, config =
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
    let outcome, handoff, handoff_error = outcome_of_exec exec_result in
    let* () =
      match outcome with
      | Run.Invalid_handoff ->
          Error
            (Printf.sprintf
               "attach: the session's reply carried no parseable handoff \
                (%s); the run is unchanged - re-attach, or resume with \
                --guidance or --skip"
               (Option.value handoff_error ~default:"missing block"))
      | _ -> Ok ()
    in
    let entry_index = existing.Run.position.Run.entry_index in
    let* skill =
      skip_target_skill ~entries:workflow.Workflow.entries
        ~position:existing.Run.position
    in
    let engine =
      {
        root;
        jig_dir;
        workflow_dir;
        config;
        task = existing.Run.task;
        context = workflow.Workflow.context;
        entries = workflow.Workflow.entries;
        workspace = Option.value existing.Run.workspace ~default:root;
        on_step;
        on_event;
      }
    in
    let cost, usage = Metering.parse_cost exec_result.Executor.stdout in
    let* () =
      Metering_port.record ~runs_dir:(runs_dir engine)
        ~event:
          {
            Metering.run_id = existing.Run.id;
            skill;
            command = config.Config.attach_headless;
            tier = None;
            cost;
            usage;
            recorded_at = Run.iso8601 (Unix.gettimeofday ());
          }
    in
    let now = Run.iso8601 (Unix.gettimeofday ()) in
    let item_index, item_key = position_item existing.Run.position in
    let record =
      {
        Run.skill;
        outcome;
        exit_code = exec_result.Executor.exit_code;
        cost;
        tier = None;
        stdout = exec_result.Executor.stdout;
        stderr = exec_result.Executor.stderr;
        handoff;
        handoff_error;
        session_id = Metering.parse_session_id exec_result.Executor.stdout;
        item_index;
        item_key;
        started_at = now;
        finished_at = now;
      }
    in
    let run =
      {
        existing with
        Run.status = Run.Running;
        finished_at = None;
        steps = existing.Run.steps @ [ record ];
      }
    in
    on_step record;
    match outcome with
    | Run.Pass ->
        let run =
          {
            run with
            Run.position =
              advance_after_human_pass ~entries:workflow.Workflow.entries
                existing.Run.position;
          }
        in
        drive engine { run; last_handoff = handoff; guidance = None }
    | Run.Escalate ->
        finish engine { run; last_handoff = handoff; guidance = None } Run.Paused
    | Run.Fail ->
        (* Inside a forEach the failing unit is the current body entry. *)
        let failing_entry =
          match
            ( List.nth_opt workflow.Workflow.entries entry_index,
              existing.Run.position.Run.for_each )
          with
          | Some (Workflow.For_each for_each), Some state ->
              List.nth_opt for_each.Workflow.body state.Run.body_index
          | entry, _ -> entry
        in
        let status =
          match failing_entry with
          | Some (Workflow.Step step) -> (
              match step.Workflow.on_fail with
              | Some Workflow.Escalate -> Run.Paused
              | Some Workflow.Abort -> Run.Aborted
              | None -> Run.Failed)
          (* A retry entry escalated to get here; a failed elicitation
             still needs the human, so the run stays theirs. *)
          | _ -> Run.Paused
        in
        finish engine { run; last_handoff = handoff; guidance = None } status
    | Run.Invalid_handoff -> Error "attach: unreachable - rejected above"
end

module Default =
  Make (Executor.Local) (Model_provider.Default) (Metering.Jsonl)
    (Store.Filesystem)
