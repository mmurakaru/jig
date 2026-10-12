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

(* Shown only when the repo opted in (the guide file exists); it invites
   the write side of the loop - injection is the read side. *)
let field_guide_protocol =
  "Field guide: if this step taught you something durable and non-obvious \
   about this repository (a build quirk, a hidden dependency, a command \
   that must run first), append it to " ^ Field_guide.relative_path
  ^ " as one short factual line. Do not restate what the code or docs \
     already say."

(* The end of a long output carries the freshest state. *)
let tail_text ~limit text =
  let length = String.length text in
  if length <= limit then text
  else "[earlier output truncated]\n" ^ String.sub text (length - limit) limit

let interruption_text_limit = 4000

let session_id_random_state = lazy (Random.State.make_self_init ())

(* Version 4 UUID shape: claude --session-id rejects anything else. *)
let new_session_id () =
  let state = Lazy.force session_id_random_state in
  let bytes = Bytes.init 16 (fun _ -> Char.chr (Random.State.int state 256)) in
  let set index transform =
    Bytes.set bytes index
      (Char.chr (transform (Char.code (Bytes.get bytes index))))
  in
  set 6 (fun byte -> (byte land 0x0f) lor 0x40);
  set 8 (fun byte -> (byte land 0x3f) lor 0x80);
  let hex =
    String.concat ""
      (List.init 16 (fun index ->
           Printf.sprintf "%02x" (Char.code (Bytes.get bytes index))))
  in
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub hex 0 8) (String.sub hex 8 4)
    (String.sub hex 12 4) (String.sub hex 16 4) (String.sub hex 20 12)

(* The capture file outlives a killed jig, so it holds the partial output. *)
let partial_output_of (interrupted : Run.attempt) =
  match interrupted.Run.stdout_path with
  | None -> None
  | Some path -> (
      match File.read path with
      | Ok text when String.trim text <> "" ->
          Some (tail_text ~limit:interruption_text_limit text)
      | Ok _ | Error _ -> None)

let interruption_section ~(interrupted : Run.attempt) ~probe_now
    ~partial_output =
  let labelled label = function
    | Some text -> Printf.sprintf "%s:\n%s\n" label text
    | None -> ""
  in
  Printf.sprintf
    "Interrupted attempt:\n\
     An earlier attempt of this step (%s) stopped before it finished. It \
     may have done part of the work: changed files, made commits, pushed a \
     branch, or opened a pull request. Inspect the workspace and any \
     external state first, keep what is done, do not repeat side effects \
     that already happened, then finish the step.\n\
     %s%s%s\n"
    interrupted.Run.attempt_id
    (labelled "Probe before that attempt" interrupted.Run.probe_before)
    (labelled "Probe now" probe_now)
    (labelled "Its partial output" partial_output)

(* The reopened session already holds its output, prompt and protocol. *)
let continue_session_prompt ~(interrupted : Run.attempt) ~probe_now ~guidance =
  let guidance_section =
    match guidance with
    | Some text -> Printf.sprintf "Human guidance:\n%s\n\n" text
    | None -> ""
  in
  Printf.sprintf
    "%s%sFinish the step, then end your reply with the handoff block."
    (interruption_section ~interrupted ~probe_now ~partial_output:None)
    guidance_section

let build_prompt ?interruption ~context ~field_guide ~task ~position ~inputs
    ~skill_body ~previous_handoff ~guidance () =
  let context_section =
    match context with
    | Some text when String.trim text <> "" ->
        Printf.sprintf "Context:\n%s\n\n" text
    | _ -> ""
  in
  let field_guide_section =
    match field_guide with
    | Some text when String.trim text <> "" ->
        Printf.sprintf
          "Field guide (repo knowledge accumulated by earlier runs):\n%s\n\n"
          text
    | _ -> ""
  in
  let protocol =
    match field_guide with
    | Some _ -> handoff_protocol ^ "\n\n" ^ field_guide_protocol
    | None -> handoff_protocol
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
  let interruption_section = Option.value interruption ~default:"" in
  Printf.sprintf "%s%sTask: %s\n\n%s\n\n%s%s%s%s%s\n\n%s" context_section
    field_guide_section task position inputs_section guidance_section
    interruption_section handoff_section skill_body protocol

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
    (* Loaded from the ledger at most once per drive; None until first use. *)
    metered_attempt_ids : (string, unit) Hashtbl.t option ref;
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

  (* Held from the first save to the last, so one process drives a run. *)
  let with_run_lock ~runs_dir ~run_id action =
    let* run_lock = Store_port.acquire_run_lock ~runs_dir ~id:run_id in
    Fun.protect
      ~finally:(fun () -> Store_port.release_run_lock run_lock)
      action

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
    let text =
      tail_text ~limit:degraded_text_limit (Handoff.agent_text stdout)
    in
    {
      Handoff.status = Handoff.Fail;
      artifacts = [];
      summary =
        "The previous attempt ended without a handoff block. Its final \
         message:\n" ^ text;
    }

  (* Keyed by attempt id, so recording it again on a resume is a no-op. *)
  let attempt_metering_event ~run_id ~cost ~usage ~interrupted
      (attempt : Run.attempt) =
    {
      Metering.run_id;
      skill = attempt.Run.skill;
      command = attempt.Run.command;
      tier = attempt.Run.tier;
      cost;
      usage;
      step_key = Some attempt.Run.step_key;
      attempt_id = Some attempt.Run.attempt_id;
      interrupted;
      recorded_at = Run.iso8601 (Unix.gettimeofday ());
    }

  let finished_metering_event ~run_id (step_record : Run.step_record) attempt =
    attempt_metering_event ~run_id ~cost:step_record.Run.cost
      ~usage:(snd (Metering.parse_cost step_record.Run.stdout))
      ~interrupted:false attempt

  (* The spend of an interrupted attempt is unknown, never zero. *)
  let interrupted_metering_event ~run_id attempt =
    attempt_metering_event ~run_id ~cost:Metering.Unknown_cost ~usage:None
      ~interrupted:true attempt

  let metered_attempt_ids engine ~run_id =
    match !(engine.metered_attempt_ids) with
    | Some attempt_ids -> Ok attempt_ids
    | None ->
        let* recorded =
          Metering_port.recorded_attempt_ids ~runs_dir:(runs_dir engine)
            ~run_id
        in
        let attempt_ids = Hashtbl.create 64 in
        List.iter
          (fun attempt_id -> Hashtbl.replace attempt_ids attempt_id ())
          recorded;
        engine.metered_attempt_ids := Some attempt_ids;
        Ok attempt_ids

  (* At most one event per attempt id, so a resume can re-record safely. *)
  let record_attempt_metering engine (event : Metering.event) =
    match event.Metering.attempt_id with
    | None -> Metering_port.record ~runs_dir:(runs_dir engine) ~event
    | Some attempt_id ->
        let* attempt_ids =
          metered_attempt_ids engine ~run_id:event.Metering.run_id
        in
        if Hashtbl.mem attempt_ids attempt_id then Ok ()
        else
          let* () = Metering_port.record ~runs_dir:(runs_dir engine) ~event in
          Hashtbl.replace attempt_ids attempt_id ();
          Ok ()

  (* A crash between the step save and its metering loses the event. *)
  let record_missing_metering engine (run : Run.t) =
    let record event = record_attempt_metering engine event in
    let* () =
      List.fold_left
        (fun previous (step_record : Run.step_record) ->
          let* () = previous in
          match step_record.Run.attempt with
          | Some attempt ->
              record
                (finished_metering_event ~run_id:run.Run.id step_record
                   attempt)
          | None -> Ok ())
        (Ok ()) run.Run.steps
    in
    List.fold_left
      (fun previous attempt ->
        let* () = previous in
        record (interrupted_metering_event ~run_id:run.Run.id attempt))
      (Ok ()) run.Run.interrupted_attempts

  let with_attempt_in_flight progress (attempt : Run.attempt) =
    {
      progress with
      run =
        {
          progress.run with
          Run.attempts_in_flight =
            Run.Step_key_map.add attempt.Run.step_key attempt
              progress.run.Run.attempts_in_flight;
        };
    }

  let without_attempt_in_flight progress (attempt : Run.attempt) =
    {
      progress with
      run =
        {
          progress.run with
          Run.attempts_in_flight =
            Run.Step_key_map.remove attempt.Run.step_key
              progress.run.Run.attempts_in_flight;
        };
    }

  let with_interrupted_attempt progress (updated : Run.attempt) =
    {
      progress with
      run =
        {
          progress.run with
          Run.interrupted_attempts =
            List.map
              (fun (attempt : Run.attempt) ->
                if attempt.Run.attempt_id = updated.Run.attempt_id then updated
                else attempt)
              progress.run.Run.interrupted_attempts;
        };
    }

  (* Best effort: a failing probe records its failure, never fails the step. *)
  let run_probe engine =
    match engine.config.Config.probe with
    | [] -> None
    | argv -> (
        match Subprocess.run ~cwd:engine.workspace ~argv () with
        | Ok { Subprocess.exit_code = 0; stdout; _ } -> Some stdout
        | Ok { Subprocess.exit_code; stdout; stderr } ->
            Some
              (Printf.sprintf "probe exited %d\n%s%s" exit_code stdout stderr)
        | Error message -> Some message)

  type recovery =
    | Fresh_attempt
    | Recovery_prompt of Run.attempt
    | Continue_session of { cut_off : Run.attempt; session_id : string }

  (* continue: reopens sessions of the default harness only, not of a tier. *)
  let ran_on_default_harness engine (attempt : Run.attempt) =
    match attempt.Run.tier with
    | None -> true
    | Some tier -> not (List.mem_assoc tier engine.config.Config.tiers)

  (* Escalate reaches here only after a human resumed, so it recovers. *)
  let recovery_plan engine (interrupted : Run.attempt option) =
    match interrupted with
    | None -> Fresh_attempt
    | Some cut_off -> (
        match cut_off.Run.on_interrupt with
        | Skill.Rerun -> Fresh_attempt
        | Skill.Recover | Skill.Escalate -> (
            match (engine.config.Config.continue_session, cut_off.Run.session_id)
            with
            | _ :: _, Some session_id when ran_on_default_harness engine cut_off
              ->
                Continue_session { cut_off; session_id }
            | _ -> Recovery_prompt cut_off))

  (* Guidance rides on exactly one step - the first one executed after a
     resume - then is consumed. *)
  let execute_step engine progress ~item ~step_index (step : Workflow.step) =
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
    let step_key =
      Run.step_key_of_position ~run_id:progress.run.Run.id
        ~position:progress.run.Run.position ~step_index
    in
    let attempt_id = Run.next_attempt_id progress.run ~step_key in
    let environment =
      [ ("JIG_STEP_KEY", step_key); ("JIG_ATTEMPT_ID", attempt_id) ]
    in
    let probe_before = run_probe engine in
    let interrupted = Run.unresolved_interrupted_attempt progress.run ~step_key in
    (* The workspace now is the state the cut-off attempt left behind. *)
    let progress =
      match interrupted with
      | Some cut_off when Option.is_none cut_off.Run.probe_after ->
          with_interrupted_attempt progress
            { cut_off with Run.probe_after = probe_before }
      | Some _ | None -> progress
    in
    let recovery = recovery_plan engine interrupted in
    let on_interrupt =
      match loaded_skill with
      | Some skill -> skill.Skill.on_interrupt
      | None -> Skill.Rerun
    in
    let* command, session_id, start_process =
      match (step.Workflow.action, loaded_skill, recovery) with
      | Workflow.Skill_step _, Some _, Continue_session { cut_off; session_id }
        ->
          let command =
            engine.config.Config.wrapper
            @ Attach.substitute_session_id ~session_id
                engine.config.Config.continue_session
          in
          let prompt =
            continue_session_prompt ~interrupted:cut_off ~probe_now:probe_before
              ~guidance:progress.guidance
          in
          Ok
            ( command,
              Some session_id,
              fun ~on_spawn ->
                Executor_port.execute ~on_spawn ~environment ~command
                  ~cwd:engine.workspace ~prompt () )
      | Workflow.Skill_step name, Some skill, _ ->
          let* resolved_command =
            Model_provider_port.resolve ~config:engine.config ~skill:name ~tier
          in
          let session_id =
            if
              List.exists
                (Attach.mentions_placeholder ~placeholder:"{session_id}")
                resolved_command
            then Some (new_session_id ())
            else None
          in
          let command =
            match session_id with
            | Some session_id ->
                Attach.substitute_session_id ~session_id resolved_command
            | None -> resolved_command
          in
          let inputs =
            List.map (fun (key, value) -> (key, interpolate value))
              step.Workflow.inputs
          in
          let interruption =
            match recovery with
            | Recovery_prompt cut_off ->
                Some
                  (interruption_section ~interrupted:cut_off
                     ~probe_now:probe_before
                     ~partial_output:(partial_output_of cut_off))
            | Fresh_attempt | Continue_session _ -> None
          in
          let prompt =
            build_prompt ?interruption ~context:engine.context
              ~field_guide:(Field_guide.load ~workspace:engine.workspace)
              ~task:engine.task
              ~position:
                (position_line ~workflow_name:progress.run.Run.workflow
                   ~entries:engine.entries
                   ~entry_index:progress.run.Run.position.Run.entry_index
                   ~skill:label ~item)
              ~inputs ~skill_body:skill.Skill.body
              ~previous_handoff:progress.last_handoff
              ~guidance:progress.guidance ()
          in
          Ok
            ( command,
              session_id,
              fun ~on_spawn ->
                Executor_port.execute ~on_spawn ~environment ~command
                  ~cwd:engine.workspace ~prompt () )
      | Workflow.Skill_step name, None, _ ->
          Error
            (Printf.sprintf "run: skill %s failed to load - unreachable" name)
      | Workflow.Command_step command_template, _, _ ->
          (* A command step is deterministic mechanical work: run the shell
             command, let its exit status be the outcome (0 = pass), record
             it at $0 (no model). *)
          let argv = [ "sh"; "-c"; interpolate command_template ] in
          Ok
            ( argv,
              None,
              fun ~on_spawn ->
                Result.map
                  (fun result ->
                    {
                      Executor.exit_code = result.Subprocess.exit_code;
                      stdout = result.Subprocess.stdout;
                      stderr = result.Subprocess.stderr;
                    })
                  (Result.map_error
                     (fun message -> "command step: " ^ message)
                     (Subprocess.run ~cwd:engine.workspace ~on_spawn
                        ~environment ~argv ())) )
    in
    let attempt =
      {
        Run.step_key;
        attempt_id;
        skill = label;
        tier;
        command;
        started_at;
        child_pid = None;
        session_id;
        on_interrupt;
        probe_before;
        probe_after = None;
        stdout_path = None;
      }
    in
    let* progress, _ =
      persist engine
        (with_attempt_in_flight progress attempt)
        ~status:Run.Running ~finished:false
    in
    let spawned_attempt = ref attempt in
    let on_spawn ~child_pid ~stdout_path ~stderr_path =
      spawned_attempt :=
        {
          attempt with
          Run.child_pid = Some child_pid;
          stdout_path = Some stdout_path;
        };
      (* Best effort: the outcome save that follows reports a failing store. *)
      ignore
        (persist engine
           (with_attempt_in_flight progress !spawned_attempt)
           ~status:Run.Running ~finished:false);
      Current.write ~runs_dir:(runs_dir engine)
        ~run_id:progress.run.Run.id
        { Current.skill = label; item_key; tier; stdout_path; stderr_path;
          started_at };
      engine.on_event (Step_output { stdout_path; stderr_path })
    in
    let process_result = start_process ~on_spawn in
    Current.remove ~runs_dir:(runs_dir engine) ~run_id:progress.run.Run.id;
    let progress = without_attempt_in_flight progress attempt in
    let* exec_result =
      match process_result with
      | Ok exec_result -> Ok exec_result
      | Error message when Option.is_none !spawned_attempt.Run.child_pid ->
          (* The process never started, so the attempt leaves no trace. *)
          ignore (persist engine progress ~status:Run.Running ~finished:false);
          Error message
      | Error message ->
          (* The harness ran, so its unknown spend stays on record. *)
          let progress =
            {
              progress with
              run =
                {
                  progress.run with
                  Run.interrupted_attempts =
                    progress.run.Run.interrupted_attempts @ [ !spawned_attempt ];
                };
            }
          in
          let* _ = persist engine progress ~status:Run.Running ~finished:false in
          let* () =
            record_attempt_metering engine
              (interrupted_metering_event ~run_id:progress.run.Run.id
                 !spawned_attempt)
          in
          Error message
    in
    (* The capture file is gone once the process is reaped. *)
    let finished_attempt =
      {
        !spawned_attempt with
        Run.probe_after = run_probe engine;
        stdout_path = None;
      }
    in
    let stdout = exec_result.Executor.stdout in
    let stderr = exec_result.Executor.stderr in
    let exit_code = exec_result.Executor.exit_code in
    let outcome, handoff, handoff_error, cost, session_id =
      match step.Workflow.action with
      | Workflow.Skill_step _ ->
          let outcome, handoff, handoff_error = outcome_of_exec exec_result in
          ( outcome,
            handoff,
            handoff_error,
            fst (Metering.parse_cost stdout),
            match Metering.parse_session_id stdout with
            | Some _ as reported -> reported
            | None -> finished_attempt.Run.session_id )
      | Workflow.Command_step command_template ->
          let outcome = if exit_code = 0 then Run.Pass else Run.Fail in
          (* On failure, thread the command's output onward so a retry (or
             a following skill) can act on what went wrong. *)
          let handoff =
            if outcome = Run.Fail then
              let tail =
                tail_text ~limit:degraded_text_limit (stdout ^ stderr)
              in
              Some
                {
                  Handoff.status = Handoff.Fail;
                  artifacts = [];
                  summary =
                    Printf.sprintf "Command failed (exit %d): %s\n%s" exit_code
                      (interpolate command_template) tail;
                }
            else None
          in
          (outcome, handoff, None, Metering.Cost_usd 0.0, None)
    in
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
        attempt = Some finished_attempt;
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
    let* () =
      record_attempt_metering engine
        (finished_metering_event ~run_id:progress.run.Run.id step_record
           finished_attempt)
    in
    engine.on_step step_record;
    engine.on_event (Step_finished step_record);
    Ok (step_record, progress)

  let stop_for_failure ~on_fail progress =
    match on_fail with
    | Some Workflow.Escalate -> Stop (Run.Paused, progress)
    | Some Workflow.Abort -> Stop (Run.Aborted, progress)
    | None -> Stop (Run.Failed, progress)

  let execute_plain_entry engine progress ~item step =
    let* step_record, progress =
      execute_step engine progress ~item ~step_index:0 step
    in
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
    let rec loop progress step_index remaining =
      match remaining with
      | [] -> Ok (Group_done progress)
      | (step : Workflow.step) :: rest -> (
          let* step_record, progress =
            execute_step engine progress ~item ~step_index step
          in
          match step_record.Run.outcome with
          | Run.Pass ->
              if step.Workflow.until_pass then Ok (Group_done progress)
              else loop progress (step_index + 1) rest
          | Run.Escalate -> Ok (Interrupted (Run.Paused, progress))
          | Run.Fail | Run.Invalid_handoff ->
              Ok (Iteration_failed progress))
    in
    loop progress 0 retry.Workflow.retry_steps

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
    let* () = record_missing_metering engine progress.run in
    match execute_entries engine progress with
    | Ok (status, progress) -> finish engine progress status
    | Error message ->
        (* The last save holds every step this drive executed. *)
        let last_saved_run =
          match
            Store_port.load ~runs_dir:(runs_dir engine) ~id:progress.run.Run.id
          with
          | Ok run -> run
          | Error _ -> progress.run
        in
        let progress =
          {
            progress with
            run = { last_saved_run with Run.error = Some message };
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
    with_run_lock ~runs_dir:(Project.runs_dir ~root) ~run_id @@ fun () ->
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
        (* A new run id has no events in the ledger yet. *)
        metered_attempt_ids = ref (Some (Hashtbl.create 64));
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
        attempts_in_flight = Run.Step_key_map.empty;
        interrupted_attempts = [];
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
        attempt = None;
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

  let pause_for_interruption engine progress (attempts : Run.attempt list) =
    let probe_now = run_probe engine in
    let progress =
      List.fold_left
        (fun progress (attempt : Run.attempt) ->
          with_interrupted_attempt progress
            { attempt with Run.probe_after = probe_now })
        progress attempts
    in
    let message =
      Printf.sprintf
        "interrupted: %s stopped before it finished and its skill sets \
         on_interrupt: escalate - check the workspace, then resume, skip, or \
         attach"
        (String.concat ", "
           (List.map
              (fun (attempt : Run.attempt) ->
                Printf.sprintf "%s (%s)" attempt.Run.skill attempt.Run.attempt_id)
              attempts))
    in
    let progress =
      { progress with run = { progress.run with Run.error = Some message } }
    in
    let* () = record_missing_metering engine progress.run in
    finish engine progress Run.Paused

  let resume_run ?(on_step = fun _ -> ()) ?(on_event = fun _ -> ())
      ?(skip = false) ~root ~run_id
      ~guidance () =
    let runs_directory = Project.runs_dir ~root in
    with_run_lock ~runs_dir:runs_directory ~run_id @@ fun () ->
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
        metered_attempt_ids = ref None;
      }
    in
    let last_handoff = Run.last_handoff existing in
    let run =
      Run.mark_attempts_interrupted
        {
          existing with
          Run.status = Run.Running;
          finished_at = None;
          error = None;
        }
    in
    let attempts_to_escalate =
      List.filter
        (fun (_, (attempt : Run.attempt)) ->
          attempt.Run.on_interrupt = Skill.Escalate)
        (Run.Step_key_map.bindings existing.Run.attempts_in_flight)
    in
    (* Guidance or a skip is already the human's decision. *)
    if attempts_to_escalate <> [] && (not skip) && Option.is_none guidance
    then
      pause_for_interruption engine
        { run; last_handoff; guidance = None }
        (List.map snd attempts_to_escalate)
    else if skip then
      let* record, run =
        human_skip ~entries:workflow.Workflow.entries ~guidance run
      in
      on_step record;
      drive engine { run; last_handoff = record.Run.handoff; guidance = None }
    else drive engine { run; last_handoff; guidance }

  (* After an interactive attach, the same session is resumed headlessly and
     asked for its handoff; that result completes the paused entry as if the
     step had just executed. An unparseable reply changes nothing - the run
     stays paused on disk and the caller reports the error. The chat runs
     inside [collect_handoff], under the run lock. *)
  let continue_attached ?(on_step = fun _ -> ()) ?(on_event = fun _ -> ())
      ~root ~run_id
      ~(collect_handoff : unit -> (Executor.exec_result, string) result) () =
    let runs_directory = Project.runs_dir ~root in
    with_run_lock ~runs_dir:runs_directory ~run_id @@ fun () ->
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
    let* exec_result = collect_handoff () in
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
        metered_attempt_ids = ref None;
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
            step_key = None;
            attempt_id = None;
            interrupted = false;
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
        attempt = None;
        started_at = now;
        finished_at = now;
      }
    in
    let run =
      Run.mark_attempts_interrupted
        {
          existing with
          Run.status = Run.Running;
          finished_at = None;
          steps = existing.Run.steps @ [ record ];
        }
    in
    (* [drive] records missing metering itself; a stop here must too. *)
    let finish_with_metering progress status =
      let* finished = finish engine progress status in
      let* () = record_missing_metering engine progress.run in
      Ok finished
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
        finish_with_metering
          { run; last_handoff = handoff; guidance = None }
          Run.Paused
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
        finish_with_metering
          { run; last_handoff = handoff; guidance = None }
          status
    | Run.Invalid_handoff -> Error "attach: unreachable - rejected above"
end

module Default =
  Make (Executor.Local) (Model_provider.Default) (Metering.Jsonl)
    (Store.Filesystem)
