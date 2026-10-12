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
  | Items_resolved of {
      entry_index : int;
      item_keys : string list;
      (* Above 1, several items run at once and each ends on its own. *)
      concurrency : int;
    }
  (* The running step's captured-output files, the moment its subprocess
     spawns - a live view can tail them while the step executes. *)
  | Step_output of {
      stdout_path : string;
      stderr_path : string;
      item_index : int option;
    }
  | Step_finished of Run.step_record
  (* Completed when the item's body passed, else the status it stopped with. *)
  | Item_finished of { entry_index : int; item_index : int; status : Run.status }

(* A worker's pipe and how many steps it reported, so reports send only new ones. *)
type worker_link = {
  worker_output : Unix.file_descr;
  mutable reported_step_count : int;
}

(* Only the driver saves, meters, and renders; a worker reports to it. *)
type worker_message =
  | Worker_progress of {
      position : Run.position;
      attempts_in_flight : Run.attempt list;
      new_steps : Run.step_record list;
      (* All of the item's, since a recovery updates them in place. *)
      interrupted_attempts : Run.attempt list;
    }
  | Worker_metering of Metering.event
  | Worker_event of run_event
  | Worker_step of Run.step_record
  | Worker_pointer of Current.t option
  | Worker_item_passed
  | Worker_item_stopped of {
      status : Run.status;
      position : Run.position;
      paused_on_attempt : string option;
    }
  | Worker_item_error of string

let send_to_driver link (message : worker_message) =
  Worker_channel.send_worker_message link.worker_output message

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

let session_id_random_state = ref (lazy (Random.State.make_self_init ()))

(* Each forked item worker reseeds, so sibling workers never share ids. *)
let reseed_session_ids () =
  session_id_random_state := lazy (Random.State.make_self_init ())

(* Version 4 UUID shape: claude --session-id rejects anything else. *)
let new_session_id () =
  let state = Lazy.force !session_id_random_state in
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

(** A hung notify hook must not block the end of a run. *)
let notify_time_limit_seconds = 30.0

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
    (* Set in a parallel forEach item worker, which reports to the driver. *)
    worker_link : worker_link option;
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
        paused_on_attempt =
          (if status = Run.Paused then
             Run.last_step_attempt_id progress.run.Run.steps
           else progress.run.Run.paused_on_attempt);
        finished_at =
          (if finished then Some (Run.iso8601 (Unix.gettimeofday ()))
           else None);
      }
    in
    let* path = Store_port.save ~runs_dir:(runs_dir engine) run in
    Ok ({ progress with run }, path)

  (* In a worker, the save is a report the driver merges into the record. *)
  let save_step_progress engine progress =
    match engine.worker_link with
    | None ->
        let* progress, _ =
          persist engine progress ~status:Run.Running ~finished:false
        in
        Ok progress
    | Some link ->
        let run = progress.run in
        let after count list = List.filteri (fun index _ -> index >= count) list in
        send_to_driver link
          (Worker_progress
             {
               position = run.Run.position;
               attempts_in_flight =
                 List.map snd (Run.Step_key_map.bindings run.Run.attempts_in_flight);
               new_steps = after link.reported_step_count run.Run.steps;
               interrupted_attempts = run.Run.interrupted_attempts;
             });
        link.reported_step_count <- List.length run.Run.steps;
        Ok progress

  let set_step_pointer engine ~run_id pointer =
    match engine.worker_link with
    | None -> Current.write ~runs_dir:(runs_dir engine) ~run_id [ pointer ]
    | Some link -> send_to_driver link (Worker_pointer (Some pointer))

  let clear_step_pointer engine ~run_id =
    match engine.worker_link with
    | None -> Current.remove ~runs_dir:(runs_dir engine) ~run_id
    | Some link -> send_to_driver link (Worker_pointer None)

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
    match (engine.worker_link, event.Metering.attempt_id) with
    | Some link, _ ->
        send_to_driver link (Worker_metering event);
        Ok ()
    | None, None -> Metering_port.record ~runs_dir:(runs_dir engine) ~event
    | None, Some attempt_id ->
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

  let handoff_after_step progress (step_record : Run.step_record) =
    match (step_record.Run.handoff, step_record.Run.outcome) with
    | Some _, _ -> step_record.Run.handoff
    | None, Run.Invalid_handoff ->
        Some (degraded_handoff step_record.Run.stdout)
    | None, _ -> progress.last_handoff

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
  let run_step_process engine progress ~item (step : Workflow.step) =
    let label = Workflow.step_label step in
    let item_key = Option.map (fun context -> Items.key context.bindings) item in
    let item_index = Option.map (fun context -> context.item_index) item in
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
        ~position:progress.run.Run.position
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
        child_host = None;
        child_start_time = None;
        session_id;
        on_interrupt;
        probe_before;
        probe_after = None;
        stdout_path = None;
      }
    in
    let* progress =
      save_step_progress engine (with_attempt_in_flight progress attempt)
    in
    let spawned_attempt = ref attempt in
    let on_spawn ~child_pid ~stdout_path ~stderr_path =
      (* Best effort: the outcome save that follows reports a failing store. *)
      let save_spawned_attempt spawned =
        spawned_attempt := spawned;
        ignore
          (save_step_progress engine (with_attempt_in_flight progress spawned))
      in
      let spawned =
        {
          attempt with
          Run.child_pid = Some child_pid;
          child_host = Some (Unix.gethostname ());
          stdout_path = Some stdout_path;
        }
      in
      save_spawned_attempt spawned;
      set_step_pointer engine ~run_id:progress.run.Run.id
        { Current.skill = label; item_key; item_index; tier; stdout_path;
          stderr_path; started_at };
      engine.on_event (Step_output { stdout_path; stderr_path; item_index });
      (* Last: the ps read must not delay the pid record or the live pointer. *)
      save_spawned_attempt
        {
          spawned with
          Run.child_start_time = Process_identity.start_time child_pid;
        }
    in
    let process_result = start_process ~on_spawn in
    clear_step_pointer engine ~run_id:progress.run.Run.id;
    let progress = without_attempt_in_flight progress attempt in
    let* exec_result =
      match process_result with
      | Ok exec_result -> Ok exec_result
      | Error message when Option.is_none !spawned_attempt.Run.child_pid ->
          (* The process never started, so the attempt leaves no trace. *)
          ignore (save_step_progress engine progress);
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
          let* _ = save_step_progress engine progress in
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
        item_index;
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
        last_handoff = handoff_after_step progress step_record;
        guidance = None;
      }
    in
    let* progress = save_step_progress engine progress in
    let* () =
      record_attempt_metering engine
        (finished_metering_event ~run_id:progress.run.Run.id step_record
           finished_attempt)
    in
    engine.on_step step_record;
    engine.on_event (Step_finished step_record);
    Ok (step_record, progress)

  (* A crash after a step's result save leaves its outcome unrouted. *)
  let execute_step engine progress ~item step =
    let step_key =
      Run.step_key_of_position ~run_id:progress.run.Run.id
        ~position:progress.run.Run.position
    in
    match Run.unrouted_finished_step progress.run ~step_key with
    | Some step_record ->
        Ok
          ( step_record,
            {
              progress with
              last_handoff = handoff_after_step progress step_record;
            } )
    | None -> run_step_process engine progress ~item step

  let stop_for_failure ~on_fail progress =
    match on_fail with
    | Some Workflow.Escalate -> Stop (Run.Paused, progress)
    | Some Workflow.Abort -> Stop (Run.Aborted, progress)
    | None -> Stop (Run.Failed, progress)

  let execute_plain_entry engine progress ~item step =
    let* step_record, progress =
      execute_step engine progress ~item step
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

  let set_position progress position =
    { progress with run = { progress.run with Run.position } }

  let set_step_index progress step_index =
    set_position progress { progress.run.Run.position with Run.step_index }

  (* A resume skips the steps this iteration already passed. *)
  let execute_iteration engine progress ~item (retry : Workflow.retry) =
    let rec loop progress step_index =
      match List.nth_opt retry.Workflow.retry_steps step_index with
      | None -> Ok (Group_done progress)
      | Some (step : Workflow.step) -> (
          let progress = set_step_index progress step_index in
          let* step_record, progress = execute_step engine progress ~item step in
          match step_record.Run.outcome with
          | Run.Pass ->
              if step.Workflow.until_pass then Ok (Group_done progress)
              else loop progress (step_index + 1)
          | Run.Escalate -> Ok (Interrupted (Run.Paused, progress))
          | Run.Fail | Run.Invalid_handoff ->
              Ok (Iteration_failed progress))
    in
    let start_index = progress.run.Run.position.Run.step_index in
    if start_index >= List.length retry.Workflow.retry_steps then
      Error "run: position is past the retry group's steps; nothing to resume"
    else loop progress start_index

  let execute_retry_entry engine progress ~item (retry : Workflow.retry) =
    (* A record update, so entry_index and any forEach cursor survive - a
       pause inside a retry inside a forEach must keep them all. *)
    let start_next_iteration progress iterations_used =
      set_position progress
        { progress.run.Run.position with Run.iterations_used; step_index = 0 }
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
              (start_next_iteration progress (iterations_used + 1))
              (iterations_used + 1)
    in
    attempt progress progress.run.Run.position.Run.iterations_used

  let set_entry_index progress entry_index =
    set_position progress
      { progress.run.Run.position with Run.entry_index }

  let restart_retry_position progress =
    set_position progress
      { progress.run.Run.position with Run.iterations_used = 0; step_index = 0 }

  (* Each item is an independent unit of the fan-out: it must not inherit
     the previous item's (or the pre-forEach step's) handoff. Threading
     within an item - step-to-step and retry - is untouched. *)
  let clear_handoff progress = { progress with last_handoff = None }

  (* One item's body, from [body_index] to its end or its first stop. *)
  let execute_item engine progress (for_each : Workflow.for_each)
      ~(state : Run.for_each_state) ~item_index ~body_index =
    let items = state.Run.items in
    let body = for_each.Workflow.body in
    let body_count = List.length body in
    let item =
      Some
        {
          var = for_each.Workflow.var;
          bindings = List.nth items item_index;
          item_index;
          item_count = List.length items;
        }
    in
    let set_cursor progress body_index =
      set_position progress
        {
          progress.run.Run.position with
          Run.for_each = Some { state with Run.item_index; body_index };
        }
    in
    let rec loop progress body_index =
      if body_index >= body_count then Ok (Proceed progress)
      else
        let progress = set_cursor progress body_index in
        let* verdict =
          match List.nth body body_index with
          | Workflow.Step step -> execute_plain_entry engine progress ~item step
          | Workflow.Retry retry ->
              execute_retry_entry engine progress ~item retry
          | Workflow.For_each _ ->
              Error "run: forEach bodies cannot contain forEach"
        in
        match verdict with
        | Proceed progress ->
            loop (restart_retry_position progress) (body_index + 1)
        | Stop (status, progress) -> Ok (Stop (status, progress))
    in
    loop progress body_index

  (* Recreated only when the item never ran a step, so no work is lost. *)
  let ensure_item_worktree engine progress (item : Run.item_in_progress) =
    let item_number = item.Run.started_item_index + 1 in
    let never_ran =
      progress.run.Run.steps = [] && progress.run.Run.interrupted_attempts = []
    in
    match item.Run.item_workspace with
    | Some path when Sys.file_exists path -> Ok path
    | Some path when never_ran ->
        let* () =
          Workspace.create_item_worktree ~root:engine.root
            ~run_workspace:engine.workspace ~path
        in
        Ok path
    | Some path ->
        Error
          (Printf.sprintf
             "run: forEach item %d used worktree %s, which no longer exists"
             item_number path)
    | None ->
        Error (Printf.sprintf "run: forEach item %d has no worktree" item_number)

  (* Runs in a forked process and never returns. *)
  let run_item_worker engine progress (for_each : Workflow.for_each) ~state
      ~(item : Run.item_in_progress) ~worker_output =
    let link =
      {
        worker_output;
        reported_step_count = List.length progress.run.Run.steps;
      }
    in
    reseed_session_ids ();
    let worker_engine workspace =
      {
        engine with
        workspace;
        worker_link = Some link;
        on_step = (fun record -> send_to_driver link (Worker_step record));
        on_event = (fun event -> send_to_driver link (Worker_event event));
      }
    in
    (try
       let result =
         let* workspace = ensure_item_worktree engine progress item in
         execute_item (worker_engine workspace) progress for_each ~state
           ~item_index:item.Run.started_item_index
           ~body_index:item.Run.item_body_index
       in
       send_to_driver link
         (match result with
         | Ok (Proceed _) -> Worker_item_passed
         | Ok (Stop (status, stopped)) ->
             Worker_item_stopped
               {
                 status;
                 position = stopped.run.Run.position;
                 paused_on_attempt =
                   (if status = Run.Paused then
                      Run.last_step_attempt_id stopped.run.Run.steps
                    else None);
               }
         | Error message -> Worker_item_error message)
     with exception_raised -> (
       try
         send_to_driver link
           (Worker_item_error
              ("run: forEach item worker raised "
              ^ Printexc.to_string exception_raised))
       with _ -> ()));
    (* _exit: the driver's at_exit handlers and buffers are not the worker's. *)
    Unix._exit 0

  type item_worker = {
    worker_pid : int;
    worker_input : Unix.file_descr;
    worker_reader : Worker_channel.reader;
    worker_item_index : int;
    mutable worker_reported_end : bool;
  }

  (* A failed or aborted item decides the run over a paused one. *)
  let combine_stop_status current status =
    match current with
    | None | Some Run.Paused -> Some status
    | Some _ -> current

  (* The item's cursor as a worker's item position reports it. *)
  let with_item_cursor (position : Run.position) (item : Run.item_in_progress) =
    {
      item with
      Run.item_body_index =
        (match position.Run.for_each with
        | Some cursor -> cursor.Run.body_index
        | None -> item.Run.item_body_index);
      item_iterations_used = position.Run.iterations_used;
      item_step_index = position.Run.step_index;
    }

  (* Forked item workers; the driver alone saves, so saves stay serial. *)
  let execute_parallel_for_each engine progress (for_each : Workflow.for_each)
      (initial_state : Run.for_each_state) =
    let entry_index = progress.run.Run.position.Run.entry_index in
    let run_id = progress.run.Run.id in
    let item_count = List.length initial_state.Run.items in
    let guidance = progress.guidance in
    let master = ref { progress with guidance = None; last_handoff = None } in
    let state = ref initial_state in
    let first_error = ref None in
    let stop_status = ref None in
    let workers = ref [] in
    let pending_resumes = ref initial_state.Run.items_in_progress in
    let pointers = Hashtbl.create 8 in
    let note_error message =
      if Option.is_none !first_error then first_error := Some message
    in
    let sync_state_into_master () =
      master :=
        set_position !master
          { !master.run.Run.position with Run.for_each = Some !state }
    in
    let save () =
      sync_state_into_master ();
      match persist engine !master ~status:Run.Running ~finished:false with
      | Ok (saved, _) -> master := saved
      | Error message -> note_error message
    in
    let update_item item_index change =
      state :=
        {
          !state with
          Run.items_in_progress =
            List.map
              (fun (item : Run.item_in_progress) ->
                if item.Run.started_item_index = item_index then change item
                else item)
              !state.Run.items_in_progress;
        }
    in
    let write_pointers () =
      let runs_directory = runs_dir engine in
      match
        List.sort compare
          (Hashtbl.fold
             (fun index pointer all -> (index, pointer) :: all)
             pointers [])
      with
      | [] -> Current.remove ~runs_dir:runs_directory ~run_id
      | indexed ->
          Current.write ~runs_dir:runs_directory ~run_id
            (List.map snd indexed)
    in
    let step_key_prefix item_index =
      Run.item_step_key_prefix ~run_id ~entry_index ~item_index
    in
    let start_worker (item : Run.item_in_progress) ~resumed =
      let item_index = item.Run.started_item_index in
      let worker_state = { !state with Run.items_in_progress = [] } in
      let lane =
        {
          (Run.item_lane !master.run
             ~step_key_prefix:(step_key_prefix item_index))
          with
          Run.position =
            {
              Run.entry_index;
              iterations_used = item.Run.item_iterations_used;
              step_index = item.Run.item_step_index;
              for_each =
                Some
                  {
                    worker_state with
                    Run.item_index;
                    body_index = item.Run.item_body_index;
                  };
            };
          paused_on_attempt = item.Run.item_paused_on_attempt;
        }
      in
      let worker_progress =
        {
          run = lane;
          last_handoff = (if resumed then Run.last_handoff lane else None);
          guidance = (if resumed then guidance else None);
        }
      in
      match Unix.pipe ~cloexec:true () with
      | exception Unix.Unix_error (error, _, _) ->
          note_error
            ("run: cannot open a forEach worker pipe: "
            ^ Unix.error_message error)
      | worker_input, worker_output -> (
          match
            Process_group.fork_worker_process ~pipe_from_worker:worker_input
          with
          | exception Unix.Unix_error (error, _, _) ->
              Unix.close worker_input;
              Unix.close worker_output;
              note_error
                ("run: cannot fork a forEach worker: "
                ^ Unix.error_message error)
          | 0 ->
              Unix.close worker_input;
              (* Another worker's end of file must not wait on this one. *)
              List.iter
                (fun worker ->
                  try Unix.close worker.worker_input
                  with Unix.Unix_error _ -> ())
                !workers;
              run_item_worker engine worker_progress for_each
                ~state:worker_state ~item ~worker_output
          | worker_pid ->
              Unix.close worker_output;
              workers :=
                !workers
                @ [
                    {
                      worker_pid;
                      worker_input;
                      worker_reader = Worker_channel.create_reader worker_input;
                      worker_item_index = item_index;
                      worker_reported_end = false;
                    };
                  ])
    in
    let start_next_item () =
      let item_index = !state.Run.item_index in
      let path =
        Workspace.item_worktree_path ~root:engine.root ~run_id ~entry_index
          ~item_index
      in
      let item =
        {
          Run.started_item_index = item_index;
          item_body_index = 0;
          item_iterations_used = 0;
          item_step_index = 0;
          item_workspace = Some path;
          item_status = Run.Running;
          item_paused_on_attempt = None;
        }
      in
      state :=
        {
          !state with
          Run.item_index = item_index + 1;
          items_in_progress = !state.Run.items_in_progress @ [ item ];
        };
      (* Saved before the worker creates the worktree, so none is orphaned. *)
      save ();
      start_worker item ~resumed:false
    in
    let resume_item (item : Run.item_in_progress) =
      let running = { item with Run.item_status = Run.Running } in
      update_item item.Run.started_item_index (fun _ -> running);
      save ();
      start_worker running ~resumed:true
    in
    let rec fill_free_slots () =
      if
        Option.is_none !first_error
        && Option.is_none !stop_status
        && List.length !workers < !state.Run.concurrency
      then
        match !pending_resumes with
        | item :: rest ->
            pending_resumes := rest;
            resume_item item;
            fill_free_slots ()
        | [] ->
            if !state.Run.item_index < item_count then (
              start_next_item ();
              fill_free_slots ())
    in
    let handle_message worker (message : worker_message) =
      let item_index = worker.worker_item_index in
      match message with
      | Worker_progress
          { position; attempts_in_flight; new_steps; interrupted_attempts }
        ->
          let prefix = step_key_prefix item_index in
          let run = !master.run in
          let other_attempts =
            Run.Step_key_map.filter
              (fun step_key _ -> not (String.starts_with ~prefix step_key))
              run.Run.attempts_in_flight
          in
          let other_interrupted_attempts =
            List.filter
              (fun (attempt : Run.attempt) ->
                not (String.starts_with ~prefix attempt.Run.step_key))
              run.Run.interrupted_attempts
          in
          master :=
            {
              !master with
              run =
                {
                  run with
                  Run.attempts_in_flight =
                    List.fold_left
                      (fun map (attempt : Run.attempt) ->
                        Run.Step_key_map.add attempt.Run.step_key attempt map)
                      other_attempts attempts_in_flight;
                  steps = run.Run.steps @ new_steps;
                  interrupted_attempts =
                    other_interrupted_attempts @ interrupted_attempts;
                };
            };
          update_item item_index (with_item_cursor position);
          save ()
      | Worker_metering event -> (
          match record_attempt_metering engine event with
          | Ok () -> ()
          | Error message -> note_error message)
      | Worker_event event -> engine.on_event event
      | Worker_step record -> engine.on_step record
      | Worker_pointer pointer ->
          (match pointer with
          | Some pointer -> Hashtbl.replace pointers item_index pointer
          | None -> Hashtbl.remove pointers item_index);
          write_pointers ()
      | Worker_item_passed ->
          worker.worker_reported_end <- true;
          let passed, others =
            List.partition
              (fun (item : Run.item_in_progress) ->
                item.Run.started_item_index = item_index)
              !state.Run.items_in_progress
          in
          state := { !state with Run.items_in_progress = others };
          save ();
          List.iter
            (fun (item : Run.item_in_progress) ->
              match item.Run.item_workspace with
              | Some path -> (
                  match
                    let* () =
                      Workspace.keep_item_commits
                        ~run_workspace:engine.workspace ~path
                    in
                    Workspace.remove ~root:engine.root ~path
                  with
                  | Ok () -> ()
                  | Error message -> note_error message)
              | None -> ())
            passed;
          engine.on_event
            (Item_finished { entry_index; item_index; status = Run.Completed })
      | Worker_item_stopped { status; position; paused_on_attempt } ->
          worker.worker_reported_end <- true;
          update_item item_index (fun item ->
              {
                (with_item_cursor position item) with
                Run.item_status = status;
                item_paused_on_attempt = paused_on_attempt;
              });
          stop_status := combine_stop_status !stop_status status;
          save ();
          engine.on_event (Item_finished { entry_index; item_index; status })
      | Worker_item_error message ->
          worker.worker_reported_end <- true;
          note_error message
    in
    let rec wait_for_exit pid =
      match Unix.waitpid [] pid with
      | _ -> ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_exit pid
      | exception Unix.Unix_error _ -> ()
    in
    let reap worker =
      Unix.close worker.worker_input;
      wait_for_exit worker.worker_pid;
      Process_group.forget_worker_process worker.worker_pid;
      workers :=
        List.filter
          (fun other -> other.worker_pid <> worker.worker_pid)
          !workers;
      if Hashtbl.mem pointers worker.worker_item_index then (
        Hashtbl.remove pointers worker.worker_item_index;
        write_pointers ());
      if not worker.worker_reported_end then
        note_error
          (Printf.sprintf
             "run: forEach item %d: its worker process ended without a result"
             (worker.worker_item_index + 1))
    in
    let rec drain () =
      fill_free_slots ();
      match !workers with
      | [] -> ()
      | running ->
          let ready =
            match
              Unix.select
                (List.map (fun worker -> worker.worker_input) running)
                [] []
                Process_group.child_poll_interval_seconds
            with
            | ready, _, _ -> ready
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> []
          in
          (* A forwarded signal may land on another thread, so poll for it. *)
          Process_group.exit_if_stop_requested ();
          List.iter
            (fun worker ->
              if List.mem worker.worker_input ready then (
                let (messages : worker_message list), closed =
                  Worker_channel.receive_worker_messages worker.worker_reader
                in
                List.iter (handle_message worker) messages;
                if closed then reap worker))
            running;
          drain ()
    in
    drain ();
    sync_state_into_master ();
    match (!first_error, !stop_status) with
    | Some message, _ -> Error message
    | None, Some status -> Ok (Stop (status, !master))
    | None, None -> Ok (Proceed !master)

  (* One forEach entry: the body runs once per item, in order, or with
     concurrency above 1 several items at once. The item snapshot and
     cursors live in the persisted position, so a resume re-enters exactly
     the item and body entry that paused; the items file is read once,
     when the entry first starts. *)
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
          let concurrency = engine.config.Config.concurrency in
          let* () =
            if concurrency > 1 && Option.is_none progress.run.Run.workspace
            then Error (Validate.concurrency_isolation_message concurrency)
            else Ok ()
          in
          Ok
            {
              Run.item_index = 0;
              body_index = 0;
              items;
              concurrency;
              items_in_progress = [];
            }
    in
    let items = state.Run.items in
    engine.on_event
      (Items_resolved
         {
           entry_index = progress.run.Run.position.Run.entry_index;
           item_keys = List.map Items.key items;
           concurrency = state.Run.concurrency;
         });
    if state.Run.concurrency > 1 then
      execute_parallel_for_each engine progress for_each state
    else
      let item_count = List.length items in
      let progress = if fresh_entry then clear_handoff progress else progress in
      let rec loop progress item_index body_index =
        if item_index >= item_count then Ok (Proceed progress)
        else
          let* verdict =
            execute_item engine progress for_each ~state ~item_index
              ~body_index
          in
          match verdict with
          | Proceed progress ->
              loop (clear_handoff progress) (item_index + 1) 0
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
                {
                  Run.entry_index;
                  iterations_used = 0;
                  step_index = 0;
                  for_each = None;
                }
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
        ignore
          (Subprocess.run ~cwd:engine.root
             ~time_limit_seconds:notify_time_limit_seconds ~argv ())

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
    let* () =
      Validate.concurrency_isolation ~concurrency:config.Config.concurrency
        ~isolated workflow
    in
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
        worker_link = None;
      }
    in
    let run =
      {
        Run.id = run_id;
        workflow = workflow.Workflow.name;
        task;
        status = Run.Running;
        error = None;
        position =
          {
            Run.entry_index = 0;
            iterations_used = 0;
            step_index = 0;
            for_each = None;
          };
        workspace;
        steps = [];
        attempts_in_flight = Run.Step_key_map.empty;
        interrupted_attempts = [];
        paused_on_attempt = None;
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
        step_index = 0;
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
            step_index = 0;
            for_each =
              Some { state with Run.body_index = state.Run.body_index + 1 };
          }
        else if state.Run.item_index + 1 < item_count then
          {
            position with
            Run.iterations_used = 0;
            step_index = 0;
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

  (* Several items can be paused at once; one human hand-back cannot pick. *)
  let parallel_items_refuse_one_item_message =
    "run: --skip and attach complete one paused step, but this run is \
     paused inside a forEach that runs items in parallel; resume it with \
     --guidance instead"

  let pause_for_interruption engine progress (attempts : Run.attempt list) =
    (* A parallel forEach item's attempt ran in the item's own worktree. *)
    let probe_after (attempt : Run.attempt) =
      match Run.item_in_progress_of_attempt progress.run attempt with
      | Some { Run.item_workspace = Some workspace; _ } ->
          run_probe { engine with workspace }
      | _ -> run_probe engine
    in
    let progress =
      List.fold_left
        (fun progress (attempt : Run.attempt) ->
          with_interrupted_attempt progress
            { attempt with Run.probe_after = probe_after attempt })
        progress attempts
    in
    let progress =
      {
        progress with
        run =
          {
            progress.run with
            Run.position = Run.pause_running_items progress.run.Run.position;
          };
      }
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
    let* () = Process_identity.refuse_while_attempts_run existing in
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
        worker_link = None;
      }
    in
    let* () =
      if skip && Run.runs_items_in_parallel existing.Run.position then
        Error parallel_items_refuse_one_item_message
      else Ok ()
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
    let* () = Process_identity.refuse_while_attempts_run existing in
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
    let* () =
      if Run.runs_items_in_parallel existing.Run.position then
        Error parallel_items_refuse_one_item_message
      else Ok ()
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
        worker_link = None;
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
