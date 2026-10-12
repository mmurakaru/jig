open Result_syntax

type outcome = Pass | Fail | Escalate | Invalid_handoff

(* Saved before the step's process starts, so a crash leaves a record. *)
type attempt = {
  (* The same across resumes and re-runs of the step. *)
  step_key : string;
  attempt_id : string;
  skill : string;
  tier : string option;
  command : string list;
  started_at : string;
  child_pid : int option;
  (* Assigned by jig before the start when the command has {session_id}. *)
  session_id : string option;
  (* What a resume does if this attempt is cut off. *)
  on_interrupt : Skill.on_interrupt;
  probe_before : string option;
  probe_after : string option;
  (* The live capture file; a resume reads the partial output from it. *)
  stdout_path : string option;
}

module Step_key_map = Map.Make (String)

type step_record = {
  skill : string;
  outcome : outcome;
  exit_code : int;
  cost : Metering.cost;
  (* The tier the step asked for (workflow step, else skill default); None
     means the default harness. *)
  tier : string option;
  stdout : string;
  stderr : string;
  handoff : Handoff.t option;
  handoff_error : string option;
  session_id : string option;
  item_index : int option;
  item_key : string option;
  (* None for a human skip, an attach hand-back, or an older record. *)
  attempt : attempt option;
  started_at : string;
  finished_at : string;
}

type status = Running | Paused | Completed | Failed | Aborted

(* A forEach entry in flight. The items are snapshotted here when the entry
   starts so a resume re-binds exactly the item it paused on - the items
   file is never re-read mid-run. Cleared when the entry completes. *)
type for_each_state = {
  item_index : int;
  body_index : int;
  items : (string * string) list list;
}

(* Where the run is in the workflow: the entry being executed, how many
   full iterations a retry has used, the step inside the retry group and,
   for a forEach entry, the item cursor and snapshot. Enough to resume. *)
type position = {
  entry_index : int;
  iterations_used : int;
  (* The step being executed inside a retry group; 0 outside one. *)
  step_index : int;
  for_each : for_each_state option;
}

type t = {
  id : string;
  workflow : string;
  task : string;
  status : status;
  error : string option;
  position : position;
  workspace : string option;
  steps : step_record list;
  (* Keyed by step key, so the record can hold several attempts at once. *)
  attempts_in_flight : attempt Step_key_map.t;
  (* Found in flight by a resume: their process ended with no outcome. *)
  interrupted_attempts : attempt list;
  (* The attempt whose result paused the run, so a resume runs it again. *)
  paused_on_attempt : string option;
  started_at : string;
  finished_at : string option;
}

let iso8601 time =
  let utc = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (utc.Unix.tm_year + 1900)
    (utc.Unix.tm_mon + 1) utc.Unix.tm_mday utc.Unix.tm_hour utc.Unix.tm_min
    utc.Unix.tm_sec

(* Inverse of [iso8601], for durations between recorded timestamps without
   depending on the environment's timezone (civil-date day arithmetic). *)
let epoch_of_iso8601 text =
  let days_from_civil year month day =
    let year = if month <= 2 then year - 1 else year in
    let era = (if year >= 0 then year else year - 399) / 400 in
    let year_of_era = year - (era * 400) in
    let day_of_year =
      ((153 * (if month > 2 then month - 3 else month + 9)) + 2) / 5 + day - 1
    in
    let day_of_era =
      (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100)
      + day_of_year
    in
    (era * 146097) + day_of_era - 719468
  in
  match
    Scanf.sscanf text "%4d-%2d-%2dT%2d:%2d:%2dZ"
      (fun year month day hour minute second ->
        float_of_int
          ((((days_from_civil year month day * 24) + hour) * 60 + minute) * 60
          + second))
  with
  | seconds -> Some seconds
  | exception _ -> None

(* pid separates concurrent jig processes; the sequence separates runs
   started by one process within the same second. *)
let sequence = ref 0

let make_id ~workflow ~time ~pid =
  incr sequence;
  let utc = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02d-%s-%02d%02d%02d-%d-%d"
    (utc.Unix.tm_year + 1900) (utc.Unix.tm_mon + 1) utc.Unix.tm_mday workflow
    utc.Unix.tm_hour utc.Unix.tm_min utc.Unix.tm_sec pid !sequence

(* Every coordinate of the step's place, so the key survives a resume. *)
let step_key_of_position ~run_id ~(position : position) =
  let for_each_part =
    match position.for_each with
    | Some state -> Printf.sprintf "/i%d/b%d" state.item_index state.body_index
    | None -> ""
  in
  Printf.sprintf "%s/e%d%s/r%d/s%d" run_id position.entry_index for_each_part
    position.iterations_used position.step_index

(* Every attempt of the key so far, finished or interrupted. *)
let attempt_count run ~step_key =
  List.length
    (List.filter
       (fun step ->
         match step.attempt with
         | Some attempt -> attempt.step_key = step_key
         | None -> false)
       run.steps)
  + List.length
      (List.filter
         (fun attempt -> attempt.step_key = step_key)
         run.interrupted_attempts)

let attempt_id_of_number ~step_key number =
  Printf.sprintf "%s#%d" step_key number

let next_attempt_id run ~step_key =
  attempt_id_of_number ~step_key (attempt_count run ~step_key + 1)

let last_step_attempt_id steps =
  match List.rev steps with
  | { attempt = Some attempt; _ } :: _ -> Some attempt.attempt_id
  | _ -> None

(* The key's newest attempt, if it finished and did not pause the run. *)
let unrouted_finished_step run ~step_key =
  match List.rev run.steps with
  | ({ attempt = Some attempt; _ } as step) :: _
    when attempt.attempt_id
         = attempt_id_of_number ~step_key (attempt_count run ~step_key)
         && run.paused_on_attempt <> Some attempt.attempt_id ->
      Some step
  | _ -> None

let attempt_number attempt =
  match String.rindex_opt attempt.attempt_id '#' with
  | None -> 0
  | Some hash -> (
      let digits =
        String.sub attempt.attempt_id (hash + 1)
          (String.length attempt.attempt_id - hash - 1)
      in
      match int_of_string_opt digits with Some number -> number | None -> 0)

(* A skip or attach that moved the position past the attempt resolved it. *)
let attempt_at_position run attempt =
  attempt.step_key = step_key_of_position ~run_id:run.id ~position:run.position

(* Interrupted attempts at the position that no later attempt followed. *)
let unresolved_interrupted_attempts run =
  let candidates =
    List.map snd (Step_key_map.bindings run.attempts_in_flight)
    @ run.interrupted_attempts
  in
  let finished_attempts = List.filter_map (fun step -> step.attempt) run.steps in
  let followed attempt =
    List.exists
      (fun later ->
        later.step_key = attempt.step_key
        && attempt_number later > attempt_number attempt)
      (finished_attempts @ candidates)
  in
  List.filter
    (fun attempt -> attempt_at_position run attempt && not (followed attempt))
    candidates

let unresolved_interrupted_attempt run ~step_key =
  List.find_opt
    (fun attempt -> attempt.step_key = step_key)
    (unresolved_interrupted_attempts run)

(* Called under the run lock, so no live jig drives these attempts. *)
let mark_attempts_interrupted run =
  {
    run with
    interrupted_attempts =
      run.interrupted_attempts
      @ List.map snd (Step_key_map.bindings run.attempts_in_flight);
    attempts_in_flight = Step_key_map.empty;
  }

let string_of_outcome = function
  | Pass -> "pass"
  | Fail -> "fail"
  | Escalate -> "escalate"
  | Invalid_handoff -> "invalid-handoff"

let outcome_of_string = function
  | "pass" -> Ok Pass
  | "fail" -> Ok Fail
  | "escalate" -> Ok Escalate
  | "invalid-handoff" -> Ok Invalid_handoff
  | other -> Error (Printf.sprintf "run: unknown outcome %S" other)

let string_of_status = function
  | Running -> "running"
  | Paused -> "paused"
  | Completed -> "completed"
  | Failed -> "failed"
  | Aborted -> "aborted"

let status_of_string = function
  | "running" -> Ok Running
  | "paused" -> Ok Paused
  | "completed" -> Ok Completed
  | "failed" -> Ok Failed
  | "aborted" -> Ok Aborted
  | other -> Error (Printf.sprintf "run: unknown status %S" other)

let attempt_to_json attempt =
  `Assoc
    ([
       ("step_key", `String attempt.step_key);
       ("attempt_id", `String attempt.attempt_id);
       ("skill", `String attempt.skill);
     ]
    @ (match attempt.tier with
      | Some tier -> [ ("tier", `String tier) ]
      | None -> [])
    @ [
        ("command", `List (List.map (fun part -> `String part) attempt.command));
        ("started_at", `String attempt.started_at);
      ]
    @ (match attempt.child_pid with
      | Some pid -> [ ("child_pid", `Int pid) ]
      | None -> [])
    @ [
        ( "on_interrupt",
          `String (Skill.string_of_on_interrupt attempt.on_interrupt) );
      ]
    @ List.filter_map
        (fun (key, value) ->
          Option.map (fun text -> (key, `String text)) value)
        [
          ("session_id", attempt.session_id);
          ("probe_before", attempt.probe_before);
          ("probe_after", attempt.probe_after);
          ("stdout_path", attempt.stdout_path);
        ])

let step_to_json step =
  let handoff_field =
    match step.handoff with
    | Some handoff -> [ ("handoff", Handoff.to_json handoff) ]
    | None -> []
  in
  let handoff_error_field =
    match step.handoff_error with
    | Some message -> [ ("handoff_error", `String message) ]
    | None -> []
  in
  let session_field =
    match step.session_id with
    | Some id -> [ ("session_id", `String id) ]
    | None -> []
  in
  let tier_field =
    match step.tier with
    | Some tier -> [ ("tier", `String tier) ]
    | None -> []
  in
  let item_fields =
    (match step.item_index with
    | Some index -> [ ("item_index", `Int index) ]
    | None -> [])
    @
    match step.item_key with
    | Some key -> [ ("item_key", `String key) ]
    | None -> []
  in
  `Assoc
    ([
       ("skill", `String step.skill);
       ("outcome", `String (string_of_outcome step.outcome));
       ("exit_code", `Int step.exit_code);
       ("cost_usd", Metering.cost_to_json step.cost);
     ]
    @ tier_field @ handoff_field @ handoff_error_field @ session_field
    @ item_fields
    @ (match step.attempt with
      | Some attempt -> [ ("attempt", attempt_to_json attempt) ]
      | None -> [])
    @ [
        ("stdout", `String step.stdout);
        ("stderr", `String step.stderr);
        ("started_at", `String step.started_at);
        ("finished_at", `String step.finished_at);
      ])

let to_json run =
  let error_field =
    match run.error with
    | Some message -> [ ("error", `String message) ]
    | None -> []
  in
  let finished_field =
    match run.finished_at with
    | Some finished_at -> [ ("finished_at", `String finished_at) ]
    | None -> []
  in
  let workspace_field =
    match run.workspace with
    | Some workspace -> [ ("workspace", `String workspace) ]
    | None -> []
  in
  `Assoc
    ([
       ("id", `String run.id);
       ("workflow", `String run.workflow);
       ("task", `String run.task);
       ("status", `String (string_of_status run.status));
     ]
    @ error_field @ workspace_field
    @ [
        ( "position",
          `Assoc
            ([
               ("entry_index", `Int run.position.entry_index);
               ("iterations_used", `Int run.position.iterations_used);
             ]
            @ (if run.position.step_index = 0 then []
               else [ ("step_index", `Int run.position.step_index) ])
            @
            (* Emitted only while a forEach entry is in flight, so runs that
               never fan out serialize exactly as before. *)
            match run.position.for_each with
            | None -> []
            | Some state ->
                [
                  ( "for_each",
                    `Assoc
                      [
                        ("item_index", `Int state.item_index);
                        ("body_index", `Int state.body_index);
                        ( "items",
                          `List
                            (List.map
                               (fun bindings ->
                                 `Assoc
                                   (List.map
                                      (fun (key, value) ->
                                        (key, `String value))
                                      bindings))
                               state.items) );
                      ] );
                ]) );
        ("steps", `List (List.map step_to_json run.steps));
      ]
    @ (if Step_key_map.is_empty run.attempts_in_flight then []
       else
         [
           ( "attempts_in_flight",
             `Assoc
               (List.map
                  (fun (step_key, attempt) ->
                    (step_key, attempt_to_json attempt))
                  (Step_key_map.bindings run.attempts_in_flight)) );
         ])
    @ (match run.interrupted_attempts with
      | [] -> []
      | attempts ->
          [ ("interrupted_attempts", `List (List.map attempt_to_json attempts)) ])
    @ (match run.paused_on_attempt with
      | None -> []
      | Some attempt_id -> [ ("paused_on_attempt", `String attempt_id) ])
    @ [ ("started_at", `String run.started_at) ]
    @ finished_field)

(* Total of the known step costs, plus how many steps reported nothing -
   callers must not present a partial total as the whole truth. *)
let cost_summary run =
  List.fold_left
    (fun (total, unknown) step ->
      match step.cost with
      | Metering.Cost_usd value -> (total +. value, unknown)
      | Metering.Unknown_cost -> (total, unknown + 1))
    (0.0, 0) run.steps

(* Per-tier totals in first-seen order: (tier, known cost, steps, steps
   with unknown cost). None is the default harness. The whole point of
   tiers is the economics, so the breakdown must be inspectable per run. *)
let cost_by_tier run =
  List.fold_left
    (fun totals step ->
      let known, unknown =
        match step.cost with
        | Metering.Cost_usd value -> (value, 0)
        | Metering.Unknown_cost -> (0.0, 1)
      in
      let rec add = function
        | [] -> [ (step.tier, known, 1, unknown) ]
        | (tier, total, steps, unknowns) :: rest when tier = step.tier ->
            (tier, total +. known, steps + 1, unknowns + unknown) :: rest
        | entry :: rest -> entry :: add rest
      in
      add totals)
    [] run.steps

let last_handoff run =
  List.fold_left
    (fun previous step ->
      match step.handoff with Some _ as found -> found | None -> previous)
    None run.steps

let member_string json key =
  match Yojson.Safe.Util.member key json with
  | `String value -> Ok value
  | _ -> Error (Printf.sprintf "run: missing or non-string key %S" key)

let member_int json key =
  match Yojson.Safe.Util.member key json with
  | `Int value -> Ok value
  | _ -> Error (Printf.sprintf "run: missing or non-integer key %S" key)

let optional_string json key =
  match Yojson.Safe.Util.member key json with
  | `String value -> Some value
  | _ -> None

let optional_int json key =
  match Yojson.Safe.Util.member key json with
  | `Int value -> Some value
  | _ -> None

let attempt_of_json json =
  let* step_key = member_string json "step_key" in
  let* attempt_id = member_string json "attempt_id" in
  let* skill = member_string json "skill" in
  let* command =
    match Yojson.Safe.Util.member "command" json with
    | `List parts ->
        List.fold_right
          (fun part accumulator ->
            let* parts = accumulator in
            match part with
            | `String text -> Ok (text :: parts)
            | _ -> Error "run: attempt command parts must be strings")
          parts (Ok [])
    | _ -> Error "run: attempt command must be a list"
  in
  let* started_at = member_string json "started_at" in
  (* Attempts recorded before the policy existed take the default. *)
  let* on_interrupt =
    match optional_string json "on_interrupt" with
    | None -> Ok Skill.Recover
    | Some text ->
        Result.map_error (fun message -> "run: " ^ message)
          (Skill.on_interrupt_of_string text)
  in
  Ok
    {
      step_key;
      attempt_id;
      skill;
      tier = optional_string json "tier";
      command;
      started_at;
      child_pid = optional_int json "child_pid";
      session_id = optional_string json "session_id";
      on_interrupt;
      probe_before = optional_string json "probe_before";
      probe_after = optional_string json "probe_after";
      stdout_path = optional_string json "stdout_path";
    }

let optional_attempt json =
  match Yojson.Safe.Util.member "attempt" json with
  | `Null -> Ok None
  | attempt_json ->
      let* attempt = attempt_of_json attempt_json in
      Ok (Some attempt)

let step_of_json json =
  let* skill = member_string json "skill" in
  let* outcome_string = member_string json "outcome" in
  let* outcome = outcome_of_string outcome_string in
  let* exit_code = member_int json "exit_code" in
  let* cost = Metering.cost_of_json (Yojson.Safe.Util.member "cost_usd" json) in
  let* stdout = member_string json "stdout" in
  let* stderr = member_string json "stderr" in
  let* started_at = member_string json "started_at" in
  let* finished_at = member_string json "finished_at" in
  let* handoff =
    match Yojson.Safe.Util.member "handoff" json with
    | `Null -> Ok None
    | handoff_json ->
        let* handoff = Handoff.of_json handoff_json in
        Ok (Some handoff)
  in
  let* attempt = optional_attempt json in
  Ok
    {
      skill;
      outcome;
      exit_code;
      cost;
      tier = optional_string json "tier";
      stdout;
      stderr;
      handoff;
      handoff_error = optional_string json "handoff_error";
      session_id = optional_string json "session_id";
      item_index = optional_int json "item_index";
      item_key = optional_string json "item_key";
      attempt;
      started_at;
      finished_at;
    }

let of_json json =
  let* id = member_string json "id" in
  let* workflow = member_string json "workflow" in
  let* task = member_string json "task" in
  let* status_string = member_string json "status" in
  let* status = status_of_string status_string in
  let position_json = Yojson.Safe.Util.member "position" json in
  let* entry_index = member_int position_json "entry_index" in
  let* iterations_used = member_int position_json "iterations_used" in
  (* Absent outside a retry group, and in older records. *)
  let* step_index =
    match Yojson.Safe.Util.member "step_index" position_json with
    | `Null -> Ok 0
    | `Int value when value >= 0 -> Ok value
    | _ -> Error "run: position step_index must be a non-negative integer"
  in
  let* for_each =
    match Yojson.Safe.Util.member "for_each" position_json with
    | `Null -> Ok None
    | state_json ->
        let* item_index = member_int state_json "item_index" in
        let* body_index = member_int state_json "body_index" in
        let* items =
          match Yojson.Safe.Util.member "items" state_json with
          | `List item_entries ->
              List.fold_right
                (fun item accumulator ->
                  let* items = accumulator in
                  match item with
                  | `Assoc pairs ->
                      let* bindings =
                        List.fold_right
                          (fun (key, value) bindings ->
                            let* bindings = bindings in
                            match value with
                            | `String text -> Ok ((key, text) :: bindings)
                            | _ ->
                                Error
                                  (Printf.sprintf
                                     "run: for_each item value %S must be a \
                                      string"
                                     key))
                          pairs (Ok [])
                      in
                      Ok (bindings :: items)
                  | _ -> Error "run: for_each items must be objects")
                item_entries (Ok [])
          | _ -> Error "run: for_each items must be a list"
        in
        Ok (Some { item_index; body_index; items })
  in
  let* steps =
    match Yojson.Safe.Util.member "steps" json with
    | `List entries ->
        List.fold_right
          (fun entry accumulator ->
            let* steps = accumulator in
            let* step = step_of_json entry in
            Ok (step :: steps))
          entries (Ok [])
    | _ -> Error "run: steps must be a list"
  in
  let* attempts_in_flight =
    match Yojson.Safe.Util.member "attempts_in_flight" json with
    | `Null -> Ok Step_key_map.empty
    | `Assoc pairs ->
        List.fold_left
          (fun accumulator (_, attempt_json) ->
            let* attempts = accumulator in
            let* attempt = attempt_of_json attempt_json in
            Ok (Step_key_map.add attempt.step_key attempt attempts))
          (Ok Step_key_map.empty) pairs
    | _ -> Error "run: attempts_in_flight must be an object"
  in
  let* interrupted_attempts =
    match Yojson.Safe.Util.member "interrupted_attempts" json with
    | `Null -> Ok []
    | `List entries ->
        List.fold_right
          (fun entry accumulator ->
            let* attempts = accumulator in
            let* attempt = attempt_of_json entry in
            Ok (attempt :: attempts))
          entries (Ok [])
    | _ -> Error "run: interrupted_attempts must be a list"
  in
  let* started_at = member_string json "started_at" in
  (* An older paused record has no field; its last step is what paused it. *)
  let paused_on_attempt =
    match optional_string json "paused_on_attempt" with
    | Some _ as attempt_id -> attempt_id
    | None when status = Paused -> last_step_attempt_id steps
    | None -> None
  in
  Ok
    {
      id;
      workflow;
      task;
      status;
      error = optional_string json "error";
      position = { entry_index; iterations_used; step_index; for_each };
      workspace = optional_string json "workspace";
      steps;
      attempts_in_flight;
      interrupted_attempts;
      paused_on_attempt;
      started_at;
      finished_at = optional_string json "finished_at";
    }
