open Result_syntax

type outcome = Pass | Fail | Escalate | Invalid_handoff

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
   full iterations a retry has used and, for a forEach entry, the item
   cursor and snapshot. Enough to resume. *)
type position = {
  entry_index : int;
  iterations_used : int;
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
        ("started_at", `String run.started_at);
      ]
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
  let* started_at = member_string json "started_at" in
  Ok
    {
      id;
      workflow;
      task;
      status;
      error = optional_string json "error";
      position = { entry_index; iterations_used; for_each };
      workspace = optional_string json "workspace";
      steps;
      started_at;
      finished_at = optional_string json "finished_at";
    }
