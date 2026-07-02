open Result_syntax

type outcome = Pass | Fail | Escalate | Invalid_handoff

type step_record = {
  skill : string;
  outcome : outcome;
  exit_code : int;
  stdout : string;
  stderr : string;
  handoff : Handoff.t option;
  handoff_error : string option;
  started_at : string;
  finished_at : string;
}

type status = Running | Paused | Completed | Failed | Aborted

(* Where the run is in the workflow: the entry being executed and, for a
   retry entry, how many full iterations have been used. Enough to resume. *)
type position = { entry_index : int; iterations_used : int }

type t = {
  id : string;
  workflow : string;
  task : string;
  status : status;
  error : string option;
  position : position;
  steps : step_record list;
  started_at : string;
  finished_at : string option;
}

let iso8601 time =
  let utc = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (utc.Unix.tm_year + 1900)
    (utc.Unix.tm_mon + 1) utc.Unix.tm_mday utc.Unix.tm_hour utc.Unix.tm_min
    utc.Unix.tm_sec

let make_id ~workflow ~time ~pid =
  let utc = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02d-%s-%02d%02d%02d-%d" (utc.Unix.tm_year + 1900)
    (utc.Unix.tm_mon + 1) utc.Unix.tm_mday workflow utc.Unix.tm_hour
    utc.Unix.tm_min utc.Unix.tm_sec pid

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
  `Assoc
    ([
       ("skill", `String step.skill);
       ("outcome", `String (string_of_outcome step.outcome));
       ("exit_code", `Int step.exit_code);
     ]
    @ handoff_field @ handoff_error_field
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
  `Assoc
    ([
       ("id", `String run.id);
       ("workflow", `String run.workflow);
       ("task", `String run.task);
       ("status", `String (string_of_status run.status));
     ]
    @ error_field
    @ [
        ( "position",
          `Assoc
            [
              ("entry_index", `Int run.position.entry_index);
              ("iterations_used", `Int run.position.iterations_used);
            ] );
        ("steps", `List (List.map step_to_json run.steps));
        ("started_at", `String run.started_at);
      ]
    @ finished_field)

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

let step_of_json json =
  let* skill = member_string json "skill" in
  let* outcome_string = member_string json "outcome" in
  let* outcome = outcome_of_string outcome_string in
  let* exit_code = member_int json "exit_code" in
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
      stdout;
      stderr;
      handoff;
      handoff_error = optional_string json "handoff_error";
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
      position = { entry_index; iterations_used };
      steps;
      started_at;
      finished_at = optional_string json "finished_at";
    }
