type outcome = Pass | Fail

type step_record = {
  skill : string;
  outcome : outcome;
  exit_code : int;
  stdout : string;
  stderr : string;
  started_at : string;
  finished_at : string;
}

type status = Completed | Failed

type t = {
  id : string;
  workflow : string;
  task : string;
  status : status;
  error : string option;
  steps : step_record list;
  started_at : string;
  finished_at : string;
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

let string_of_outcome = function Pass -> "pass" | Fail -> "fail"
let string_of_status = function Completed -> "completed" | Failed -> "failed"

let step_to_json step =
  `Assoc
    [
      ("skill", `String step.skill);
      ("outcome", `String (string_of_outcome step.outcome));
      ("exit_code", `Int step.exit_code);
      ("stdout", `String step.stdout);
      ("stderr", `String step.stderr);
      ("started_at", `String step.started_at);
      ("finished_at", `String step.finished_at);
    ]

let to_json run =
  let error_field =
    match run.error with
    | Some message -> [ ("error", `String message) ]
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
        ("steps", `List (List.map step_to_json run.steps));
        ("started_at", `String run.started_at);
        ("finished_at", `String run.finished_at);
      ])
