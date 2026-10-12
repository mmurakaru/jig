(* Every id derives from the record, so a live step and any export agree. *)

(* MD5 from the stdlib Digest: deterministic, 16 bytes, no new dependency. *)
let hex_digest text = Digest.to_hex (Digest.string text)

let trace_id_of_run ~run_id = hex_digest run_id

let root_span_id_of_run ~run_id =
  String.sub (hex_digest (run_id ^ "#run")) 0 16

(* An attempt id is unique in its run and fixed before the process starts. *)
let attempt_span_id ~attempt_id =
  String.sub (hex_digest ("attempt:" ^ attempt_id)) 0 16

(* For a record with no attempt: a human skip, an attach hand-back, or older. *)
let step_position_span_id ~run_id ~step_index =
  String.sub (hex_digest (run_id ^ "#step:" ^ string_of_int step_index)) 0 16

let step_span_id ~run_id ~step_index (step : Run.step_record) =
  match step.Run.attempt with
  | Some attempt -> attempt_span_id ~attempt_id:attempt.Run.attempt_id
  | None -> step_position_span_id ~run_id ~step_index

let traceparent ~run_id ~span_id =
  Printf.sprintf "00-%s-%s-01" (trace_id_of_run ~run_id) span_id

let telemetry_disabled ~getenv =
  match getenv "OTEL_SDK_DISABLED" with
  | Some value -> String.lowercase_ascii (String.trim value) = "true"
  | None -> false

let traceparent_environment ~getenv ~run_id ~span_id =
  if telemetry_disabled ~getenv then []
  else [ ("TRACEPARENT", traceparent ~run_id ~span_id) ]

let percent_decode text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec decode index =
    if index < length then
      let escaped_code =
        if text.[index] = '%' && index + 2 < length then
          int_of_string_opt ("0x" ^ String.sub text (index + 1) 2)
        else None
      in
      match escaped_code with
      | Some code ->
          Buffer.add_char buffer (Char.chr code);
          decode (index + 3)
      | None ->
          Buffer.add_char buffer text.[index];
          decode (index + 1)
  in
  decode 0;
  Buffer.contents buffer

(* OTEL_SERVICE_NAME wins over service.name in OTEL_RESOURCE_ATTRIBUTES. *)
let resource_attributes_of_environment ~getenv =
  let listed =
    match getenv "OTEL_RESOURCE_ATTRIBUTES" with
    | None -> []
    | Some text ->
        List.filter_map
          (fun pair ->
            match String.index_opt pair '=' with
            | None -> None
            | Some equals_index ->
                let key = String.trim (String.sub pair 0 equals_index) in
                let value =
                  String.sub pair (equals_index + 1)
                    (String.length pair - equals_index - 1)
                in
                if key = "" then None
                else Some (key, percent_decode (String.trim value)))
          (String.split_on_char ',' text)
  in
  let service_name =
    match getenv "OTEL_SERVICE_NAME" with
    | Some name when String.trim name <> "" -> String.trim name
    | _ -> Option.value (List.assoc_opt "service.name" listed) ~default:"jig"
  in
  ("service.name", service_name)
  :: List.filter (fun (key, _) -> key <> "service.name") listed

let attribute key value = `Assoc [ ("key", `String key); ("value", value) ]
let string_value text = `Assoc [ ("stringValue", `String text) ]

(* OTLP/JSON encodes 64-bit integers as decimal strings. *)
let int_value number = `Assoc [ ("intValue", `String (string_of_int number)) ]
let double_value number = `Assoc [ ("doubleValue", `Float number) ]

let string_array_value texts =
  `Assoc
    [
      ( "arrayValue",
        `Assoc [ ("values", `List (List.map string_value texts)) ] );
    ]

(* Recorded times carry at most milliseconds. *)
let unix_nano_of_iso8601 timestamp =
  match Run.epoch_of_iso8601 timestamp with
  | Some seconds ->
      Printf.sprintf "%d000000" (int_of_float (Float.round (seconds *. 1000.0)))
  | None -> "0"

let span_kind_internal = 1
let status_code_ok = 1
let status_code_error = 2

let status_json ~code ~message =
  `Assoc
    ([ ("code", `Int code) ]
    @
    match message with
    | Some text -> [ ("message", `String text) ]
    | None -> [])

(* Cut at a UTF-8 boundary so the JSON stays valid text. *)
let truncate_utf8 ~max_bytes text =
  if String.length text <= max_bytes then text
  else
    let rec boundary index =
      if index > 0 && Char.code text.[index] land 0xC0 = 0x80 then
        boundary (index - 1)
      else index
    in
    String.sub text 0 (boundary max_bytes) ^ "..."

let handoff_summary_limit = 1024

(* The GenAI conventions count cached tokens inside input_tokens. *)
let usage_attributes usage =
  let count key =
    match Yojson.Safe.Util.member key usage with
    | `Int value -> Some value
    | _ -> None
  in
  let input = count "input_tokens" in
  let output = count "output_tokens" in
  let cache_read = count "cache_read_input_tokens" in
  let cache_creation = count "cache_creation_input_tokens" in
  let total_input =
    match (input, cache_read, cache_creation) with
    | None, None, None -> None
    | _ ->
        Some
          (List.fold_left
             (fun total part -> total + Option.value part ~default:0)
             0
             [ input; cache_read; cache_creation ])
  in
  List.filter_map
    (fun (key, value) ->
      Option.map (fun number -> attribute key (int_value number)) value)
    [
      ("gen_ai.usage.input_tokens", total_input);
      ("gen_ai.usage.output_tokens", output);
      ("gen_ai.usage.cache_read.input_tokens", cache_read);
      ("gen_ai.usage.cache_creation.input_tokens", cache_creation);
    ]

let event ~name ~time ~attributes =
  `Assoc
    [
      ("timeUnixNano", `String (unix_nano_of_iso8601 time));
      ("name", `String name);
      ("attributes", `List attributes);
    ]

let optional_string_attribute key = function
  | Some text -> [ attribute key (string_value text) ]
  | None -> []

let optional_int_attribute key = function
  | Some number -> [ attribute key (int_value number) ]
  | None -> []

let attempt_attributes (attempt : Run.attempt) =
  [
    attribute "jig.step.key" (string_value attempt.Run.step_key);
    attribute "jig.attempt.id" (string_value attempt.Run.attempt_id);
  ]

let timeout_event (step : Run.step_record) =
  let requested_at, limit_attributes =
    match step.Run.attempt with
    | Some attempt ->
        ( attempt.Run.timeout_requested_at,
          optional_int_attribute "jig.timeout.limit_seconds"
            attempt.Run.time_limit_seconds
          @ optional_string_attribute "jig.timeout.deadline_at"
              attempt.Run.deadline_at )
    | None -> (None, [])
  in
  event ~name:"jig.timeout"
    ~time:(Option.value requested_at ~default:step.Run.finished_at)
    ~attributes:limit_attributes

let step_events (step : Run.step_record) =
  let resume_event =
    if step.Run.resumed then
      [ event ~name:"jig.resume" ~time:step.Run.started_at ~attributes:[] ]
    else []
  in
  let handoff_event =
    match step.Run.handoff with
    | None -> []
    | Some handoff ->
        let artifacts =
          match handoff.Handoff.artifacts with
          | [] -> []
          | paths ->
              [ attribute "jig.handoff.artifacts" (string_array_value paths) ]
        in
        [
          event ~name:"jig.handoff" ~time:step.Run.finished_at
            ~attributes:
              ([
                 attribute "jig.handoff.status"
                   (string_value
                      (Handoff.string_of_status handoff.Handoff.status));
                 attribute "jig.handoff.summary"
                   (string_value
                      (truncate_utf8 ~max_bytes:handoff_summary_limit
                         handoff.Handoff.summary));
               ]
              @ artifacts);
        ]
  in
  let handoff_error_event =
    match step.Run.handoff_error with
    | None -> []
    | Some message ->
        [
          event ~name:"jig.handoff_error" ~time:step.Run.finished_at
            ~attributes:
              [ attribute "jig.handoff_error.message" (string_value message) ];
        ]
  in
  let outcome_event =
    match step.Run.outcome with
    | Run.Escalate ->
        [ event ~name:"jig.escalate" ~time:step.Run.finished_at ~attributes:[] ]
    | Run.Timeout -> [ timeout_event step ]
    | Run.Pass | Run.Fail | Run.Invalid_handoff -> []
  in
  resume_event @ outcome_event @ handoff_event @ handoff_error_event

let step_span ~run_id ~step_index (step : Run.step_record) =
  let optional_string = optional_string_attribute in
  let cost =
    match step.Run.cost with
    | Metering.Cost_usd value ->
        [ attribute "jig.cost_usd" (double_value value) ]
    | Metering.Unknown_cost -> []
  in
  let item_index =
    match step.Run.item_index with
    | Some index -> [ attribute "jig.item.index" (int_value index) ]
    | None -> []
  in
  let usage =
    match snd (Metering.parse_cost step.Run.stdout) with
    | Some usage -> usage_attributes usage
    | None -> []
  in
  let outcome = Run.string_of_outcome step.Run.outcome in
  let status =
    match step.Run.outcome with
    | Run.Pass -> [ ("status", status_json ~code:status_code_ok ~message:None) ]
    | Run.Fail | Run.Invalid_handoff | Run.Timeout ->
        [
          ( "status",
            status_json ~code:status_code_error ~message:(Some outcome) );
        ]
    | Run.Escalate -> []
  in
  let error_type =
    match step.Run.outcome with
    | Run.Fail | Run.Invalid_handoff | Run.Timeout ->
        [ attribute "error.type" (string_value outcome) ]
    | Run.Pass | Run.Escalate -> []
  in
  let attempt =
    match step.Run.attempt with
    | Some attempt -> attempt_attributes attempt
    | None -> []
  in
  `Assoc
    ([
       ("traceId", `String (trace_id_of_run ~run_id));
       ("spanId", `String (step_span_id ~run_id ~step_index step));
       ("parentSpanId", `String (root_span_id_of_run ~run_id));
       ("name", `String step.Run.skill);
       ("kind", `Int span_kind_internal);
       ( "startTimeUnixNano",
         `String (unix_nano_of_iso8601 step.Run.started_at) );
       ( "endTimeUnixNano",
         `String (unix_nano_of_iso8601 step.Run.finished_at) );
       ( "attributes",
         `List
           ([
              attribute "jig.step.index" (int_value step_index);
              attribute "jig.step.name" (string_value step.Run.skill);
              attribute "jig.step.outcome" (string_value outcome);
              attribute "jig.step.exit_code" (int_value step.Run.exit_code);
            ]
           @ attempt
           @ optional_string "jig.step.tier" step.Run.tier
           @ cost @ item_index
           @ optional_string "jig.item.key" step.Run.item_key
           @ optional_string "gen_ai.conversation.id" step.Run.session_id
           @ usage @ error_type) );
       ("events", `List (step_events step));
     ]
    @ status)

let root_span (run : Run.t) =
  let run_id = run.Run.id in
  let known_cost, unknown_cost_steps = Run.cost_summary run in
  let end_time =
    match (run.Run.finished_at, List.rev run.Run.steps) with
    | Some finished_at, _ -> finished_at
    | None, last_step :: _ -> last_step.Run.finished_at
    | None, [] -> run.Run.started_at
  in
  let status_name = Run.string_of_status run.Run.status in
  let status =
    match run.Run.status with
    | Run.Completed ->
        [ ("status", status_json ~code:status_code_ok ~message:None) ]
    | Run.Failed | Run.Aborted ->
        [
          ( "status",
            status_json ~code:status_code_error
              ~message:
                (Some (Option.value run.Run.error ~default:status_name)) );
        ]
    | Run.Running | Run.Paused -> []
  in
  `Assoc
    ([
       ("traceId", `String (trace_id_of_run ~run_id));
       ("spanId", `String (root_span_id_of_run ~run_id));
       ("name", `String ("invoke_workflow " ^ run.Run.workflow));
       ("kind", `Int span_kind_internal);
       ("startTimeUnixNano", `String (unix_nano_of_iso8601 run.Run.started_at));
       ("endTimeUnixNano", `String (unix_nano_of_iso8601 end_time));
       ( "attributes",
         `List
           ([
              attribute "gen_ai.operation.name"
                (string_value "invoke_workflow");
              attribute "gen_ai.workflow.name" (string_value run.Run.workflow);
              attribute "jig.run.id" (string_value run_id);
              attribute "jig.run.status" (string_value status_name);
              attribute "jig.task" (string_value run.Run.task);
              attribute "jig.cost_usd" (double_value known_cost);
              attribute "jig.cost.unknown_steps" (int_value unknown_cost_steps);
            ]
           @
           match run.Run.workspace with
           | Some path -> [ attribute "jig.workspace" (string_value path) ]
           | None -> []) );
     ]
    @ status)

let run_is_terminal (run : Run.t) =
  match run.Run.status with
  | Run.Completed | Run.Failed | Run.Aborted -> true
  | Run.Running | Run.Paused -> false

let interrupted_attempt_span ~run_id ~end_time (attempt : Run.attempt) =
  let item_index =
    Option.bind
      (Run.coordinates_of_step_key ~run_id attempt.Run.step_key)
      (fun coordinates -> coordinates.Run.coordinate_item_index)
  in
  `Assoc
    [
      ("traceId", `String (trace_id_of_run ~run_id));
      ( "spanId",
        `String (attempt_span_id ~attempt_id:attempt.Run.attempt_id) );
      ("parentSpanId", `String (root_span_id_of_run ~run_id));
      ("name", `String attempt.Run.skill);
      ("kind", `Int span_kind_internal);
      ( "startTimeUnixNano",
        `String (unix_nano_of_iso8601 attempt.Run.started_at) );
      ("endTimeUnixNano", `String (unix_nano_of_iso8601 end_time));
      ( "attributes",
        `List
          ([
             attribute "jig.step.name" (string_value attempt.Run.skill);
             attribute "jig.step.outcome" (string_value "interrupted");
             attribute "error.type" (string_value "interrupted");
           ]
          @ attempt_attributes attempt
          @ optional_string_attribute "jig.step.tier" attempt.Run.tier
          @ optional_int_attribute "jig.item.index" item_index
          @ optional_string_attribute "gen_ai.conversation.id"
              attempt.Run.session_id) );
      ( "events",
        `List [ event ~name:"jig.interrupt" ~time:end_time ~attributes:[] ] );
      ( "status",
        status_json ~code:status_code_error ~message:(Some "interrupted") );
    ]

(* Sent once its end is final: receivers keep a span's first copy. *)
let interrupted_attempt_spans (run : Run.t) =
  let finished_attempts =
    List.filter_map (fun (step : Run.step_record) -> step.Run.attempt)
      run.Run.steps
  in
  let all_attempts = finished_attempts @ run.Run.interrupted_attempts in
  List.filter_map
    (fun (attempt : Run.attempt) ->
      let finished =
        List.exists
          (fun (other : Run.attempt) ->
            other.Run.attempt_id = attempt.Run.attempt_id)
          finished_attempts
      in
      let next_start =
        List.fold_left
          (fun earliest (later : Run.attempt) ->
            if
              later.Run.step_key = attempt.Run.step_key
              && Run.attempt_number later > Run.attempt_number attempt
            then
              match earliest with
              | Some (number, _) when number <= Run.attempt_number later ->
                  earliest
              | _ -> Some (Run.attempt_number later, later.Run.started_at)
            else earliest)
          None all_attempts
      in
      let end_time =
        match (next_start, run.Run.finished_at) with
        | Some (_, started_at), _ -> Some started_at
        | None, Some finished_at when run_is_terminal run -> Some finished_at
        | None, _ -> None
      in
      match end_time with
      | Some end_time when not finished ->
          Some (interrupted_attempt_span ~run_id:run.Run.id ~end_time attempt)
      | _ -> None)
    run.Run.interrupted_attempts

(* The root waits for a terminal run: receivers keep a span's first copy. *)
let spans_of_run (run : Run.t) =
  let run_id = run.Run.id in
  let step_spans =
    List.mapi
      (fun step_index step -> step_span ~run_id ~step_index step)
      run.Run.steps
    @ interrupted_attempt_spans run
  in
  if run_is_terminal run then root_span run :: step_spans else step_spans

let span_id_of_span span =
  match Yojson.Safe.Util.member "spanId" span with
  | `String span_id -> span_id
  | _ -> ""

(** One OTLP/JSON TracesData document; no spans gives an empty, valid one. *)
let traces_document ~resource_attributes spans =
  match spans with
  | [] -> `Assoc [ ("resourceSpans", `List []) ]
  | spans ->
      `Assoc
        [
          ( "resourceSpans",
            `List
              [
                `Assoc
                  [
                    ( "resource",
                      `Assoc
                        [
                          ( "attributes",
                            `List
                              (List.map
                                 (fun (key, value) ->
                                   attribute key (string_value value))
                                 resource_attributes) );
                        ] );
                    ( "scopeSpans",
                      `List
                        [
                          `Assoc
                            [
                              ("scope", `Assoc [ ("name", `String "jig") ]);
                              ("spans", `List spans);
                            ];
                        ] );
                  ];
              ] );
        ]

let traces_of_run ~resource_attributes run =
  traces_document ~resource_attributes (spans_of_run run)
