type cost = Cost_usd of float | Unknown_cost

type event = {
  run_id : string;
  skill : string;
  command : string list;
  (* The tier that selected the command; None means the default harness. *)
  tier : string option;
  cost : cost;
  usage : Yojson.Safe.t option;
  (* None for an attach hand-back, which is not a step attempt. *)
  step_key : string option;
  attempt_id : string option;
  interrupted : bool;
  recorded_at : string;
}

(* A harness that reports nothing meters as unknown, not zero - absence of
   data must stay distinguishable from free. *)
let parse_cost stdout =
  match Yojson.Safe.from_string stdout with
  | exception _ -> (Unknown_cost, None)
  | `Assoc _ as json ->
      let cost =
        match Yojson.Safe.Util.member "total_cost_usd" json with
        | `Float value -> Cost_usd value
        | `Int value -> Cost_usd (float_of_int value)
        | _ -> Unknown_cost
      in
      let usage =
        match Yojson.Safe.Util.member "usage" json with
        | `Assoc _ as usage -> Some usage
        | _ -> None
      in
      (cost, usage)
  | _ -> (Unknown_cost, None)

(* The id an interactive harness needs to reopen the step's session. *)
let parse_session_id stdout =
  match Yojson.Safe.from_string stdout with
  | exception _ -> None
  | `Assoc _ as json -> (
      match Yojson.Safe.Util.member "session_id" json with
      | `String value -> Some value
      | _ -> None)
  | _ -> None

let cost_to_json = function
  | Cost_usd value -> `Float value
  | Unknown_cost -> `String "unknown"

let cost_of_json = function
  | `Float value -> Ok (Cost_usd value)
  | `Int value -> Ok (Cost_usd (float_of_int value))
  | `String "unknown" -> Ok Unknown_cost
  | _ -> Error "metering: cost must be a number or \"unknown\""

let event_to_json event =
  `Assoc
    ([
       ("run_id", `String event.run_id);
       ("skill", `String event.skill);
       ("harness", `List (List.map (fun part -> `String part) event.command));
       ("cost_usd", cost_to_json event.cost);
     ]
    @ (match event.tier with
      | Some tier -> [ ("tier", `String tier) ]
      | None -> [])
    @ (match event.usage with
      | Some usage -> [ ("usage", usage) ]
      | None -> [])
    @ (match event.step_key with
      | Some step_key -> [ ("step_key", `String step_key) ]
      | None -> [])
    @ (match event.attempt_id with
      | Some attempt_id -> [ ("attempt_id", `String attempt_id) ]
      | None -> [])
    @ (if event.interrupted then [ ("interrupted", `Bool true) ] else [])
    @ [ ("recorded_at", `String event.recorded_at) ])

module type S = sig
  (* Appends unconditionally; the runner skips attempt ids already recorded. *)
  val record : runs_dir:string -> event:event -> (unit, string) result

  (* Read once per drive, so an append never rescans the ledger. *)
  val recorded_attempt_ids :
    runs_dir:string -> run_id:string -> (string list, string) result
end

(* Default: append-only JSONL next to the run records. *)
module Jsonl : S = struct
  let ensure_directory path =
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let ledger_path ~runs_dir = Filename.concat runs_dir "metering.jsonl"

  (* A torn or foreign line names no attempt, so it is skipped. *)
  let line_attempt_id ~run_id line =
    match Yojson.Safe.from_string line with
    | exception Yojson.Json_error _ -> None
    | json -> (
        match
          ( Yojson.Safe.Util.member "run_id" json,
            Yojson.Safe.Util.member "attempt_id" json )
        with
        | `String line_run_id, `String attempt_id when line_run_id = run_id ->
            Some attempt_id
        | _ -> None
        | exception Yojson.Safe.Util.Type_error _ -> None)

  let recorded_attempt_ids ~runs_dir ~run_id =
    let path = ledger_path ~runs_dir in
    if not (Sys.file_exists path) then Ok []
    else
      try
        Ok
          (In_channel.with_open_text path (fun channel ->
               let rec scan attempt_ids =
                 match In_channel.input_line channel with
                 | None -> List.rev attempt_ids
                 | Some line -> (
                     match line_attempt_id ~run_id line with
                     | Some attempt_id -> scan (attempt_id :: attempt_ids)
                     | None -> scan attempt_ids)
               in
               scan []))
      with Sys_error message -> Error (Printf.sprintf "metering: %s" message)

  (* Ends a line torn by a crash, so the next event stays parseable. *)
  let ends_mid_line path =
    In_channel.with_open_bin path (fun channel ->
        let length = In_channel.length channel in
        length > 0L
        &&
        (In_channel.seek channel (Int64.pred length);
         In_channel.input_char channel <> Some '\n'))

  let append ~path line =
    let separator =
      if Sys.file_exists path && ends_mid_line path then "\n" else ""
    in
    let channel =
      open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 path
    in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel (separator ^ line ^ "\n"))

  let record ~runs_dir ~event =
    try
      ensure_directory runs_dir;
      append ~path:(ledger_path ~runs_dir)
        (Yojson.Safe.to_string (event_to_json event));
      Ok ()
    with
    | Sys_error message -> Error (Printf.sprintf "metering: %s" message)
    | Unix.Unix_error (error, _, _) ->
        Error (Printf.sprintf "metering: %s" (Unix.error_message error))
end

module Noop : S = struct
  let record ~runs_dir:_ ~event:_ = Ok ()
  let recorded_attempt_ids ~runs_dir:_ ~run_id:_ = Ok []
end
