(* Resolves the command that reopens a step's harness session
   interactively. The template comes from config; every occurrence of
   a placeholder is replaced with its value. *)

let substitute ~placeholder ~value part =
  let placeholder_length = String.length placeholder in
  let buffer = Buffer.create (String.length part) in
  let rec copy from =
    if from >= String.length part then ()
    else
      match String.index_from_opt part from '{' with
      | None ->
          Buffer.add_substring buffer part from (String.length part - from)
      | Some brace ->
          Buffer.add_substring buffer part from (brace - from);
          if
            brace + placeholder_length <= String.length part
            && String.sub part brace placeholder_length = placeholder
          then (
            Buffer.add_string buffer value;
            copy (brace + placeholder_length))
          else (
            Buffer.add_char buffer '{';
            copy (brace + 1))
  in
  copy 0;
  Buffer.contents buffer

let command ~attach ~session_id =
  match attach with
  | [] ->
      Error
        "attach: no attach command configured - add attach: to \
         .jig/config.yaml (e.g. [claude, --resume, \"{session_id}\"])"
  | parts ->
      Ok
        (List.map
           (substitute ~placeholder:"{session_id}" ~value:session_id)
           parts)

let mentions_placeholder ~placeholder part =
  substitute ~placeholder ~value:"" part <> part

let substitute_session_id ~session_id parts =
  List.map (substitute ~placeholder:"{session_id}" ~value:session_id) parts

(* After a crash, an earlier step's session is the wrong one to reopen. *)
let session_of_run (run : Run.t) =
  match List.rev (Run.unresolved_interrupted_attempts run) with
  | attempt :: _ -> (
      match attempt.Run.session_id with
      | Some session_id -> Ok session_id
      | None ->
          Error
            (Printf.sprintf
               "attempt %s was interrupted and has no session id (put \
                {session_id} in the harness command to assign one); resume \
                it with jig run --resume %s, or --skip"
               attempt.Run.attempt_id run.Run.id))
  | [] -> (
      let newest_with_session =
        List.fold_left
          (fun found (step : Run.step_record) ->
            match step.Run.session_id with
            | Some _ as session -> session
            | None -> found)
          None run.Run.steps
      in
      match newest_with_session with
      | Some session_id -> Ok session_id
      | None ->
          Error
            (Printf.sprintf "no step of run %s recorded a session id"
               run.Run.id))
