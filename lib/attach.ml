(* Resolves the command that reopens a step's harness session
   interactively. The template comes from config; every occurrence of
   the placeholder is replaced with the step's recorded session id. *)

let placeholder = "{session_id}"

let substitute ~session_id part =
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
            Buffer.add_string buffer session_id;
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
  | parts -> Ok (List.map (substitute ~session_id) parts)
