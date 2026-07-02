open Result_syntax

type status = Pass | Fail | Escalate

type t = { status : status; artifacts : string list; summary : string }

let status_of_string = function
  | "pass" -> Ok Pass
  | "fail" -> Ok Fail
  | "escalate" -> Ok Escalate
  | other ->
      Error
        (Printf.sprintf
           "handoff: unknown status %S (expected pass, fail or escalate)" other)

let string_of_status = function
  | Pass -> "pass"
  | Fail -> "fail"
  | Escalate -> "escalate"

let fence_opening = "```handoff"
let fence_closing = "```"

(* The handoff is the LAST ```handoff fenced block in the step's output, so
   prose or quoted examples earlier in the output cannot shadow it. *)
let extract_block output =
  let lines = String.split_on_char '\n' output in
  let _, _, last_complete_block =
    List.fold_left
      (fun (inside, current, last_found) line ->
        let trimmed = String.trim line in
        if inside then
          if trimmed = fence_closing then
            (false, [], Some (String.concat "\n" (List.rev current)))
          else (true, line :: current, last_found)
        else if trimmed = fence_opening then (true, [], last_found)
        else (false, [], last_found))
      (false, [], None) lines
  in
  last_complete_block

let of_yaml yaml =
  match yaml with
  | `O fields ->
      let* status =
        match List.assoc_opt "status" fields with
        | Some (`String value) -> status_of_string value
        | Some _ -> Error "handoff: status must be a string"
        | None -> Error "handoff: missing required key: status"
      in
      let* artifacts =
        match List.assoc_opt "artifacts" fields with
        | None -> Ok []
        | Some (`A entries) ->
            let paths =
              List.filter_map
                (function `String path -> Some path | _ -> None)
                entries
            in
            if List.length paths <> List.length entries then
              Error "handoff: artifacts must be a list of paths"
            else Ok paths
        | Some _ -> Error "handoff: artifacts must be a list of paths"
      in
      let summary =
        match List.assoc_opt "summary" fields with
        | Some (`String value) -> value
        | _ -> ""
      in
      Ok { status; artifacts; summary }
  | _ -> Error "handoff: expected a mapping inside the handoff block"

(* Structured harnesses (e.g. claude -p --output-format json) wrap the
   agent's text in a JSON envelope; the handoff block then lives inside the
   "result" string. Raw output is tried first. *)
let agent_text output =
  match extract_block output with
  | Some _ -> output
  | None -> (
      match Yojson.Safe.from_string output with
      | exception _ -> output
      | json -> (
          match Yojson.Safe.Util.member "result" json with
          | `String result -> result
          | _ -> output))

let parse output =
  match extract_block (agent_text output) with
  | None -> Error "handoff: no ```handoff block found in step output"
  | Some block -> (
      match Yaml.of_string block with
      | Ok yaml -> of_yaml yaml
      | Error (`Msg message) ->
          Error (Printf.sprintf "handoff: invalid yaml: %s" message))

let render handoff =
  let artifacts =
    match handoff.artifacts with
    | [] -> ""
    | paths ->
        Printf.sprintf "artifacts:\n%s"
          (String.concat ""
             (List.map (fun path -> Printf.sprintf "  - %s\n" path) paths))
  in
  Printf.sprintf "status: %s\n%s%s"
    (string_of_status handoff.status)
    artifacts handoff.summary

let of_json json =
  let* status =
    match Yojson.Safe.Util.member "status" json with
    | `String value -> status_of_string value
    | _ -> Error "handoff: missing or non-string status in run record"
  in
  let artifacts =
    match Yojson.Safe.Util.member "artifacts" json with
    | `List entries ->
        List.filter_map
          (function `String path -> Some path | _ -> None)
          entries
    | _ -> []
  in
  let summary =
    match Yojson.Safe.Util.member "summary" json with
    | `String value -> value
    | _ -> ""
  in
  Ok { status; artifacts; summary }

let to_json handoff =
  `Assoc
    [
      ("status", `String (string_of_status handoff.status));
      ("artifacts", `List (List.map (fun path -> `String path) handoff.artifacts));
      ("summary", `String handoff.summary);
    ]
