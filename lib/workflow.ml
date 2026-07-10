open Result_syntax

type on_failure = Escalate | Abort

type step = {
  skill : string;
  on_fail : on_failure option;
  until_pass : bool;
  inputs : (string * string) list;
}

type retry = {
  max_iterations : int;
  on_exhausted : on_failure;
  retry_steps : step list;
}

type entry = Step of step | Retry of retry
type t = { name : string; entries : entry list }

let is_valid_name name =
  name <> ""
  && String.for_all
       (fun character ->
         (character >= 'a' && character <= 'z')
         || (character >= 'A' && character <= 'Z')
         || (character >= '0' && character <= '9')
         || character = '-' || character = '_' || character = '.')
       name

let on_failure_of_string ~context = function
  | "escalate" -> Ok Escalate
  | "abort" -> Ok Abort
  | other ->
      Error
        (Printf.sprintf
           "workflow: %s must be \"escalate\" or \"abort\", got %S" context
           other)

(* The schema is frozen (recorded decision: on_fail + retry, plus `with`
   as pure data binding); unknown keys are rejected so typos and schema
   drift fail loudly. *)
let check_no_unknown_keys ~context ~allowed fields =
  match
    List.find_opt (fun (key, _) -> not (List.mem key allowed)) fields
  with
  | Some (key, _) ->
      Error
        (Printf.sprintf "workflow: unknown key %S in %s (allowed: %s)" key
           context
           (String.concat ", " allowed))
  | None -> Ok ()

let traverse parse entries =
  List.fold_right
    (fun entry accumulator ->
      let* parsed = accumulator in
      let* value = parse entry in
      Ok (value :: parsed))
    entries (Ok [])

(* `with` binds literal strings into the step prompt (recorded decision:
   data binding, not logic) - jig never evaluates, inlines, or resolves
   the values. Order is preserved for rendering. *)
let inputs_of_yaml fields =
  match List.assoc_opt "with" fields with
  | None -> Ok []
  | Some (`O pairs) ->
      let* () =
        match
          List.find_opt
            (fun (key, _) ->
              List.length (List.filter (fun (other, _) -> other = key) pairs)
              > 1)
            pairs
        with
        | Some (key, _) ->
            Error (Printf.sprintf "workflow: duplicate with key %S" key)
        | None -> Ok ()
      in
      traverse
        (fun (key, value) ->
          match value with
          | `String text -> Ok (key, text)
          | _ ->
              Error
                (Printf.sprintf
                   "workflow: with value for %S must be a string (quote \
                    numbers and booleans)"
                   key))
        pairs
  | Some _ -> Error "workflow: with must be a mapping of strings to strings"

let step_of_yaml ~inside_retry yaml =
  match yaml with
  | `O fields ->
      let* () =
        check_no_unknown_keys ~context:"a step"
          ~allowed:
            (if inside_retry then [ "skill"; "on_fail"; "until"; "with" ]
             else [ "skill"; "on_fail"; "with" ])
          fields
      in
      let* skill =
        match List.assoc_opt "skill" fields with
        | Some (`String skill) -> Ok skill
        | Some _ -> Error "workflow: step skill must be a string"
        | None -> Error "workflow: step is missing required key: skill"
      in
      let* on_fail =
        match List.assoc_opt "on_fail" fields with
        | None -> Ok None
        | Some (`String value) ->
            let* on_failure =
              on_failure_of_string ~context:"step on_fail" value
            in
            Ok (Some on_failure)
        | Some _ -> Error "workflow: step on_fail must be a string"
      in
      let* until_pass =
        match List.assoc_opt "until" fields with
        | None -> Ok false
        | Some (`String "pass") -> Ok true
        | Some _ ->
            Error "workflow: until only supports the value \"pass\""
      in
      let* inputs = inputs_of_yaml fields in
      Ok { skill; on_fail; until_pass; inputs }
  | _ -> Error "workflow: each step must be a mapping"

let retry_of_yaml yaml =
  match yaml with
  | `O fields ->
      let* () =
        check_no_unknown_keys ~context:"a retry block"
          ~allowed:[ "max_iterations"; "on_exhausted"; "steps" ]
          fields
      in
      let* max_iterations =
        match List.assoc_opt "max_iterations" fields with
        | Some (`Float value) when Float.is_integer value && value >= 1.0 ->
            Ok (int_of_float value)
        | Some _ ->
            Error
              "workflow: retry max_iterations must be a positive integer"
        | None ->
            Error
              "workflow: retry block is missing required key: max_iterations"
      in
      let* on_exhausted =
        match List.assoc_opt "on_exhausted" fields with
        | Some (`String value) ->
            on_failure_of_string ~context:"retry on_exhausted" value
        | Some _ -> Error "workflow: retry on_exhausted must be a string"
        | None ->
            Error
              "workflow: retry block is missing required key: on_exhausted"
      in
      let* retry_steps =
        match List.assoc_opt "steps" fields with
        | Some (`A entries) -> (
            let* steps =
              traverse
                (fun entry ->
                  match entry with
                  | `O entry_fields
                    when List.mem_assoc "retry" entry_fields ->
                      Error "workflow: retry blocks cannot nest"
                  | _ -> step_of_yaml ~inside_retry:true entry)
                entries
            in
            match steps with
            | [] -> Error "workflow: retry steps must not be empty"
            | steps -> Ok steps)
        | Some _ -> Error "workflow: retry steps must be a list"
        | None -> Error "workflow: retry block is missing required key: steps"
      in
      Ok { max_iterations; on_exhausted; retry_steps }
  | _ -> Error "workflow: a retry block must be a mapping"

let entry_of_yaml yaml =
  match yaml with
  | `O fields when List.mem_assoc "retry" fields ->
      let* () =
        check_no_unknown_keys ~context:"a retry entry" ~allowed:[ "retry" ]
          fields
      in
      let* retry = retry_of_yaml (List.assoc "retry" fields) in
      Ok (Retry retry)
  | yaml ->
      let* step = step_of_yaml ~inside_retry:false yaml in
      Ok (Step step)

let of_yaml yaml =
  match yaml with
  | `O fields ->
      let* () =
        check_no_unknown_keys ~context:"the workflow"
          ~allowed:[ "name"; "steps" ] fields
      in
      let* name =
        match List.assoc_opt "name" fields with
        | Some (`String name) -> Ok name
        | Some _ -> Error "workflow: name must be a string"
        | None -> Error "workflow: missing required key: name"
      in
      let* () =
        if is_valid_name name then Ok ()
        else
          Error
            (Printf.sprintf
               "workflow: name %S must contain only letters, digits, '.', '_' \
                or '-'"
               name)
      in
      let* entries =
        match List.assoc_opt "steps" fields with
        | Some (`A yaml_entries) -> traverse entry_of_yaml yaml_entries
        | Some _ -> Error "workflow: steps must be a list"
        | None -> Error "workflow: missing required key: steps"
      in
      if entries = [] then Error "workflow: steps must not be empty"
      else Ok { name; entries }
  | _ -> Error "workflow: expected a mapping at the top level"

let of_string content =
  match Yaml.of_string content with
  | Ok yaml -> of_yaml yaml
  | Error (`Msg message) ->
      Error (Printf.sprintf "workflow: invalid yaml: %s" message)

let load ~path =
  let* content =
    Result.map_error (fun message -> "workflow: " ^ message) (File.read path)
  in
  of_string content

let referenced_skills workflow =
  List.concat_map
    (function
      | Step step -> [ step.skill ]
      | Retry retry -> List.map (fun step -> step.skill) retry.retry_steps)
    workflow.entries
