open Result_syntax

type on_failure = Escalate | Abort

(* A step either runs an agent skill or a plain shell command. A command
   step carries no inputs - it interpolates {{ item.* }} directly in its
   command string inside a forEach. *)
type action = Skill_step of string | Command_step of string

type step = {
  action : action;
  on_fail : on_failure option;
  until_pass : bool;
  inputs : (string * string) list;
  (* The abstract cost tier this step runs on; config.yaml maps it to a
     concrete harness command. None defers to the skill's own tier, then
     to the default harness. *)
  tier : string option;
}

(* The human-facing name of a step: the skill name, or the command itself. *)
let step_label step =
  match step.action with Skill_step name -> name | Command_step cmd -> cmd

(* The skill a step resolves, if it is a skill step. *)
let step_skill step =
  match step.action with Skill_step name -> Some name | Command_step _ -> None

type retry = {
  max_iterations : int;
  on_exhausted : on_failure;
  retry_steps : step list;
}

type entry = Step of step | Retry of retry | For_each of for_each

(* Bounded fan-out over a checked-in data file: the body runs once per
   item, in order. The parser guarantees the body holds only Step/Retry. *)
and for_each = { items_file : string; var : string; body : entry list }

type t = { name : string; context : string option; entries : entry list }

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
   as pure data binding and `forEach` as bounded fan-out over data);
   unknown keys are rejected so typos and schema drift fail loudly. *)
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
      let base = [ "on_fail" ] @ if inside_retry then [ "until" ] else [] in
      let* () =
        check_no_unknown_keys ~context:"a step"
          ~allowed:([ "skill"; "command"; "with"; "tier" ] @ base)
          fields
      in
      (* Exactly one of skill / command. `with` (inputs) is skill-only; a
         command step interpolates its item bindings directly. *)
      let* action =
        match (List.assoc_opt "skill" fields, List.assoc_opt "command" fields) with
        | Some (`String skill), None -> Ok (Skill_step skill)
        | None, Some (`String command) ->
            if List.mem_assoc "with" fields then
              Error "workflow: with belongs to a skill step, not a command"
            else if List.mem_assoc "tier" fields then
              (* A command step runs no harness; it has nothing to tier. *)
              Error "workflow: tier belongs to a skill step, not a command"
            else Ok (Command_step command)
        | Some _, None -> Error "workflow: step skill must be a string"
        | None, Some _ -> Error "workflow: step command must be a string"
        | Some _, Some _ ->
            Error "workflow: a step sets either skill or command, not both"
        | None, None -> Error "workflow: step is missing required key: skill or command"
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
      let* tier =
        match List.assoc_opt "tier" fields with
        | None -> Ok None
        | Some (`String name) when is_valid_name name -> Ok (Some name)
        | Some (`String name) ->
            Error
              (Printf.sprintf
                 "workflow: step tier %S must contain only letters, digits, \
                  '.', '_' or '-'"
                 name)
        | Some _ -> Error "workflow: step tier must be a string"
      in
      Ok { action; on_fail; until_pass; inputs; tier }
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

(* {{ <var>.<column> }} - the only interpolation jig will ever do: pure
   textual substitution inside a forEach body's `with:` values. *)
let is_valid_placeholder_part text =
  text <> ""
  && String.for_all
       (fun character ->
         (character >= 'a' && character <= 'z')
         || (character >= 'A' && character <= 'Z')
         || (character >= '0' && character <= '9')
         || character = '-' || character = '_')
       text

let malformed_placeholder value =
  Error
    (Printf.sprintf
       "workflow: malformed placeholder in with value %S; expected {{ \
        <var>.<column> }}"
       value)

(* Scans one with: value; returns the referenced columns, or an error for a
   malformed placeholder or one that names anything but [var]. *)
let check_placeholders ~var value =
  let length = String.length value in
  let rec scan i columns =
    if i + 1 >= length then Ok (List.rev columns)
    else if value.[i] = '{' && value.[i + 1] = '{' then
      match String.index_from_opt value (i + 2) '}' with
      | Some close when close + 1 < length && value.[close + 1] = '}' -> (
          let inner = String.trim (String.sub value (i + 2) (close - i - 2)) in
          match String.index_opt inner '.' with
          | Some dot ->
              let name = String.sub inner 0 dot in
              let column =
                String.sub inner (dot + 1) (String.length inner - dot - 1)
              in
              if
                not
                  (is_valid_placeholder_part name
                  && is_valid_placeholder_part column)
              then malformed_placeholder value
              else if name <> var then
                Error
                  (Printf.sprintf
                     "workflow: with value references {{ %s.%s }} but \
                      forEach binds %S"
                     name column var)
              else scan (close + 2) (column :: columns)
          | None -> malformed_placeholder value)
      | _ -> malformed_placeholder value
    else scan (i + 1) columns
  in
  scan 0 []

(* Substitutes {{ var.column }} placeholders; parse-time checking makes a
   missing binding impossible by the time this runs. *)
let interpolate ~var ~bindings value =
  let length = String.length value in
  let buffer = Buffer.create length in
  let rec go i =
    if i >= length then ()
    else if i + 1 < length && value.[i] = '{' && value.[i + 1] = '{' then
      match String.index_from_opt value (i + 2) '}' with
      | Some close when close + 1 < length && value.[close + 1] = '}' -> (
          let inner = String.trim (String.sub value (i + 2) (close - i - 2)) in
          match String.index_opt inner '.' with
          | Some dot when String.sub inner 0 dot = var -> (
              let column =
                String.sub inner (dot + 1) (String.length inner - dot - 1)
              in
              match List.assoc_opt column bindings with
              | Some bound ->
                  Buffer.add_string buffer bound;
                  go (close + 2)
              | None ->
                  Buffer.add_char buffer value.[i];
                  go (i + 1))
          | _ ->
              Buffer.add_char buffer value.[i];
              go (i + 1))
      | _ ->
          Buffer.add_char buffer value.[i];
          go (i + 1)
    else (
      Buffer.add_char buffer value.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents buffer

let rec steps_of_entries entries =
  List.concat_map
    (function
      | Step step -> [ step ]
      | Retry retry -> retry.retry_steps
      | For_each for_each -> steps_of_entries for_each.body)
    entries

(* The interpolatable strings of a step: its with-value list, plus the
   command string for a command step (which interpolates directly). *)
let step_interpolated step =
  let input_values = List.map snd step.inputs in
  match step.action with
  | Skill_step _ -> input_values
  | Command_step command -> command :: input_values

(* The columns a forEach body actually references - what every item must
   bind. *)
let for_each_columns for_each =
  List.sort_uniq compare
    (List.concat_map
       (fun step ->
         List.concat_map
           (fun value ->
             match check_placeholders ~var:for_each.var value with
             | Ok columns -> columns
             | Error _ -> [])
           (step_interpolated step))
       (steps_of_entries for_each.body))

let validate_body_placeholders ~var body =
  List.fold_left
    (fun accumulator (step : step) ->
      let* () = accumulator in
      let* () =
        (* A skill name must stay static so validation can resolve it; a
           command, by contrast, is meant to interpolate. *)
        match step_skill step with
        | Some skill when String.index_opt skill '{' <> None ->
            Error "workflow: interpolation is only allowed in with values and commands"
        | _ -> Ok ()
      in
      List.fold_left
        (fun accumulator value ->
          let* () = accumulator in
          let* _columns = check_placeholders ~var value in
          Ok ())
        (Ok ()) (step_interpolated step))
    (Ok ()) (steps_of_entries body)

let rec for_each_of_yaml yaml =
  match yaml with
  | `O fields ->
      let* () =
        check_no_unknown_keys ~context:"a forEach block"
          ~allowed:[ "items"; "as"; "steps" ] fields
      in
      let* items_file =
        match List.assoc_opt "items" fields with
        | Some (`String path)
          when Filename.check_suffix path ".tsv"
               || Filename.check_suffix path ".json" ->
            Ok path
        | Some _ ->
            Error
              "workflow: forEach items must be a string path ending in .tsv \
               or .json"
        | None -> Error "workflow: forEach block is missing required key: items"
      in
      let* var =
        match List.assoc_opt "as" fields with
        | Some (`String name) when is_valid_placeholder_part name -> Ok name
        | Some (`String name) ->
            Error
              (Printf.sprintf
                 "workflow: forEach as %S must contain only letters, digits, \
                  '_' or '-'"
                 name)
        | Some _ -> Error "workflow: forEach as must be a string"
        | None -> Error "workflow: forEach block is missing required key: as"
      in
      let* body =
        match List.assoc_opt "steps" fields with
        | Some (`A entries) -> (
            let* body =
              traverse
                (fun entry ->
                  match entry with
                  | `O entry_fields when List.mem_assoc "forEach" entry_fields
                    ->
                      Error "workflow: forEach blocks cannot nest"
                  | entry -> body_entry_of_yaml entry)
                entries
            in
            match body with
            | [] -> Error "workflow: forEach steps must not be empty"
            | body -> Ok body)
        | Some _ -> Error "workflow: forEach steps must be a list"
        | None ->
            Error "workflow: forEach block is missing required key: steps"
      in
      let* () = validate_body_placeholders ~var body in
      Ok { items_file; var; body }
  | _ -> Error "workflow: a forEach block must be a mapping"

and body_entry_of_yaml yaml =
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

let entry_of_yaml yaml =
  match yaml with
  | `O fields when List.mem_assoc "forEach" fields ->
      let* () =
        check_no_unknown_keys ~context:"a forEach entry"
          ~allowed:[ "forEach" ] fields
      in
      let* for_each = for_each_of_yaml (List.assoc "forEach" fields) in
      Ok (For_each for_each)
  | yaml -> body_entry_of_yaml yaml

let of_yaml yaml =
  match yaml with
  | `O fields ->
      let* () =
        check_no_unknown_keys ~context:"the workflow"
          ~allowed:[ "name"; "context"; "steps" ] fields
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
      (* Constant framing for the whole workflow (recorded decision: data,
         not logic) - rendered into every step's prompt, never threaded. *)
      let* context =
        match List.assoc_opt "context" fields with
        | None -> Ok None
        | Some (`String text) -> Ok (Some text)
        | Some _ -> Error "workflow: context must be a string"
      in
      let* entries =
        match List.assoc_opt "steps" fields with
        | Some (`A yaml_entries) -> traverse entry_of_yaml yaml_entries
        | Some _ -> Error "workflow: steps must be a list"
        | None -> Error "workflow: missing required key: steps"
      in
      if entries = [] then Error "workflow: steps must not be empty"
      else Ok { name; context; entries }
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
  let* workflow = of_string content in
  (* A sibling AGENTS.md (convention-based auto-discovery, like Terraform's
     terraform.tfvars) supplies the workflow's context. It is one source of
     truth: pairing it with an inline `context:` is an error. *)
  let agents_path = Filename.concat (Filename.dirname path) "AGENTS.md" in
  if Sys.file_exists agents_path && not (Sys.is_directory agents_path) then
    match workflow.context with
    | Some _ ->
        Error
          "workflow: context is set both inline and via AGENTS.md - use one"
    | None ->
        let* agents =
          Result.map_error
            (fun message -> "workflow: " ^ message)
            (File.read agents_path)
        in
        Ok { workflow with context = Some agents }
  else Ok workflow

let referenced_skills workflow =
  List.filter_map step_skill (steps_of_entries workflow.entries)

let referenced_tiers workflow =
  List.sort_uniq compare
    (List.filter_map
       (fun step -> step.tier)
       (steps_of_entries workflow.entries))
