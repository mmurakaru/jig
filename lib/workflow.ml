open Result_syntax

type step = { skill : string }
type t = { name : string; steps : step list }

let is_valid_name name =
  name <> ""
  && String.for_all
       (fun character ->
         (character >= 'a' && character <= 'z')
         || (character >= 'A' && character <= 'Z')
         || (character >= '0' && character <= '9')
         || character = '-' || character = '_' || character = '.')
       name

let step_of_yaml yaml =
  match yaml with
  | `O fields -> (
      match List.assoc_opt "skill" fields with
      | Some (`String skill) -> Ok { skill }
      | Some _ -> Error "workflow: step skill must be a string"
      | None -> Error "workflow: step is missing required key: skill")
  | _ -> Error "workflow: each step must be a mapping"

let steps_of_yaml entries =
  List.fold_right
    (fun entry accumulator ->
      let* steps = accumulator in
      let* step = step_of_yaml entry in
      Ok (step :: steps))
    entries (Ok [])

let of_yaml yaml =
  match yaml with
  | `O fields ->
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
      let* steps =
        match List.assoc_opt "steps" fields with
        | Some (`A entries) -> steps_of_yaml entries
        | Some _ -> Error "workflow: steps must be a list"
        | None -> Error "workflow: missing required key: steps"
      in
      if steps = [] then Error "workflow: steps must not be empty"
      else Ok { name; steps }
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
