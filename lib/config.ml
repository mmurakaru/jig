open Result_syntax

type t = { harness : string list }

let of_yaml yaml =
  match yaml with
  | `O fields -> (
      match List.assoc_opt "harness" fields with
      | Some (`A entries) ->
          let strings =
            List.filter_map
              (function `String value -> Some value | _ -> None)
              entries
          in
          if List.length strings <> List.length entries then
            Error "config: harness must be a list of strings"
          else if strings = [] then Error "config: harness must not be empty"
          else Ok { harness = strings }
      | Some _ -> Error "config: harness must be a list of strings"
      | None -> Error "config: missing required key: harness")
  | _ -> Error "config: expected a mapping at the top level"

let of_string content =
  match Yaml.of_string content with
  | Ok yaml -> of_yaml yaml
  | Error (`Msg message) ->
      Error (Printf.sprintf "config: invalid yaml: %s" message)

let load ~jig_dir =
  let path = Filename.concat jig_dir "config.yaml" in
  let* content =
    Result.map_error (fun message -> "config: " ^ message) (File.read path)
  in
  of_string content
