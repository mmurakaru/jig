open Result_syntax

type t = {
  harness : string list;
  wrapper : string list;
  skill_paths : string list;
  attach : string list;
  attach_headless : string list;
}

let expand_home path =
  match Sys.getenv_opt "HOME" with
  | None -> path
  | Some home ->
      if path = "~" then home
      else if String.length path >= 2 && String.sub path 0 2 = "~/" then
        Filename.concat home (String.sub path 2 (String.length path - 2))
      else path

let string_list_field fields ~key =
  match List.assoc_opt key fields with
  | None -> Ok None
  | Some (`A entries) ->
      let strings =
        List.filter_map
          (function `String value -> Some value | _ -> None)
          entries
      in
      if List.length strings <> List.length entries then
        Error (Printf.sprintf "config: %s must be a list of strings" key)
      else Ok (Some strings)
  | Some _ -> Error (Printf.sprintf "config: %s must be a list of strings" key)

let of_yaml yaml =
  match yaml with
  | `O fields -> (
      let* harness = string_list_field fields ~key:"harness" in
      let* wrapper = string_list_field fields ~key:"wrapper" in
      let* skill_paths = string_list_field fields ~key:"skill_paths" in
      let* attach = string_list_field fields ~key:"attach" in
      let* attach_headless = string_list_field fields ~key:"attach_headless" in
      match harness with
      | None -> Error "config: missing required key: harness"
      | Some [] -> Error "config: harness must not be empty"
      | Some harness ->
          Ok
            {
              harness;
              wrapper = Option.value wrapper ~default:[];
              skill_paths =
                List.map expand_home (Option.value skill_paths ~default:[]);
              attach = Option.value attach ~default:[];
              attach_headless = Option.value attach_headless ~default:[];
            })
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

(* Validation needs skill resolution but not a harness; a missing config is
   fine there (repo-local skills only), an invalid one is still an error. *)
let load_skill_paths ~jig_dir =
  if not (Sys.file_exists (Filename.concat jig_dir "config.yaml")) then Ok []
  else Result.map (fun config -> config.skill_paths) (load ~jig_dir)
