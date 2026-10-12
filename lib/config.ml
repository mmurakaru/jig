open Result_syntax

type t = {
  harness : string list;
  wrapper : string list;
  skill_paths : string list;
  attach : string list;
  attach_headless : string list;
  notify : string list;
  (* The YAML key is `continue`, an OCaml keyword. *)
  continue_session : string list;
  probe : string list;
  (* Named alternates to the default harness, for steps that declare a
     tier - the map from an abstract cost tier to a concrete command. *)
  tiers : (string * string list) list;
  (* How many forEach items run at once; above 1 needs an isolated run. *)
  concurrency : int;
  timeout : Step_timeout.config;
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

(* tiers: a mapping of tier name -> harness command. A tier command
   replaces `harness` for the steps that declare that tier; the wrapper
   still applies. *)
let tiers_field fields =
  match List.assoc_opt "tiers" fields with
  | None -> Ok []
  | Some (`O pairs) ->
      List.fold_right
        (fun (name, value) accumulator ->
          let* tiers = accumulator in
          match value with
          | `A entries ->
              let strings =
                List.filter_map
                  (function `String value -> Some value | _ -> None)
                  entries
              in
              if List.length strings <> List.length entries || strings = []
              then
                Error
                  (Printf.sprintf
                     "config: tier %s must be a non-empty list of strings" name)
              else Ok ((name, strings) :: tiers)
          | _ ->
              Error
                (Printf.sprintf
                   "config: tier %s must be a non-empty list of strings" name))
        pairs (Ok [])
  | Some _ -> Error "config: tiers must be a mapping of names to commands"

(* YAML numbers parse as floats, so a whole float is the integer form. *)
let concurrency_field fields =
  match List.assoc_opt "concurrency" fields with
  | None -> Ok 1
  | Some (`Float value)
    when Float.is_integer value && value >= 1.0 && value <= 256.0 ->
      Ok (int_of_float value)
  | Some _ -> Error "config: concurrency must be a whole number from 1 to 256"

let of_yaml yaml =
  match yaml with
  | `O fields -> (
      let* harness = string_list_field fields ~key:"harness" in
      let* tiers = tiers_field fields in
      let* wrapper = string_list_field fields ~key:"wrapper" in
      let* skill_paths = string_list_field fields ~key:"skill_paths" in
      let* attach = string_list_field fields ~key:"attach" in
      let* attach_headless = string_list_field fields ~key:"attach_headless" in
      let* notify = string_list_field fields ~key:"notify" in
      let* continue_session = string_list_field fields ~key:"continue" in
      let* probe = string_list_field fields ~key:"probe" in
      let* concurrency = concurrency_field fields in
      let* timeout =
        match List.assoc_opt "timeout" fields with
        | None -> Ok Step_timeout.default_config
        | Some value -> Step_timeout.of_yaml value
      in
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
              notify = Option.value notify ~default:[];
              continue_session = Option.value continue_session ~default:[];
              probe = Option.value probe ~default:[];
              tiers;
              concurrency;
              timeout;
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

(* Validation warns about tiers when a valid config exists, and stays quiet
   when there is none to check against. *)
let load_if_valid ~jig_dir =
  if not (Sys.file_exists (Filename.concat jig_dir "config.yaml")) then None
  else Result.to_option (load ~jig_dir)


