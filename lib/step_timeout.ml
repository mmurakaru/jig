open Result_syntax

(** The wall-clock limit per step attempt from config.yaml timeout:; None is off. *)
type config = {
  skill_step_limit_seconds : int option;
  command_step_limit_seconds : int option;
  (* Between SIGTERM and SIGKILL when a step attempt times out. *)
  grace_seconds : int;
  tier_limit_seconds : (string * int option) list;
}

let default_config =
  {
    skill_step_limit_seconds = Some (60 * 60);
    command_step_limit_seconds = Some (30 * 60);
    grace_seconds = 10;
    tier_limit_seconds = [];
  }

let duration_error ~key =
  Printf.sprintf
    "config: timeout.%s must be a duration with a unit (s, m or h), such as \
     30m, or off"
    key

(** Parses a timeout duration such as 90s, 45m or 2h; a bare number is an error. *)
let parse_duration_seconds ~key text =
  let length = String.length text in
  let digits = if length > 1 then String.sub text 0 (length - 1) else "" in
  let all_digits =
    digits <> "" && String.for_all (fun character -> character >= '0' && character <= '9') digits
  in
  let multiplier =
    if length = 0 then None
    else
      match text.[length - 1] with
      | 's' -> Some 1
      | 'm' -> Some 60
      | 'h' -> Some 3600
      | _ -> None
  in
  match (all_digits, multiplier) with
  | true, Some multiplier -> (
      match int_of_string_opt digits with
      | Some value when value > 0 -> Ok (value * multiplier)
      | _ ->
          Error
            (Printf.sprintf "config: timeout.%s must be above zero; use off for no limit"
               key))
  | _ -> Error (duration_error ~key)

(* YAML 1.1 reads a bare off as false. *)
let limit_field ~key = function
  | `String "off" | `Bool false -> Ok None
  | `String text ->
      let* seconds = parse_duration_seconds ~key text in
      Ok (Some seconds)
  | _ -> Error (duration_error ~key)

(** Parses the `timeout:` block; absent keys keep their defaults. *)
let of_yaml = function
  | `O fields ->
      let* () =
        match
          List.find_opt
            (fun (key, _) ->
              not (List.mem key [ "default"; "command"; "grace"; "tiers" ]))
            fields
        with
        | Some (key, _) ->
            Error
              (Printf.sprintf
                 "config: unknown key timeout.%s (expected default, command, \
                  grace or tiers)"
                 key)
        | None -> Ok ()
      in
      let optional_limit ~key ~default =
        match List.assoc_opt key fields with
        | None -> Ok default
        | Some value -> limit_field ~key value
      in
      let* skill_step_limit_seconds =
        optional_limit ~key:"default"
          ~default:default_config.skill_step_limit_seconds
      in
      let* command_step_limit_seconds =
        optional_limit ~key:"command"
          ~default:default_config.command_step_limit_seconds
      in
      let* grace_seconds =
        match List.assoc_opt "grace" fields with
        | None -> Ok default_config.grace_seconds
        | Some (`String text) -> parse_duration_seconds ~key:"grace" text
        | Some _ ->
            Error
              "config: timeout.grace must be a duration with a unit (s, m or \
               h), such as 10s"
      in
      let* tier_limit_seconds =
        match List.assoc_opt "tiers" fields with
        | None -> Ok []
        | Some (`O pairs) ->
            List.fold_right
              (fun (name, value) accumulator ->
                let* limits = accumulator in
                let* limit = limit_field ~key:("tiers." ^ name) value in
                Ok ((name, limit) :: limits))
              pairs (Ok [])
        | Some _ ->
            Error "config: timeout.tiers must be a mapping of tier names to durations"
      in
      Ok
        {
          skill_step_limit_seconds;
          command_step_limit_seconds;
          grace_seconds;
          tier_limit_seconds;
        }
  | _ -> Error "config: timeout must be a mapping (default, command, grace, tiers)"

(** The limit of a skill step: its tier's limit, else the default one. *)
let skill_step_limit_seconds config ~tier =
  match tier with
  | Some tier when List.mem_assoc tier config.tier_limit_seconds ->
      List.assoc tier config.tier_limit_seconds
  | _ -> config.skill_step_limit_seconds

(** A timeout duration as config.yaml writes it: 90s, 45m or 2h. *)
let format_duration_seconds seconds =
  if seconds mod 3600 = 0 then Printf.sprintf "%dh" (seconds / 3600)
  else if seconds mod 60 = 0 then Printf.sprintf "%dm" (seconds / 60)
  else Printf.sprintf "%ds" seconds

(** Warnings for timeout.tiers names that the config's tiers: map does not have. *)
let unmapped_tier_warnings config ~mapped_tiers =
  List.filter_map
    (fun (name, _) ->
      if List.mem_assoc name mapped_tiers then None
      else
        Some
          (Printf.sprintf
             "timeout.tiers names tier %S, which config tiers does not map; \
              its limit applies only to steps that declare that tier"
             name))
    config.tier_limit_seconds
