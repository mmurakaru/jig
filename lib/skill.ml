(* Skills resolve repo-first: .jig/skills/ always wins, then each configured
   skill_paths directory in order - a repo can shadow an external skill by
   name, and omitting skill_paths keeps resolution repo-local. *)
let search_directories ~jig_dir ~skill_paths =
  Filename.concat jig_dir "skills" :: skill_paths

let candidate ~directory ~name =
  Filename.concat (Filename.concat directory name) "SKILL.md"

let resolve_path ~jig_dir ~skill_paths ~name =
  let directories = search_directories ~jig_dir ~skill_paths in
  match
    List.find_opt
      (fun directory -> Sys.file_exists (candidate ~directory ~name))
      directories
  with
  | Some directory -> Ok (candidate ~directory ~name)
  | None ->
      Error
        (Printf.sprintf "skill: %s not found; searched %s" name
           (String.concat ", " directories))

(* What a resume does with an attempt of this skill that a crash cut off. *)
type on_interrupt = Rerun | Recover | Escalate

let string_of_on_interrupt = function
  | Rerun -> "rerun"
  | Recover -> "recover"
  | Escalate -> "escalate"

let on_interrupt_of_string = function
  | "rerun" -> Ok Rerun
  | "recover" -> Ok Recover
  | "escalate" -> Ok Escalate
  | other ->
      Error
        (Printf.sprintf
           "on_interrupt must be rerun, recover, or escalate; got %S" other)

(* What the runner needs from a skill file: the instructions to put in the
   prompt, and the skill's own default tier. *)
type t = { body : string; tier : string option; on_interrupt : on_interrupt }

(* YAML frontmatter is runner metadata, not instructions - the body the
   agent sees excludes it. A file without a leading `---` line has no
   frontmatter and is passed byte-for-byte, so existing skills are
   untouched. Only `tier:` and `on_interrupt:` are read; other keys (name,
   description) belong to the harness ecosystems skills are shared with and
   are ignored. *)
let split_frontmatter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | first :: rest when String.trim first = "---" -> (
      let rec collect seen = function
        | [] -> None
        | line :: remaining when String.trim line = "---" ->
            Some (List.rev seen, remaining)
        | line :: remaining -> collect (line :: seen) remaining
      in
      match collect [] rest with
      | None -> (content, [])
      | Some (frontmatter, body) ->
          (String.concat "\n" body, frontmatter))
  | _ -> (content, [])

let frontmatter_value lines ~key =
  List.find_map
    (fun line ->
      match String.index_opt line ':' with
      | Some colon when String.trim (String.sub line 0 colon) = key ->
          let value =
            String.trim
              (String.sub line (colon + 1) (String.length line - colon - 1))
          in
          if value = "" then None else Some value
      | _ -> None)
    lines

let parse content =
  let body, frontmatter = split_frontmatter content in
  let on_interrupt =
    match frontmatter_value frontmatter ~key:"on_interrupt" with
    | None -> Ok Recover
    | Some value -> on_interrupt_of_string value
  in
  Result.map
    (fun on_interrupt ->
      { body; tier = frontmatter_value frontmatter ~key:"tier"; on_interrupt })
    on_interrupt

let load_path path =
  Result.bind
    (Result.map_error (fun message -> "skill: " ^ message) (File.read path))
    (fun content ->
      Result.map_error
        (fun message -> Printf.sprintf "skill: %s: %s" path message)
        (parse content))

let load ~jig_dir ~skill_paths ~name =
  Result.bind (resolve_path ~jig_dir ~skill_paths ~name) load_path
