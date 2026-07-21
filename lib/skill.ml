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

(* What the runner needs from a skill file: the instructions to put in the
   prompt, and the skill's own default tier. *)
type t = { body : string; tier : string option }

(* YAML frontmatter is runner metadata, not instructions - the body the
   agent sees excludes it. A file without a leading `---` line has no
   frontmatter and is passed byte-for-byte, so existing skills are
   untouched. Only `tier:` is read; other keys (name, description) belong
   to the harness ecosystems skills are shared with and are ignored. *)
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

let tier_of_frontmatter lines =
  List.find_map
    (fun line ->
      match String.index_opt line ':' with
      | Some colon when String.trim (String.sub line 0 colon) = "tier" ->
          let value =
            String.trim
              (String.sub line (colon + 1) (String.length line - colon - 1))
          in
          if value = "" then None else Some value
      | _ -> None)
    lines

let parse content =
  let body, frontmatter = split_frontmatter content in
  { body; tier = tier_of_frontmatter frontmatter }

let load ~jig_dir ~skill_paths ~name =
  Result.bind (resolve_path ~jig_dir ~skill_paths ~name) (fun path ->
      Result.map
        (fun content -> parse content)
        (Result.map_error (fun message -> "skill: " ^ message) (File.read path)))
