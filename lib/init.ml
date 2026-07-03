type harness_preset = Claude | Codex | Custom

(* Presets encode third-party CLI flags: verify against the harness docs
   whenever one is touched. jig appends the prompt as the final argument,
   so no preset may end with a flag that takes multiple values. *)
let claude_config =
  "# Verified against the claude CLI docs at preset creation. jig appends\n\
   # the step prompt as the FINAL argument - never end the list with a\n\
   # multi-value flag.\n\
   # git and gh are what the starter skills themselves use (committing,\n\
   # opening PRs, reading issues). Add your stack's tools to the same\n\
   # list, e.g. Bash(pnpm:*) or Bash(pytest:*) or Bash(cargo:*).\n\
   # jig owns workspace isolation: the harness's own worktree tools are\n\
   # denied so a step cannot relocate the run's work.\n\
   harness:\n\
   \  - claude\n\
   \  - -p\n\
   \  - --allowedTools\n\
   \  - \"Bash(git:*),Bash(gh:*)\"\n\
   \  - --disallowedTools\n\
   \  - \"EnterWorktree,ExitWorktree\"\n\
   \  - --permission-mode\n\
   \  - acceptEdits\n\
   \  - --output-format\n\
   \  - json\n"

let codex_config =
  "# Verified against the codex CLI docs at preset creation. --json streams\n\
   # JSONL events; widen --sandbox only if your skills need it.\n\
   harness:\n\
   \  - codex\n\
   \  - exec\n\
   \  - --sandbox\n\
   \  - workspace-write\n\
   \  - --json\n"

let config_content ~preset ~skill_paths =
  let base =
    match preset with
    | Custom -> List.assoc "config.yaml" Template_data.files
    | Claude -> claude_config
    | Codex -> codex_config
  in
  match skill_paths with
  | [] -> base
  | paths ->
      base ^ "\nskill_paths:\n"
      ^ String.concat ""
          (List.map (fun path -> Printf.sprintf "  - %S\n" path) paths)

let rec ensure_directory path =
  if not (Sys.file_exists path) then (
    ensure_directory (Filename.dirname path);
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

(* Scaffold the embedded starter set into <root>/.jig. Refuses to touch an
   existing .jig - init bootstraps, it never merges or overwrites. *)
let scaffold ~root ~preset ~skill_paths =
  let jig_dir = Filename.concat root ".jig" in
  if Sys.file_exists jig_dir then
    Error
      (Printf.sprintf
         "%s already exists; jig init only bootstraps a repository that has \
          no .jig yet"
         jig_dir)
  else (
    try
      let files =
        List.map
          (fun (relative_path, content) ->
            if relative_path = "config.yaml" then
              (relative_path, config_content ~preset ~skill_paths)
            else (relative_path, content))
          Template_data.files
      in
      List.iter
        (fun (relative_path, content) ->
          let path = Filename.concat jig_dir relative_path in
          ensure_directory (Filename.dirname path);
          Out_channel.with_open_text path (fun channel ->
              Out_channel.output_string channel content))
        files;
      Ok (List.map fst files)
    with
    | Sys_error message -> Error (Printf.sprintf "init: %s" message)
    | Unix.Unix_error (error, _, _) ->
        Error (Printf.sprintf "init: %s" (Unix.error_message error)))
