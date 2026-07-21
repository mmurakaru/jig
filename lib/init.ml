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
   \  - json\n\
   \n\
   # Steps that declare `tier: mechanical` (the starter workflows tier\n\
   # their test, PR, and evidence steps) run on the cheaper model below;\n\
   # judgment steps stay on the default harness above. The model name is\n\
   # the full slug - `haiku` is not a CLI alias.\n\
   tiers:\n\
   \  mechanical:\n\
   \    - claude\n\
   \    - -p\n\
   \    - --model\n\
   \    - claude-haiku-4-5-20251001\n\
   \    - --allowedTools\n\
   \    - \"Bash(git:*),Bash(gh:*)\"\n\
   \    - --disallowedTools\n\
   \    - \"EnterWorktree,ExitWorktree\"\n\
   \    - --permission-mode\n\
   \    - acceptEdits\n\
   \    - --output-format\n\
   \    - json\n\
   \n\
   # jig attach <run-id> reopens a step's recorded session interactively;\n\
   # {session_id} is replaced with the id from the step record.\n\
   attach:\n\
   \  - claude\n\
   \  - --resume\n\
   \  - \"{session_id}\"\n\
   \n\
   # The headless twin: after the chat ends on a paused run, jig resumes\n\
   # the same session with this command to collect its handoff and\n\
   # continue the run.\n\
   attach_headless:\n\
   \  - claude\n\
   \  - -p\n\
   \  - --resume\n\
   \  - \"{session_id}\"\n\
   \  - --output-format\n\
   \  - json\n"

let codex_config =
  "# Verified against the codex CLI docs at preset creation. --json streams\n\
   # JSONL events; widen --sandbox only if your skills need it.\n\
   # `codex resume <session-id>` reopens sessions; set attach: once your\n\
   # codex version's --json output records a session id in jig's step\n\
   # records.\n\
   harness:\n\
   \  - codex\n\
   \  - exec\n\
   \  - --sandbox\n\
   \  - workspace-write\n\
   \  - --json\n\
   \n\
   # Steps that declare `tier: mechanical` (the starter workflows tier\n\
   # their test, PR, and evidence steps) run on the fast/affordable model;\n\
   # judgment steps stay on the default harness above.\n\
   tiers:\n\
   \  mechanical:\n\
   \    - codex\n\
   \    - exec\n\
   \    - --model\n\
   \    - gpt-5.6-luna\n\
   \    - --sandbox\n\
   \    - workspace-write\n\
   \    - --json\n"

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

(* Where the claude preset installs the harness skill, relative to root. *)
let harness_skill_dir =
  Filename.concat ".claude" (Filename.concat "skills" "jig")

let write_tree ~directory files =
  List.iter
    (fun (relative_path, content) ->
      let path = Filename.concat directory relative_path in
      ensure_directory (Filename.dirname path);
      Out_channel.with_open_text path (fun channel ->
          Out_channel.output_string channel content))
    files

(* Scaffold the embedded starter set into <root>/.jig - and, for the claude
   preset, the harness skill into <root>/.claude/skills/jig. Refuses to
   touch either tree if it exists - init bootstraps, it never merges or
   overwrites. Returns the written paths relative to root. *)
let scaffold ~root ~preset ~skill_paths =
  let jig_dir = Filename.concat root ".jig" in
  let skill_dir = Filename.concat root harness_skill_dir in
  let installs_harness_skill = preset = Claude in
  if Sys.file_exists jig_dir then
    Error
      (Printf.sprintf
         "%s already exists; jig init only bootstraps a repository that has \
          no .jig yet"
         jig_dir)
  else if installs_harness_skill && Sys.file_exists skill_dir then
    Error
      (Printf.sprintf
         "%s already exists; jig init only bootstraps a repository that has \
          no harness skill installed yet"
         skill_dir)
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
      write_tree ~directory:jig_dir files;
      if installs_harness_skill then
        write_tree ~directory:skill_dir Harness_skill_data.files;
      let written =
        List.map
          (fun (relative_path, _) -> Filename.concat ".jig" relative_path)
          files
        @
        if installs_harness_skill then
          List.map
            (fun (relative_path, _) ->
              Filename.concat harness_skill_dir relative_path)
            Harness_skill_data.files
        else []
      in
      Ok written
    with
    | Sys_error message -> Error (Printf.sprintf "init: %s" message)
    | Unix.Unix_error (error, _, _) ->
        Error (Printf.sprintf "init: %s" (Unix.error_message error)))
