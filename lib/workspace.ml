let ( let* ) = Result.bind

(* Worktrees live in a sibling directory of the repo: tooling commonly
   treats any path containing /.git/ as git internals, and an in-repo
   location would surface full checkouts to IDE and watcher scans. *)
let worktree_path ~root ~run_id =
  let container = Filename.basename root ^ "-worktrees" in
  Filename.concat (Filename.concat (Filename.dirname root) container) run_id

let git_output ~root arguments =
  let* outcome = Subprocess.run ~cwd:root ~argv:("git" :: arguments) () in
  if outcome.Subprocess.exit_code <> 0 then
    Error
      (Printf.sprintf "workspace: git %s failed: %s" (String.concat " " arguments)
         (String.trim outcome.Subprocess.stderr))
  else Ok (String.trim outcome.Subprocess.stdout)

let run_git ~root arguments = Result.map ignore (git_output ~root arguments)

let create ~root ~run_id =
  let path = worktree_path ~root ~run_id in
  let* () = run_git ~root [ "worktree"; "add"; "--detach"; path ] in
  Ok path

(* Named like the item's step keys, so two forEach entries never collide. *)
let item_worktree_path ~root ~run_id ~entry_index ~item_index =
  worktree_path ~root ~run_id:(Printf.sprintf "%s-e%d-i%d" run_id entry_index item_index)

(* A detached worktree at the run workspace's HEAD commit, for one forEach
   item that runs in parallel with others. *)
let create_item_worktree ~root ~run_workspace ~path =
  run_git ~root:run_workspace [ "worktree"; "add"; "--detach"; path ]

let item_branch_name ~path = "jig/" ^ Filename.basename path

(* A detached worktree's commits are unreachable once it is removed. *)
let keep_item_commits ~run_workspace ~path =
  let* item_head = git_output ~root:path [ "rev-parse"; "HEAD" ] in
  let* run_head = git_output ~root:run_workspace [ "rev-parse"; "HEAD" ] in
  if item_head = run_head then Ok ()
  else run_git ~root:path [ "branch"; item_branch_name ~path; item_head ]

let remove ~root ~path = run_git ~root [ "worktree"; "remove"; "--force"; path ]
