let ( let* ) = Result.bind

(* Worktrees live in a sibling directory of the repo: tooling commonly
   treats any path containing /.git/ as git internals, and an in-repo
   location would surface full checkouts to IDE and watcher scans. *)
let worktree_path ~root ~run_id =
  let container = Filename.basename root ^ "-worktrees" in
  Filename.concat (Filename.concat (Filename.dirname root) container) run_id

let run_git ~root arguments =
  let* outcome = Subprocess.run ~cwd:root ~argv:("git" :: arguments) () in
  if outcome.Subprocess.exit_code <> 0 then
    Error
      (Printf.sprintf "workspace: git %s failed: %s"
         (String.concat " " arguments)
         (String.trim outcome.Subprocess.stderr))
  else Ok ()

let create ~root ~run_id =
  let path = worktree_path ~root ~run_id in
  let* () = run_git ~root [ "worktree"; "add"; "--detach"; path ] in
  Ok path

(* Named like the item's step keys, so two forEach entries never collide. *)
let item_worktree_path ~root ~run_id ~entry_index ~item_index =
  worktree_path ~root
    ~run_id:(Printf.sprintf "%s-e%d-i%d" run_id entry_index item_index)

(* A detached worktree at the run workspace's HEAD commit, for one forEach
   item that runs in parallel with others. *)
let create_item_worktree ~root ~run_workspace ~path =
  run_git ~root:run_workspace [ "worktree"; "add"; "--detach"; path ]

let remove ~root ~path =
  run_git ~root [ "worktree"; "remove"; "--force"; path ]
