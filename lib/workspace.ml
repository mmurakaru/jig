let ( let* ) = Result.bind

(* Worktrees live in a sibling directory of the repo: paths containing
   /.git/ are treated as git internals by vite, biome, and file-write
   guards, and an in-repo location would surface in IDE and watcher scans. *)
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

let remove ~root ~path =
  run_git ~root [ "worktree"; "remove"; "--force"; path ]
