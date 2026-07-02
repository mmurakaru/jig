let ( let* ) = Result.bind

(* Worktrees live under .git/jig-worktrees/ - inside the git dir they can
   never show up as untracked files in the parent checkout. *)
let worktree_path ~root ~run_id =
  Filename.concat (Filename.concat (Filename.concat root ".git") "jig-worktrees") run_id

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
