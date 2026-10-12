(* The field guide: repo knowledge that agents accumulate across runs.
   `.jig/FIELDGUIDE.md` is read from the run's workspace before every skill
   step - an append mid-run reaches the very next step - and rendered into
   the prompt. The file's existence is the opt-in: no file, no injection
   and no instruction to write one. It is a plain repo file, so it is
   versioned, reviewable, and survives across runs; in an isolated run the
   worktree's copy is the one read and written, and its changes travel with
   the run's branch. *)

let relative_path = Filename.concat ".jig" "FIELDGUIDE.md"

(* None: not opted in. Some content: inject (content may be empty - the
   opt-in still enables the append instruction). *)
let load ~workspace =
  let path = Filename.concat workspace relative_path in
  if not (Sys.file_exists path) || Sys.is_directory path then None
  else match File.read path with Ok content -> Some content | Error _ -> None
