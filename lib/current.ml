(* The active step's live-output pointer: runs/<run-id>.current names the
   running step and its captured-output files while its subprocess runs.
   The runner writes it at spawn and removes it when the step finishes, so
   its presence means "a step is executing right now" - a watcher tails the
   named paths for the Log box and drops the box when the file disappears.
   Everything is best-effort: the pointer is advisory display state, and a
   failure to write or remove it must never affect the run. *)

type t = {
  skill : string;
  item_key : string option;
  stdout_path : string;
  stderr_path : string;
  started_at : string;
}

let path ~runs_dir ~run_id = Filename.concat runs_dir (run_id ^ ".current")

let to_json pointer =
  `Assoc
    ([ ("skill", `String pointer.skill) ]
    @ (match pointer.item_key with
      | Some key -> [ ("item_key", `String key) ]
      | None -> [])
    @ [
        ("stdout_path", `String pointer.stdout_path);
        ("stderr_path", `String pointer.stderr_path);
        ("started_at", `String pointer.started_at);
      ])

let of_json json =
  let string_member key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Some value
    | _ -> None
  in
  match
    ( string_member "skill",
      string_member "stdout_path",
      string_member "stderr_path",
      string_member "started_at" )
  with
  | Some skill, Some stdout_path, Some stderr_path, Some started_at ->
      Some
        {
          skill;
          item_key = string_member "item_key";
          stdout_path;
          stderr_path;
          started_at;
        }
  | _ -> None

(* Temp-then-rename, so a reader never sees a half-written pointer. *)
let write ~runs_dir ~run_id pointer =
  let destination = path ~runs_dir ~run_id in
  let temporary = destination ^ ".tmp" in
  try
    Out_channel.with_open_text temporary (fun channel ->
        Out_channel.output_string channel
          (Yojson.Safe.to_string (to_json pointer)));
    Sys.rename temporary destination
  with Sys_error _ -> ()

(* None when absent or mid-write - the watcher just tries again next tick. *)
let read ~runs_dir ~run_id =
  let file = path ~runs_dir ~run_id in
  if not (Sys.file_exists file) then None
  else
    match
      Yojson.Safe.from_string
        (In_channel.with_open_text file In_channel.input_all)
    with
    | json -> of_json json
    | exception _ -> None

let remove ~runs_dir ~run_id =
  try Sys.remove (path ~runs_dir ~run_id) with Sys_error _ -> ()
