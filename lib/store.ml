(* A record on disk that does not load, e.g. from another jig version. *)
type unreadable_record = { id : string; reason : string }

(* Unreadable records travel apart, so one bad record never hides the rest. *)
type run_listing = { runs : Run.t list; unreadable_records : unreadable_record list }

module type S = sig
  type run_lock

  (* An atomic full replacement: readers see the old or the new record. *)
  val save : runs_dir:string -> Run.t -> (string, string) result

  val load : runs_dir:string -> id:string -> (Run.t, string) result

  (* Runs newest-first by start time; empty when nothing has run. *)
  val list_runs : runs_dir:string -> (run_listing, string) result

  (* Exclusive per-run lock: Error while another live process drives it. *)
  val acquire_run_lock : runs_dir:string -> id:string -> (run_lock, string) result

  val release_run_lock : run_lock -> unit
end

module Filesystem : S = struct
  type lock_holder = { pid : int; host : string; started_at : string }

  type run_lock = { lock_path : string; holder : lock_holder }

  let ensure_directory path =
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let remove_quietly path = try Unix.unlink path with Unix.Unix_error _ -> ()

  let store_message message = "store: " ^ message

  let store_error error = store_message (Unix.error_message error)

  let record_path ~runs_dir ~id = Filename.concat runs_dir (id ^ ".json")

  let run_lock_path ~runs_dir ~id = Filename.concat runs_dir (id ^ ".lock")

  let write_file_durably ~path content =
    let descriptor =
      Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ] 0o644
    in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () ->
        let length = String.length content in
        let rec write_from offset =
          if offset < length then
            write_from (offset + Unix.write_substring descriptor content offset (length - offset))
        in
        write_from 0;
        Unix.fsync descriptor)

  (* Persists the rename itself, not only the file content. *)
  let fsync_directory path =
    let descriptor = Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () -> try Unix.fsync descriptor with Unix.Unix_error (Unix.EINVAL, _, _) -> ())

  let save ~runs_dir run =
    let content = Yojson.Safe.pretty_to_string (Run.to_json run) in
    let path = record_path ~runs_dir ~id:run.Run.id in
    (* Per-process name, so two writers never share a temp file. *)
    let temporary = Printf.sprintf "%s.tmp.%d" path (Unix.getpid ()) in
    try
      ensure_directory runs_dir;
      write_file_durably ~path:temporary content;
      Unix.rename temporary path;
      fsync_directory runs_dir;
      Ok path
    with Unix.Unix_error (error, _, _) ->
      remove_quietly temporary;
      Error (store_error error)

  let load ~runs_dir ~id =
    let path = record_path ~runs_dir ~id in
    Result.bind
      (Result.map_error store_message (File.read path))
      (fun content ->
        let parsed =
          match Yojson.Safe.from_string content with
          | json -> Run.of_json json
          | exception Yojson.Json_error message ->
              Error (Printf.sprintf "not valid json: %s" message)
        in
        Result.map_error
          (fun message ->
            Printf.sprintf
              "store: failed to read %s: %s (the record may have been written by a different jig \
               version)"
              path message)
          parsed)

  let compare_newest_first a b =
    (* Epochs order whole-second and millisecond records; ties go by id. *)
    let started run = Run.epoch_of_iso8601 run.Run.started_at in
    match compare (started b) (started a) with
    | 0 -> (
        match compare (String.length b.Run.id) (String.length a.Run.id) with
        | 0 -> String.compare b.Run.id a.Run.id
        | order -> order)
    | order -> order

  let list_runs ~runs_dir =
    if not (Sys.file_exists runs_dir) then Ok { runs = []; unreadable_records = [] }
    else
      match Sys.readdir runs_dir with
      | exception Sys_error message -> Error (store_message message)
      | files ->
          let ids =
            files |> Array.to_list |> List.sort String.compare
            |> List.filter_map (fun file ->
                if Filename.check_suffix file ".json" then Some (Filename.chop_suffix file ".json")
                else None)
          in
          let runs, unreadable_records =
            List.partition_map
              (fun id ->
                match load ~runs_dir ~id with
                | Ok run -> Either.Left run
                | Error reason -> Either.Right { id; reason })
              ids
          in
          Ok { runs = List.sort compare_newest_first runs; unreadable_records }

  let lock_holder_to_string holder =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("pid", `Int holder.pid);
           ("host", `String holder.host);
           ("started_at", `String holder.started_at);
         ])

  type lock_state = Lock_absent | Lock_held of lock_holder | Lock_unreadable of string

  let read_lock_state path =
    match In_channel.with_open_text path In_channel.input_all with
    | exception Sys_error _ when not (Sys.file_exists path) -> Lock_absent
    | exception Sys_error message -> Lock_unreadable message
    | content -> (
        match Yojson.Safe.from_string content with
        | exception Yojson.Json_error message -> Lock_unreadable message
        | `Assoc fields -> (
            match
              ( List.assoc_opt "pid" fields,
                List.assoc_opt "host" fields,
                List.assoc_opt "started_at" fields )
            with
            | Some (`Int pid), Some (`String host), Some (`String started_at) ->
                Lock_held { pid; host; started_at }
            | _ -> Lock_unreadable "missing pid, host, or started_at")
        | _ -> Lock_unreadable "not a JSON object")

  let process_is_alive pid =
    match Unix.kill pid 0 with
    | () -> true
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
    (* EPERM: the pid exists but belongs to another user. *)
    | exception Unix.Unix_error _ -> true

  (* A pid on another host cannot be checked. *)
  let lock_is_stale holder = holder.host = Unix.gethostname () && not (process_is_alive holder.pid)

  let live_lock_message ~id ~lock_path holder =
    if holder.host = Unix.gethostname () then
      Printf.sprintf
        "store: run %s is driven by live pid %d (since %s); wait for it to finish or stop that \
         process"
        id holder.pid holder.started_at
    else
      Printf.sprintf
        "store: run %s is driven by pid %d on host %s (since %s); remove %s only when that process \
         is gone"
        id holder.pid holder.host holder.started_at lock_path

  (* link(2) never exposes a lock file without its content, unlike O_EXCL. *)
  let rec claim_lock ~id ~lock_path ~candidate ~attempts_left =
    match Unix.link candidate lock_path with
    | () -> Ok ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> (
        if attempts_left = 0 then
          Error
            (Printf.sprintf "store: run %s: the lock %s keeps changing hands; try again" id
               lock_path)
        else
          let retry () = claim_lock ~id ~lock_path ~candidate ~attempts_left:(attempts_left - 1) in
          match read_lock_state lock_path with
          | Lock_absent -> retry ()
          | Lock_unreadable reason ->
              Error
                (Printf.sprintf
                   "store: run %s has an unreadable lock %s (%s); remove it only when no jig \
                    process drives the run"
                   id lock_path reason)
          | Lock_held holder when lock_is_stale holder ->
              remove_stale_lock ~lock_path ~candidate ~stale_holder:holder;
              retry ()
          | Lock_held holder -> Error (live_lock_message ~id ~lock_path holder))

  (* A takeover guard serializes removers, so none unlinks a fresh lock. *)
  and remove_stale_lock ~lock_path ~candidate ~stale_holder =
    let guard_path = lock_path ^ ".takeover" in
    match Unix.link candidate guard_path with
    | () ->
        (match read_lock_state lock_path with
        | Lock_held holder when holder = stale_holder -> remove_quietly lock_path
        | Lock_held _ | Lock_absent | Lock_unreadable _ -> ());
        remove_quietly guard_path
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> (
        match read_lock_state guard_path with
        | Lock_held guard_holder when lock_is_stale guard_holder -> remove_quietly guard_path
        | Lock_held _ | Lock_absent | Lock_unreadable _ -> ())

  (* Temp records of a killed writer; safe to remove while holding the lock. *)
  let remove_abandoned_temporary_records ~runs_dir ~id =
    let prefix = id ^ ".json.tmp." in
    Array.iter
      (fun file ->
        if String.starts_with ~prefix file then remove_quietly (Filename.concat runs_dir file))
      (Sys.readdir runs_dir)

  let acquire_run_lock ~runs_dir ~id =
    let lock_path = run_lock_path ~runs_dir ~id in
    let holder =
      {
        pid = Unix.getpid ();
        host = Unix.gethostname ();
        started_at = Run.iso8601 (Unix.gettimeofday ());
      }
    in
    let candidate = Printf.sprintf "%s.%d" lock_path holder.pid in
    try
      ensure_directory runs_dir;
      write_file_durably ~path:candidate (lock_holder_to_string holder);
      let claimed = claim_lock ~id ~lock_path ~candidate ~attempts_left:3 in
      remove_quietly candidate;
      Result.map
        (fun () ->
          (try
             fsync_directory runs_dir;
             remove_abandoned_temporary_records ~runs_dir ~id
           with Unix.Unix_error _ | Sys_error _ -> ());
          { lock_path; holder })
        claimed
    with
    | Unix.Unix_error (error, _, _) ->
        remove_quietly candidate;
        Error (store_error error)
    | Sys_error message ->
        remove_quietly candidate;
        Error (store_message message)

  (* Removes the lock only while it is still ours, never a successor's. *)
  let release_run_lock run_lock =
    match read_lock_state run_lock.lock_path with
    | Lock_held holder when holder = run_lock.holder -> remove_quietly run_lock.lock_path
    | Lock_held _ | Lock_absent | Lock_unreadable _ -> ()
end
