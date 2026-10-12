(* A record that is on disk but does not load: a different jig version, or
   a file a human edited by hand. *)
type unreadable_record = { id : string; reason : string }

(* The runs a store holds, plus the records it could not read, so one bad
   record never hides the others. *)
type run_listing = {
  runs : Run.t list;
  unreadable_records : unreadable_record list;
}

module type S = sig
  (* Proof that this process drives one run; give it back with
     [release_run_lock]. *)
  type run_lock

  (* An atomic full replacement: a reader sees the previous record or the
     new one, never a mix. *)
  val save : runs_dir:string -> Run.t -> (string, string) result
  val load : runs_dir:string -> id:string -> (Run.t, string) result

  (* Runs newest-first by start time; empty when nothing has run. *)
  val list_runs : runs_dir:string -> (run_listing, string) result

  (* Exclusive per-run lock: an Error when another live process drives the
     run, so two processes never write one record. *)
  val acquire_run_lock :
    runs_dir:string -> id:string -> (run_lock, string) result

  val release_run_lock : run_lock -> unit
end

module Filesystem : S = struct
  type lock_holder = { pid : int; host : string; started_at : string }
  type run_lock = { lock_path : string; holder : lock_holder }

  let ensure_directory path =
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

  let remove_quietly path =
    try Unix.unlink path with Unix.Unix_error _ -> ()

  let record_path ~runs_dir ~id = Filename.concat runs_dir (id ^ ".json")
  let lock_path ~runs_dir ~id = Filename.concat runs_dir (id ^ ".lock")

  let write_file_durably ~path content =
    let descriptor =
      Unix.openfile path
        [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ]
        0o644
    in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () ->
        let length = String.length content in
        let rec write_from offset =
          if offset < length then
            write_from
              (offset
              + Unix.write_substring descriptor content offset
                  (length - offset))
        in
        write_from 0;
        Unix.fsync descriptor)

  (* Makes a rename in the directory survive a host crash. *)
  let fsync_directory path =
    let descriptor = Unix.openfile path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close descriptor)
      (fun () ->
        try Unix.fsync descriptor
        with Unix.Unix_error (Unix.EINVAL, _, _) -> ())

  let store_error error = Printf.sprintf "store: %s" (Unix.error_message error)

  let save ~runs_dir run =
    let content = Yojson.Safe.pretty_to_string (Run.to_json run) in
    let path = record_path ~runs_dir ~id:run.Run.id in
    (* One temp name per process, so two writers never share a file. *)
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
      (Result.map_error (fun message -> "store: " ^ message) (File.read path))
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
              "store: failed to read %s: %s (the record may have been \
               written by a different jig version)"
              path message)
          parsed)

  let compare_newest_first a b =
    (* Same-second ties break on the id's sequence suffix: with equal
       prefixes a longer id is a later run. *)
    match String.compare b.Run.started_at a.Run.started_at with
    | 0 -> (
        match compare (String.length b.Run.id) (String.length a.Run.id) with
        | 0 -> String.compare b.Run.id a.Run.id
        | order -> order)
    | order -> order

  let list_runs ~runs_dir =
    if not (Sys.file_exists runs_dir) then
      Ok { runs = []; unreadable_records = [] }
    else
      match Sys.readdir runs_dir with
      | exception Sys_error message -> Error ("store: " ^ message)
      | files ->
          let ids =
            files |> Array.to_list |> List.sort String.compare
            |> List.filter_map (fun file ->
                   if Filename.check_suffix file ".json" then
                     Some (Filename.chop_suffix file ".json")
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
          Ok
            {
              runs = List.sort compare_newest_first runs;
              unreadable_records;
            }

  let lock_holder_to_string holder =
    Yojson.Safe.to_string
      (`Assoc
        [
          ("pid", `Int holder.pid);
          ("host", `String holder.host);
          ("started_at", `String holder.started_at);
        ])

  type lock_state =
    | Lock_absent
    | Lock_held of lock_holder
    | Lock_unreadable of string

  let read_lock_state path =
    match In_channel.with_open_text path In_channel.input_all with
    | exception Sys_error _ when not (Sys.file_exists path) -> Lock_absent
    | exception Sys_error message -> Lock_unreadable message
    | content -> (
        match Yojson.Safe.from_string content with
        | exception Yojson.Json_error message -> Lock_unreadable message
        | json -> (
            let member key = Yojson.Safe.Util.member key json in
            match (member "pid", member "host", member "started_at") with
            | `Int pid, `String host, `String started_at ->
                Lock_held { pid; host; started_at }
            | _ -> Lock_unreadable "missing pid, host, or started_at"))

  let process_is_alive pid =
    match Unix.kill pid 0 with
    | () -> true
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
    (* EPERM: the pid exists but belongs to another user. *)
    | exception Unix.Unix_error _ -> true

  (* A pid on another host cannot be checked, so only same-host locks go
     stale. *)
  let lock_is_stale holder =
    holder.host = Unix.gethostname () && not (process_is_alive holder.pid)

  let live_lock_message ~id ~lock holder =
    if holder.host = Unix.gethostname () then
      Printf.sprintf
        "store: run %s is driven by live pid %d (since %s); wait for it to \
         finish or stop that process"
        id holder.pid holder.started_at
    else
      Printf.sprintf
        "store: run %s is driven by pid %d on host %s (since %s); remove %s \
         only when that process is gone"
        id holder.pid holder.host holder.started_at lock

  (* link(2) fails with EEXIST on an existing lock and never exposes a lock
     file without its content, unlike O_EXCL create-then-write. *)
  let rec claim_lock ~id ~lock ~candidate ~attempts_left =
    match Unix.link candidate lock with
    | () -> Ok ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> (
        if attempts_left = 0 then
          Error
            (Printf.sprintf
               "store: run %s: the lock %s keeps changing hands; try again" id
               lock)
        else
          let retry () =
            claim_lock ~id ~lock ~candidate ~attempts_left:(attempts_left - 1)
          in
          match read_lock_state lock with
          | Lock_absent -> retry ()
          | Lock_unreadable reason ->
              Error
                (Printf.sprintf
                   "store: run %s has an unreadable lock %s (%s); remove it \
                    only when no jig process drives the run"
                   id lock reason)
          | Lock_held holder when lock_is_stale holder ->
              take_over_stale_lock ~id ~lock ~retry
          | Lock_held holder -> Error (live_lock_message ~id ~lock holder))

  (* Move the stale lock aside first, so a racing process that already took
     it over keeps its fresh lock instead of losing it to our unlink. *)
  and take_over_stale_lock ~id ~lock ~retry =
    let moved = Printf.sprintf "%s.stale.%d" lock (Unix.getpid ()) in
    match Unix.rename lock moved with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> retry ()
    | () -> (
        match read_lock_state moved with
        | Lock_held holder when not (lock_is_stale holder) ->
            (try Unix.link moved lock
             with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
            remove_quietly moved;
            Error (live_lock_message ~id ~lock holder)
        | Lock_held _ | Lock_absent | Lock_unreadable _ ->
            remove_quietly moved;
            retry ())

  (* Temp records a killed writer left behind; safe to remove under the
     lock, because no other process writes this run. *)
  let remove_abandoned_temporary_records ~runs_dir ~id =
    let prefix = id ^ ".json.tmp." in
    Array.iter
      (fun file ->
        if String.starts_with ~prefix file then
          remove_quietly (Filename.concat runs_dir file))
      (Sys.readdir runs_dir)

  let acquire_run_lock ~runs_dir ~id =
    let lock = lock_path ~runs_dir ~id in
    let holder =
      {
        pid = Unix.getpid ();
        host = Unix.gethostname ();
        started_at = Run.iso8601 (Unix.gettimeofday ());
      }
    in
    let candidate = Printf.sprintf "%s.%d" lock holder.pid in
    try
      ensure_directory runs_dir;
      write_file_durably ~path:candidate (lock_holder_to_string holder);
      let claimed = claim_lock ~id ~lock ~candidate ~attempts_left:3 in
      remove_quietly candidate;
      Result.map
        (fun () ->
          (try
             fsync_directory runs_dir;
             remove_abandoned_temporary_records ~runs_dir ~id
           with Unix.Unix_error _ | Sys_error _ -> ());
          { lock_path = lock; holder })
        claimed
    with
    | Unix.Unix_error (error, _, _) ->
        remove_quietly candidate;
        Error (store_error error)
    | Sys_error message ->
        remove_quietly candidate;
        Error ("store: " ^ message)

  (* Removes the lock only while it is still ours, never a successor's. *)
  let release_run_lock run_lock =
    match read_lock_state run_lock.lock_path with
    | Lock_held holder when holder = run_lock.holder ->
        remove_quietly run_lock.lock_path
    | Lock_held _ | Lock_absent | Lock_unreadable _ -> ()
end
