(* Incremental tail over the running step's output files. The subprocess
   layer already captures a child's output into temp files while it runs;
   polling those files is how the live view shows work in flight without
   touching the pipe-free capture design. Each source remembers its byte
   offset and stitches partial lines across reads; only the newest
   [capacity] complete lines are kept. Polling must never block and must
   tolerate files that do not exist yet. *)

(* Bounds one poll after a burst; older output is visible in the on-disk
   log, not the tail. *)
let max_read = 65536

type source = { path : string; mutable offset : int; mutable partial : string }

type t = {
  sources : source list;
  capacity : int;
  (* newest first *)
  mutable kept : string list;
}

let create ~capacity paths =
  {
    sources = List.map (fun path -> { path; offset = 0; partial = "" }) paths;
    capacity = max 1 capacity;
    kept = [];
  }

let rec take count = function
  | [] -> []
  | _ when count = 0 -> []
  | head :: rest -> head :: take (count - 1) rest

let push t line = t.kept <- take t.capacity (line :: t.kept)

let read_delta source size =
  let channel = open_in_bin source.path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      seek_in channel source.offset;
      let wanted = min (size - source.offset) max_read in
      let chunk = really_input_string channel wanted in
      source.offset <- source.offset + wanted;
      chunk)

let poll_source t source =
  match Unix.stat source.path with
  | exception Unix.Unix_error _ -> ()
  | exception Sys_error _ -> ()
  | stat ->
      let size = stat.Unix.st_size in
      (* A shrinking file was truncated or replaced; start over. *)
      if size < source.offset then (
        source.offset <- 0;
        source.partial <- "");
      if size > source.offset then (
        (* After a burst larger than one read, jump to the newest bytes and
           drop the then-incomplete first line. *)
        let skipped = size - source.offset > max_read in
        if skipped then (
          source.offset <- size - max_read;
          source.partial <- "");
        match read_delta source size with
        | exception Sys_error _ -> ()
        | chunk -> (
            let text = source.partial ^ chunk in
            match List.rev (String.split_on_char '\n' text) with
            | [] -> ()
            | trailing :: complete_reversed ->
                source.partial <- trailing;
                let complete = List.rev complete_reversed in
                let complete =
                  if skipped then match complete with _ :: rest -> rest | [] -> []
                  else complete
                in
                List.iter (push t) complete))

let poll t = List.iter (poll_source t) t.sources

(* Oldest-to-newest complete lines, then any in-progress partials - the
   freshest thing the step said, even before its newline arrives. *)
let lines t =
  List.rev t.kept
  @ List.filter_map
      (fun source -> if source.partial = "" then None else Some source.partial)
      t.sources
