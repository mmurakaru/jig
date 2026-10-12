(* Receivers such as otel-desktop-viewer drop a batch with a known span id. *)

let path ~runs_dir ~run_id = Filename.concat runs_dir (run_id ^ ".otlp-sent")

let is_span_id text =
  String.length text = 16
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) text

let read_ledger_text ~runs_dir ~run_id =
  let ledger_path = path ~runs_dir ~run_id in
  if not (Sys.file_exists ledger_path) then Ok ""
  else
    match In_channel.with_open_bin ledger_path In_channel.input_all with
    | text -> Ok text
    | exception Sys_error message -> Error ("otlp-sent: " ^ message)

(* A last line with no newline is torn by a crash, so it does not count. *)
let span_ids_of_ledger_text text =
  match List.rev (String.split_on_char '\n' text) with
  | [] -> []
  | _torn_or_empty :: complete_lines -> List.filter is_span_id (List.rev complete_lines)

(** The span ids already printed for the run; none when no ledger exists. *)
let read_sent_span_ids ~runs_dir ~run_id =
  Result.map span_ids_of_ledger_text (read_ledger_text ~runs_dir ~run_id)

let fsync_directory directory =
  let descriptor = Unix.openfile directory [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () -> try Unix.fsync descriptor with Unix.Unix_error _ -> ())

(** Appends [span_ids] in one fsynced write, after any torn last line. *)
let append_sent_span_ids ~runs_dir ~run_id span_ids =
  match span_ids with
  | [] -> Ok ()
  | span_ids -> (
      match read_ledger_text ~runs_dir ~run_id with
      | Error message -> Error message
      | Ok existing -> (
          let ledger_path = path ~runs_dir ~run_id in
          let created = not (Sys.file_exists ledger_path) in
          let separator =
            if existing = "" || String.ends_with ~suffix:"\n" existing then "" else "\n"
          in
          let text =
            separator ^ String.concat "" (List.map (fun span_id -> span_id ^ "\n") span_ids)
          in
          try
            let descriptor =
              Unix.openfile ledger_path
                [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT; Unix.O_CLOEXEC ]
                0o644
            in
            Fun.protect
              ~finally:(fun () -> Unix.close descriptor)
              (fun () ->
                let bytes = Bytes.of_string text in
                let rec write_from offset =
                  if offset < Bytes.length bytes then
                    write_from
                      (offset + Unix.write descriptor bytes offset (Bytes.length bytes - offset))
                in
                write_from 0;
                Unix.fsync descriptor);
            if created then fsync_directory runs_dir;
            Ok ()
          with Unix.Unix_error (error, _, _) -> Error ("otlp-sent: " ^ Unix.error_message error)))

(** Prints, then records, the spans no earlier call printed; maybe none. *)
let print_new_spans ~runs_dir ~resource_attributes ~print (run : Run.t) =
  match read_sent_span_ids ~runs_dir ~run_id:run.Run.id with
  | Error message -> Error message
  | Ok sent ->
      let new_spans =
        List.filter
          (fun span -> not (List.mem (Otlp.span_id_of_span span) sent))
          (Otlp.spans_of_run run)
      in
      print (Yojson.Safe.to_string (Otlp.traces_document ~resource_attributes new_spans));
      append_sent_span_ids ~runs_dir ~run_id:run.Run.id (List.map Otlp.span_id_of_span new_spans)
