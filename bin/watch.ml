(* jig watch: the same Pipeline + Log live view as a foreground run, for a
   run that is already executing (or finished) - typically one started with
   --detach. Everything is reconstructed from files: the run record gives
   the step tree (re-parsed when its mtime changes), and runs/<id>.current
   names the running step's captured-output files for the log tail. The
   watcher is a pure reader - quitting it never affects the run. *)

module Progress = Jig_core.Progress
module Boxes = Jig_core.Boxes
module Tail = Jig_core.Tail
module Run = Jig_core.Run
module Current = Jig_core.Current

(* Same cadence and tail depth as the foreground live view. *)
let tick_interval = Live.tick_interval
let tail_capacity = Live.tail_capacity

let state_of_status = function
  | Run.Running -> Boxes.Running
  | Run.Paused -> Boxes.Paused
  | Run.Completed -> Boxes.Passed
  | Run.Failed | Run.Aborted -> Boxes.Failed

let item_key_of_position (position : Run.position) =
  match position.Run.for_each with
  | Some state ->
      Option.map Jig_core.Items.key
        (List.nth_opt state.Run.items state.Run.item_index)
  | None -> None

(* The model for one frame: replay the record, then mark the in-flight step
   Working - its identity comes from the pointer while the subprocess runs,
   and from the persisted position between steps. *)
let build_model ~entries ~entry_count (record : Run.t) pointer =
  let model = Progress.restore ~entries record in
  (if
     record.Run.status = Run.Running
     && record.Run.position.Run.entry_index < entry_count
   then
     let skill, item_key, tier =
       match pointer with
       | Some current ->
           ( Some current.Current.skill,
             current.Current.item_key,
             current.Current.tier )
       | None -> (
           match
             Jig_core.Runner.Default.skip_target_skill ~entries
               ~position:record.Run.position
           with
           | Ok skill ->
               (Some skill, item_key_of_position record.Run.position, None)
           | Error _ -> (None, None, None))
     in
     match skill with
     | Some skill ->
         Progress.apply model
           (Jig_core.Runner.Step_started
              { skill; position = record.Run.position; item_key; tier })
     | None -> ());
  model

(* skill · item · tier - the same title the foreground live view shows. *)
let log_title pointer =
  Option.map
    (fun current ->
      String.concat " · "
        (current.Current.skill
         :: Option.to_list current.Current.item_key
        @ Option.to_list current.Current.tier))
    pointer

let elapsed_of_pointer pointer =
  Option.bind pointer (fun current ->
      Option.map
        (fun started ->
          Live.format_elapsed (max 0.0 (Unix.gettimeofday () -. started)))
        (Run.epoch_of_iso8601 current.Current.started_at))

let frame ~workflow ~state ~tick ~elapsed ~log_title ~log_lines model =
  Boxes.view ~columns:(Term_size.columns ()) ~height:(Term_size.rows ())
    ~workflow ~state
    ~spinner:(Progress.spinner_frame tick)
    ~elapsed ~log_title ~log_lines (Progress.rows model)

(* Raw stdin while watching, so a bare q detaches; restored on every exit
   path (q, terminal state, Ctrl-C). *)
let with_keyboard body =
  if not (Unix.isatty Unix.stdin) then body (fun () -> false)
  else
    let original = Unix.tcgetattr Unix.stdin in
    let restore () = Unix.tcsetattr Unix.stdin Unix.TCSANOW original in
    let raw =
      { original with Unix.c_icanon = false; c_echo = false; c_vmin = 0;
        c_vtime = 0 }
    in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW raw;
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle
         (fun _ ->
           restore ();
           print_newline ();
           exit 130));
    let quit_requested () =
      match Unix.select [ Unix.stdin ] [] [] 0.0 with
      | [], _, _ -> false
      | _ ->
          let buffer = Bytes.create 1 in
          Unix.read Unix.stdin buffer 0 1 > 0 && Bytes.get buffer 0 = 'q'
    in
    Fun.protect ~finally:restore (fun () -> body quit_requested)

(* Poll loop: reload the record when its mtime moves, retarget the tail
   when the pointer's paths change, repaint every tick. *)
let watch ~root ~run_id =
  let runs_dir = Jig_core.Project.runs_dir ~root in
  let record_path = Filename.concat runs_dir (run_id ^ ".json") in
  Result.bind (Jig_core.Store.Filesystem.load ~runs_dir ~id:run_id)
    (fun initial ->
      let jig_dir = Filename.concat root ".jig" in
      Result.bind
        (Jig_core.Project.resolve_workflow ~jig_dir
           ~name:initial.Run.workflow)
        (fun (workflow_path, _) ->
          Result.bind (Jig_core.Workflow.load ~path:workflow_path)
            (fun workflow ->
              let entries = workflow.Jig_core.Workflow.entries in
              let entry_count = List.length entries in
              let record = ref initial in
              let record_mtime = ref 0.0 in
              let tail : (string * Tail.t) option ref = ref None in
              let height = ref 0 in
              let tick = ref 0 in
              let reload () =
                match Unix.stat record_path with
                | { Unix.st_mtime; _ } when st_mtime <> !record_mtime -> (
                    match
                      Jig_core.Store.Filesystem.load ~runs_dir ~id:run_id
                    with
                    (* The mtime advances only on a good parse: a mid-write
                       read keeps the last good record AND retries, so the
                       run's final write can never be skipped. *)
                    | Ok fresh ->
                        record_mtime := st_mtime;
                        record := fresh
                    | Error _ -> ())
                | _ | (exception Unix.Unix_error _) -> ()
              in
              let pointer () =
                if !record.Run.status = Run.Running then
                  Current.read ~runs_dir ~run_id
                else None
              in
              let tail_lines current =
                match current with
                | None ->
                    tail := None;
                    []
                | Some pointer ->
                    let key =
                      pointer.Current.stdout_path ^ "\n"
                      ^ pointer.Current.stderr_path
                    in
                    let live_tail =
                      match !tail with
                      | Some (existing_key, existing) when existing_key = key
                        ->
                          existing
                      | _ ->
                          let created =
                            Tail.create ~capacity:tail_capacity
                              [
                                pointer.Current.stdout_path;
                                pointer.Current.stderr_path;
                              ]
                          in
                          tail := Some (key, created);
                          created
                    in
                    Tail.poll live_tail;
                    Tail.lines live_tail
              in
              let paint ~final =
                let current = if final then None else pointer () in
                let model =
                  build_model ~entries ~entry_count !record current
                in
                if final then Progress.finalize model;
                let state = state_of_status !record.Run.status in
                height :=
                  Live.repaint ~previous_height:!height
                    (frame ~workflow:!record.Run.workflow ~state ~tick:!tick
                       ~elapsed:(elapsed_of_pointer current)
                       ~log_title:(log_title current)
                       ~log_lines:(tail_lines current) model)
              in
              with_keyboard (fun quit_requested ->
                  let rec loop () =
                    reload ();
                    if !record.Run.status <> Run.Running then (
                      paint ~final:true;
                      Printf.printf "run %s: %s\n" run_id
                        (Run.string_of_status !record.Run.status);
                      Ok ())
                    else (
                      paint ~final:false;
                      if quit_requested () then (
                        Printf.printf "detached from %s (still running)\n"
                          run_id;
                        Ok ())
                      else (
                        Thread.delay tick_interval;
                        incr tick;
                        loop ()))
                  in
                  loop ()))))
