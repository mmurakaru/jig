(* In-place live step tree for a TTY run. Consumes the runner's lifecycle
   events and redraws the block in place. A background ticker animates the
   active step's spinner while a step runs (the main thread is blocked in
   the step's subprocess); the ticker is gated to only spin while something
   is working, so an idle or paused run costs nothing. All redraws take the
   mutex, so the two threads never interleave terminal output. *)

module Progress = Jig_core.Progress

let color_of : Progress.status -> string = function
  | Pending -> "90" (* dim *)
  | Working -> "33" (* yellow *)
  | Done -> "32" (* green *)
  | Failed -> "31" (* red *)
  | Paused -> "35" (* magenta *)

let cols () =
  match Sys.getenv_opt "COLUMNS" with
  | Some s -> ( try max 40 (int_of_string s) with _ -> 120)
  | None -> 120

let truncate limit s = if String.length s <= limit then s else String.sub s 0 limit

let tick_interval = 0.1 (* ~10 fps while working *)

type t = {
  model : Progress.t;
  mutex : Mutex.t;
  wake : Condition.t;
  mutable tick : int;
  mutable working : bool;
  mutable stopped : bool;
  mutable height : int;
  mutable ticker : Thread.t option;
}

(* Assumes the mutex is held. Move to the top of the previously drawn block,
   clear it, and reprint. Labels are truncated to the terminal width so a
   long line cannot wrap and throw off the cursor-up count. *)
let redraw_locked t =
  if t.height > 0 then Printf.printf "\027[%dA" t.height;
  print_string "\027[0J";
  let width = cols () in
  let working = Progress.spinner_frame t.tick in
  let rows = Progress.rows t.model in
  List.iter
    (fun (r : Progress.row) ->
      let glyph = Progress.glyph ~working r.Progress.status in
      let avail = max 8 (width - r.Progress.indent - 2) in
      let label = truncate avail r.Progress.label in
      Printf.printf "%*s\027[%sm%s\027[0m %s\n" r.Progress.indent ""
        (color_of r.Progress.status) glyph label)
    rows;
  t.height <- List.length rows;
  flush stdout

let ticker_loop t =
  Mutex.lock t.mutex;
  while not t.stopped do
    if t.working then (
      t.tick <- t.tick + 1;
      redraw_locked t;
      Mutex.unlock t.mutex;
      Thread.delay tick_interval;
      Mutex.lock t.mutex)
    else Condition.wait t.wake t.mutex
  done;
  Mutex.unlock t.mutex

let create entries =
  let t =
    {
      model = Progress.init entries;
      mutex = Mutex.create ();
      wake = Condition.create ();
      tick = 0;
      working = false;
      stopped = false;
      height = 0;
      ticker = None;
    }
  in
  t.ticker <- Some (Thread.create ticker_loop t);
  t

let on_event t event =
  Mutex.lock t.mutex;
  Progress.apply t.model event;
  (match event with
  | Jig_core.Runner.Step_started _ -> t.working <- true
  | Jig_core.Runner.Step_finished _ -> t.working <- false
  | Jig_core.Runner.Items_resolved _ | Jig_core.Runner.Step_output _ -> ());
  redraw_locked t;
  Condition.signal t.wake;
  Mutex.unlock t.mutex

let finalize t =
  Mutex.lock t.mutex;
  Progress.finalize t.model;
  t.working <- false;
  t.stopped <- true;
  redraw_locked t;
  Condition.signal t.wake;
  Mutex.unlock t.mutex;
  match t.ticker with Some th -> Thread.join th | None -> ()
