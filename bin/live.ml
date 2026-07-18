(* In-place live view for a TTY run: the Pipeline box over the Log box,
   redrawn in place as lifecycle events arrive. A background ticker
   animates the spinner and polls the running step's output tail while the
   main thread is blocked in the step's subprocess; it is gated to only
   run while something is working, so an idle or paused run costs nothing.
   All terminal writes and model mutations happen under the mutex. Layout
   lives in Jig_core.Boxes; this module is only ANSI, clocks, and threads. *)

module Progress = Jig_core.Progress
module Boxes = Jig_core.Boxes
module Tail = Jig_core.Tail

let sgr_of_style : Boxes.style -> string option = function
  | Boxes.Plain -> None
  | Boxes.Dim -> Some "90"
  | Boxes.Green -> Some "32"
  | Boxes.Red -> Some "31"
  | Boxes.Yellow -> Some "33"
  | Boxes.Magenta -> Some "35"
  | Boxes.Title -> Some "1;36"

let print_segment (segment : Boxes.segment) =
  match sgr_of_style segment.Boxes.style with
  | None -> print_string segment.Boxes.text
  | Some code -> Printf.printf "\027[%sm%s\027[0m" code segment.Boxes.text

let format_elapsed seconds =
  let whole = int_of_float seconds in
  if whole < 60 then Printf.sprintf "%ds" whole
  else Printf.sprintf "%dm%02ds" (whole / 60) (whole mod 60)

let tick_interval = 0.1 (* ~10 fps while working *)
let tail_capacity = 32

(* Overwrite the previous frame in place. Every Boxes line is padded to the
   full box width, so an erase-to-end-of-line per line repaints without the
   flicker of clearing the whole block first; the final erase-below handles
   a block that shrank. Returns the new frame height. *)
let repaint ~previous_height lines =
  if previous_height > 0 then Printf.printf "\027[%dA" previous_height;
  List.iter
    (fun line ->
      List.iter print_segment line;
      print_string "\027[K\n")
    lines;
  print_string "\027[0J";
  flush stdout;
  List.length lines

type t = {
  model : Progress.t;
  workflow : string;
  mutex : Mutex.t;
  wake : Condition.t;
  mutable tick : int;
  mutable working : bool;
  mutable stopped : bool;
  mutable height : int;
  mutable tail : Tail.t option;
  mutable step_label : string option;
  mutable step_started : float;
  mutable ticker : Thread.t option;
}

(* Assumes the mutex is held. *)
let redraw_locked t ~state =
  Option.iter Tail.poll t.tail;
  let elapsed =
    if t.working then
      Some (format_elapsed (Unix.gettimeofday () -. t.step_started))
    else None
  in
  let lines =
    Boxes.view ~columns:(Term_size.columns ()) ~height:(Term_size.rows ())
      ~workflow:t.workflow ~state
      ~spinner:(Progress.spinner_frame t.tick)
      ~elapsed ~log_title:t.step_label
      ~log_lines:(match t.tail with Some tail -> Tail.lines tail | None -> [])
      (Progress.rows t.model)
  in
  t.height <- repaint ~previous_height:t.height lines

let ticker_loop t =
  Mutex.lock t.mutex;
  while not t.stopped do
    if t.working then (
      t.tick <- t.tick + 1;
      redraw_locked t ~state:Boxes.Running;
      Mutex.unlock t.mutex;
      Thread.delay tick_interval;
      Mutex.lock t.mutex)
    else Condition.wait t.wake t.mutex
  done;
  Mutex.unlock t.mutex

let create ~workflow entries =
  let t =
    {
      model = Progress.init entries;
      workflow;
      mutex = Mutex.create ();
      wake = Condition.create ();
      tick = 0;
      working = false;
      stopped = false;
      height = 0;
      tail = None;
      step_label = None;
      step_started = 0.0;
      ticker = None;
    }
  in
  t.ticker <- Some (Thread.create ticker_loop t);
  t

let on_event t event =
  Mutex.lock t.mutex;
  Progress.apply t.model event;
  (match event with
  | Jig_core.Runner.Step_started { skill; item_key; _ } ->
      t.working <- true;
      t.step_started <- Unix.gettimeofday ();
      t.step_label <-
        Some
          (match item_key with
          | Some key -> skill ^ " · " ^ key
          | None -> skill);
      t.tail <- None
  | Jig_core.Runner.Step_output { stdout_path; stderr_path } ->
      t.tail <- Some (Tail.create ~capacity:tail_capacity [ stdout_path; stderr_path ])
  | Jig_core.Runner.Step_finished _ ->
      t.working <- false;
      t.tail <- None;
      t.step_label <- None
  | Jig_core.Runner.Items_resolved _ -> ());
  redraw_locked t ~state:Boxes.Running;
  Condition.signal t.wake;
  Mutex.unlock t.mutex

(* The last frame is the durable one: the completed Pipeline box stays in
   the scrollback (the log tail was transient working info; full logs live
   on disk) and the run report prints below it. *)
let finalize t ~state =
  Mutex.lock t.mutex;
  Progress.finalize t.model;
  t.working <- false;
  t.stopped <- true;
  t.tail <- None;
  t.step_label <- None;
  redraw_locked t ~state;
  Condition.signal t.wake;
  Mutex.unlock t.mutex;
  match t.ticker with Some thread -> Thread.join thread | None -> ()
