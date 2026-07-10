(* In-place live step tree for a TTY run. Consumes the runner's lifecycle
   events, redraws the whole block in place on each one. The working glyph
   is static here; the animated spinner is a separate concern. *)

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

type t = { model : Progress.t; mutable height : int }

let create entries = { model = Progress.init entries; height = 0 }

let working_glyph = "▸"

(* Move to the top of the previously drawn block, clear it, and reprint.
   Labels are truncated to the terminal width so a long line can't wrap and
   throw off the cursor-up count. *)
let redraw t =
  if t.height > 0 then Printf.printf "\027[%dA" t.height;
  print_string "\027[0J";
  let width = cols () in
  let rows = Progress.rows t.model in
  List.iter
    (fun (r : Progress.row) ->
      let glyph = Progress.glyph ~working:working_glyph r.Progress.status in
      let avail = max 8 (width - r.Progress.indent - 2) in
      let label = truncate avail r.Progress.label in
      Printf.printf "%*s\027[%sm%s\027[0m %s\n" r.Progress.indent ""
        (color_of r.Progress.status) glyph label)
    rows;
  t.height <- List.length rows;
  flush stdout

let on_event t event =
  Progress.apply t.model event;
  redraw t

let finalize t =
  Progress.finalize t.model;
  redraw t
