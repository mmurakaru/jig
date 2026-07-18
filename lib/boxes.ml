(* Pure layout for the live two-box run view: a Pipeline box (the step tree
   with durations and costs) over a Log box (the tail of the running step's
   output). Produces styled segments, no ANSI and no IO - the terminal
   caller maps styles to escape codes; tests assert on [plain]. Every line
   is padded to exactly the box width, which is what lets the renderer
   overwrite frames in place without clearing. *)

type style = Plain | Dim | Green | Red | Yellow | Magenta | Title
type segment = { text : string; style : style }
type run_state = Running | Passed | Failed | Paused

let seg ?(style = Plain) text = { text; style }
let plain segments = String.concat "" (List.map (fun s -> s.text) segments)

let segment_width segment = Sanitize.width segment.text
let width_of segments = List.fold_left (fun n s -> n + segment_width s) 0 segments

(* Cut a segment list to [limit] display columns, truncating mid-segment. *)
let truncate_segments limit segments =
  let rec cut remaining = function
    | [] -> []
    | segment :: rest ->
        let w = segment_width segment in
        if w <= remaining then segment :: cut (remaining - w) rest
        else if remaining <= 0 then []
        else [ { segment with text = Sanitize.truncate remaining segment.text } ]
  in
  cut limit segments

let default_log_lines = 6
let compact_log_lines = 3
let max_box_width = 80
let min_box_width = 40

let box_width ~columns = max min_box_width (min columns max_box_width)

(* "| " + content + " |", content padded (or truncated) to box width. *)
let interior ~box_width left right =
  let content_width = box_width - 4 in
  let right_width = width_of right in
  let available =
    if right = [] then content_width
    else max 0 (content_width - right_width - 2)
  in
  let left = truncate_segments available left in
  let padding = content_width - width_of left - right_width in
  List.concat
    [
      [ seg ~style:Dim "│ " ];
      left;
      (if padding > 0 then [ seg (String.make padding ' ') ] else []);
      right;
      [ seg ~style:Dim " │" ];
    ]

let repeat count text = String.concat "" (List.init (max 0 count) (fun _ -> text))

let top ~box_width title =
  let title = truncate_segments (box_width - 6) title in
  let fill = box_width - 5 - width_of title in
  List.concat
    [ [ seg ~style:Dim "╭─ " ]; title; [ seg ~style:Dim (" " ^ repeat fill "─" ^ "╮") ] ]

let bottom ?footer ~box_width () =
  match footer with
  | None -> [ seg ~style:Dim ("╰" ^ repeat (box_width - 2) "─" ^ "╯") ]
  | Some footer ->
      let footer = Sanitize.truncate (box_width - 9) footer in
      let fill = box_width - 8 - Sanitize.width footer in
      [
        seg ~style:Dim
          ("╰" ^ repeat 4 "─" ^ " " ^ footer ^ " " ^ repeat fill "─" ^ "╯");
      ]

let status_style : Progress.status -> style = function
  | Progress.Pending -> Dim
  | Progress.Working -> Yellow
  | Progress.Done -> Green
  | Progress.Failed -> Red
  | Progress.Paused -> Magenta

let state_banner = function
  | Running -> seg ~style:Yellow "running"
  | Passed -> seg ~style:Green "passed"
  | Failed -> seg ~style:Red "failed"
  | Paused -> seg ~style:Magenta "paused"

(* The live elapsed belongs on the deepest row currently working - inside a
   forEach that is the item, not the loop head. *)
let last_working_index rows =
  let result = ref (-1) in
  List.iteri
    (fun index (row : Progress.row) ->
      if row.Progress.status = Progress.Working then result := index)
    rows;
  !result

let pipeline_box ~box_width ~connectors ~workflow ~state ~spinner ~elapsed rows
    =
  let lines = ref [] in
  let push line = lines := line :: !lines in
  push (top ~box_width [ seg ~style:Title "Pipeline" ]);
  push
    (interior ~box_width
       [ seg ~style:Dim (Sanitize.line workflow) ]
       [ state_banner state ]);
  push (interior ~box_width [] []);
  let elapsed_index = last_working_index rows in
  let seen_top_level = ref false in
  List.iteri
    (fun index (row : Progress.row) ->
      if row.Progress.indent = 0 then (
        if connectors && !seen_top_level then
          push (interior ~box_width [ seg ~style:Dim "│" ] []);
        seen_top_level := true);
      let glyph = Progress.glyph ~working:spinner row.Progress.status in
      let label_style =
        if row.Progress.status = Progress.Pending then Dim else Plain
      in
      (* Labels come from workflow files and can carry anything a YAML
         string can (a command step's label is the command itself). *)
      let left =
        List.concat
          [
            (if row.Progress.indent > 0 then
               [ seg (String.make row.Progress.indent ' ') ]
             else []);
            [ seg ~style:(status_style row.Progress.status) glyph ];
            [ seg ~style:label_style (" " ^ Sanitize.line row.Progress.label) ];
          ]
      in
      let detail =
        match row.Progress.detail with
        | Some _ as detail -> detail
        | None when index = elapsed_index -> elapsed
        | None -> None
      in
      let right =
        match detail with Some text -> [ seg ~style:Dim text ] | None -> []
      in
      push (interior ~box_width left right))
    rows;
  push (bottom ~box_width ());
  List.rev !lines

let log_box ~box_width ~title ~max_lines lines =
  let content_width = box_width - 4 in
  let sanitized =
    List.map (fun line -> Sanitize.truncate content_width (Sanitize.line line)) lines
  in
  let tail =
    let overflow = List.length sanitized - max_lines in
    if overflow > 0 then List.filteri (fun i _ -> i >= overflow) sanitized
    else sanitized
  in
  let styled line =
    if String.length line >= 4 && String.sub line 0 4 = "PASS" then
      [ seg ~style:Green "PASS"; seg ~style:Dim (String.sub line 4 (String.length line - 4)) ]
    else if String.length line >= 4 && String.sub line 0 4 = "FAIL" then
      [ seg ~style:Red "FAIL"; seg ~style:Dim (String.sub line 4 (String.length line - 4)) ]
    else [ seg ~style:Dim line ]
  in
  let title_segments =
    seg ~style:Title "Log"
    :: (match title with
       | Some text -> [ seg ~style:Dim (" ─ " ^ Sanitize.line text) ]
       | None -> [])
  in
  List.concat
    [
      [ top ~box_width title_segments ];
      List.map (fun line -> interior ~box_width (styled line) []) tail;
      [ bottom ~footer:"tail of step output" ~box_width () ];
    ]

(* The whole frame. The block must fit the terminal height or the in-place
   repaint corrupts, so it degrades in order: drop the connectors, shrink
   the log, hide the log. The last tier is used even when it still does not
   fit - a best effort beats rendering nothing. *)
let view ~columns ~height ~workflow ~state ~spinner ~elapsed ~log_title
    ~log_lines rows =
  let box_width = box_width ~columns in
  let log_wanted = state = Running && log_lines <> [] in
  let assemble ~connectors ~log_budget =
    let pipeline =
      pipeline_box ~box_width ~connectors ~workflow ~state ~spinner ~elapsed
        rows
    in
    if log_wanted && log_budget > 0 then
      pipeline @ [ [] ]
      @ log_box ~box_width ~title:log_title ~max_lines:log_budget log_lines
    else pipeline
  in
  let tiers =
    [
      (true, default_log_lines);
      (false, default_log_lines);
      (false, compact_log_lines);
      (false, 0);
    ]
  in
  let fits frame = List.length frame <= height - 1 in
  let rec pick = function
    | [] -> assemble ~connectors:false ~log_budget:0
    | (connectors, log_budget) :: rest ->
        let frame = assemble ~connectors ~log_budget in
        if fits frame || rest = [] then frame else pick rest
  in
  pick tiers
