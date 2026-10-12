(* Pure rendering of a run's progress as a step tree. The runner's
   lifecycle events (Runner.run_event) drive the model via [apply]; [render]
   turns it into display lines. The working glyph (a static marker, or an
   animated spinner frame) and any colour are supplied by the caller, so
   this module stays pure and testable.

   forEach density: top-level steps are one line each; inside a loop, done
   and pending items are one line each, and only the active item expands to
   show its body steps. *)

type status = Pending | Working | Done | Failed | Paused

(* [detail] is the right-aligned annotation a renderer may show next to a
   finished step: its duration, and its cost when the step spent money. *)
type body_step = { bskill : string; mutable bstatus : status; mutable bdetail : string option }

type item = {
  key : string;
  mutable istatus : status;
  mutable idetail : string option;
  body : body_step list;
}

type node =
  | Step_node of { skill : string; mutable status : status; mutable detail : string option }
  | Loop_node of {
      body_skills : string list;
      mutable items : item list;
      mutable lstatus : status;
      (* Items run at once and each ends on its own Item_finished. *)
      mutable parallel : bool;
    }

type t = { nodes : node array }

let init (entries : Workflow.entry list) =
  let node_of_entry = function
    | Workflow.Step step ->
        Step_node { skill = Workflow.step_label step; status = Pending; detail = None }
    | Workflow.Retry retry ->
        let skill =
          match retry.Workflow.retry_steps with
          | step :: _ -> Workflow.step_label step
          | [] -> "retry"
        in
        Step_node { skill; status = Pending; detail = None }
    | Workflow.For_each for_each ->
        Loop_node
          {
            body_skills =
              List.map
                (fun step -> Workflow.step_label step)
                (Workflow.steps_of_entries for_each.Workflow.body);
            items = [];
            lstatus = Pending;
            parallel = false;
          }
  in
  { nodes = Array.of_list (List.map node_of_entry entries) }

let is_terminal = function Failed | Paused -> true | _ -> false

(* Everything before the entry now starting has finished. *)
let mark_before_done nodes entry_index =
  for j = 0 to entry_index - 1 do
    match nodes.(j) with
    | Step_node s -> if not (is_terminal s.status) then s.status <- Done
    | Loop_node l ->
        if not (is_terminal l.lstatus) then l.lstatus <- Done;
        List.iter (fun it -> if not (is_terminal it.istatus) then it.istatus <- Done) l.items
  done

let format_duration seconds =
  if seconds < 60.0 then Printf.sprintf "%.1fs" seconds
  else
    let whole = int_of_float seconds in
    Printf.sprintf "%dm%02ds" (whole / 60) (whole mod 60)

(* Duration always (when both timestamps parse); cost only when the step
   spent money - "$0.00" on every command step would be noise; the tier
   when the step declared one - per-step economics are the point of tiers. *)
let detail_of_record (record : Run.step_record) =
  let duration =
    match
      (Run.epoch_of_iso8601 record.Run.started_at, Run.epoch_of_iso8601 record.Run.finished_at)
    with
    | Some started, Some finished when finished >= started ->
        Some (format_duration (finished -. started))
    | _ -> None
  in
  let cost =
    match record.Run.cost with
    | Metering.Cost_usd value when value > 0.0 -> Some (Printf.sprintf "$%.2f" value)
    | _ -> None
  in
  let timed_out = if record.Run.outcome = Run.Timeout then Some "timed out" else None in
  match List.filter_map Fun.id [ timed_out; duration; cost; record.Run.tier ] with
  | [] -> None
  | parts -> Some (String.concat "  " parts)

let apply t (event : Runner.run_event) =
  match event with
  (* Output paths matter to a log tail, not to the step tree. *)
  | Runner.Step_output _ -> ()
  | Runner.Items_resolved { entry_index; item_keys; concurrency } -> (
      match t.nodes.(entry_index) with
      | Loop_node l ->
          l.parallel <- concurrency > 1;
          l.items <-
            List.map
              (fun key ->
                {
                  key;
                  istatus = Pending;
                  idetail = None;
                  body =
                    List.map
                      (fun bskill -> { bskill; bstatus = Pending; bdetail = None })
                      l.body_skills;
                })
              item_keys
      | _ -> ())
  | Runner.Step_started { skill; position; item_key = _ } -> (
      let entry_index = position.Run.entry_index in
      mark_before_done t.nodes entry_index;
      match t.nodes.(entry_index) with
      | Step_node s -> s.status <- Working
      | Loop_node l -> (
          l.lstatus <- Working;
          match position.Run.for_each with
          | None -> ()
          | Some fe ->
              List.iteri
                (fun idx it ->
                  if idx < fe.Run.item_index && not l.parallel then (
                    it.istatus <- Done;
                    List.iter
                      (fun b -> if not (is_terminal b.bstatus) then b.bstatus <- Done)
                      it.body)
                  else if idx = fe.Run.item_index then (
                    it.istatus <- Working;
                    (* the started skill is working; body steps before it in
                       this item's run are done *)
                    let reached = ref false in
                    List.iter
                      (fun b ->
                        if b.bskill = skill then (
                          b.bstatus <- Working;
                          reached := true)
                        else if (not !reached) && not (is_terminal b.bstatus) then b.bstatus <- Done)
                      it.body))
                l.items))
  | Runner.Step_finished record ->
      let outcome_status =
        match record.Run.outcome with
        | Run.Pass -> Done
        | Run.Fail | Run.Invalid_handoff | Run.Timeout -> Failed
        | Run.Escalate -> Paused
      in
      let detail = detail_of_record record in
      (* The working leaf: the only one, or the record's item when parallel. *)
      Array.iter
        (fun node ->
          match node with
          | Step_node s ->
              if s.status = Working then (
                s.status <- outcome_status;
                s.detail <- detail)
          | Loop_node l ->
              List.iteri
                (fun idx it ->
                  if it.istatus = Working && ((not l.parallel) || record.Run.item_index = Some idx)
                  then
                    List.iter
                      (fun b ->
                        if b.bstatus = Working then (
                          b.bstatus <- outcome_status;
                          b.bdetail <- detail;
                          (* A single-step body collapses to the item line
                             when done; carry the detail up so it stays
                             visible. *)
                          if List.length it.body = 1 then it.idetail <- detail))
                      it.body)
                l.items;
              if is_terminal outcome_status && l.lstatus = Working && not l.parallel then
                l.lstatus <- outcome_status)
        t.nodes
  | Runner.Item_finished { entry_index; item_index; status } -> (
      match t.nodes.(entry_index) with
      | Loop_node l ->
          let item_status =
            match status with
            | Run.Completed -> Done
            | Run.Paused -> Paused
            | Run.Failed | Run.Aborted -> Failed
            | Run.Running -> Working
          in
          (match List.nth_opt l.items item_index with
          | Some it ->
              it.istatus <- item_status;
              if item_status = Done then
                List.iter (fun b -> if not (is_terminal b.bstatus) then b.bstatus <- Done) it.body
          | None -> ());
          (* The loop shows a stop once no other item is still working. *)
          let any status = List.exists (fun it -> it.istatus = status) l.items in
          if not (any Working) then
            if any Failed then l.lstatus <- Failed else if any Paused then l.lstatus <- Paused
      | Step_node _ -> ())

(* Rebuild the model of an existing run from its persisted record - the
   watcher's entry point, where no live events exist. Each recorded step is
   replayed through [apply] as a started/finished pair; the step's entry is
   re-derived by walking the entries in order, since the run itself is
   sequential. Adjacent entries sharing a skill label are ambiguous (the
   record carries no entry index); their steps can replay onto the earlier
   entry, a display-only blemish. *)
let restore ~(entries : Workflow.entry list) (run : Run.t) =
  let t = init entries in
  let entry_array = Array.of_list entries in
  let entry_count = Array.length entry_array in
  let labels_of_entry = function
    | Workflow.Step step -> [ Workflow.step_label step ]
    | Workflow.Retry retry -> List.map Workflow.step_label retry.Workflow.retry_steps
    | Workflow.For_each for_each ->
        List.map
          (fun step -> Workflow.step_label step)
          (Workflow.steps_of_entries for_each.Workflow.body)
  in
  let is_loop = function Workflow.For_each _ -> true | _ -> false in
  let belongs entry_index (record : Run.step_record) =
    let entry = entry_array.(entry_index) in
    List.mem record.Run.skill (labels_of_entry entry)
    && is_loop entry = Option.is_some record.Run.item_index
  in
  (* Pass 1: assign each recorded step to an entry with a forward cursor. *)
  let assigned =
    let cursor = ref 0 in
    List.map
      (fun (record : Run.step_record) ->
        (if not (belongs !cursor record) then
           let rec advance candidate =
             if candidate >= entry_count then ()
             else if belongs candidate record then cursor := candidate
             else advance (candidate + 1)
           in
           advance (!cursor + 1));
        (!cursor, record))
      run.Run.steps
  in
  (* Pass 2: item keys per loop entry - the in-flight snapshot when the run
     is paused or working inside it (it knows the pending items too),
     otherwise the keys the records mention, in first-seen order. *)
  let parallel_state entry_index =
    match run.Run.position.Run.for_each with
    | Some state when run.Run.position.Run.entry_index = entry_index && state.Run.concurrency > 1 ->
        Some state
    | _ -> None
  in
  let loop_keys entry_index =
    match run.Run.position.Run.for_each with
    | Some state when run.Run.position.Run.entry_index = entry_index ->
        List.map Items.key state.Run.items
    | _ ->
        List.rev
          (List.fold_left
             (fun keys (assigned_index, (record : Run.step_record)) ->
               match record.Run.item_key with
               | Some key when assigned_index = entry_index && not (List.mem key keys) ->
                   key :: keys
               | _ -> keys)
             [] assigned)
  in
  (* Pass 3: replay. Items must be resolved before a loop's first step. *)
  let announced = Hashtbl.create 4 in
  let announce entry_index =
    if is_loop entry_array.(entry_index) && not (Hashtbl.mem announced entry_index) then (
      Hashtbl.add announced entry_index ();
      apply t
        (Runner.Items_resolved
           {
             entry_index;
             item_keys = loop_keys entry_index;
             concurrency =
               (match parallel_state entry_index with
               | Some state -> state.Run.concurrency
               | None -> 1);
           }))
  in
  List.iter
    (fun (entry_index, (record : Run.step_record)) ->
      announce entry_index;
      let for_each =
        Option.map
          (fun item_index ->
            { Run.item_index; body_index = 0; items = []; concurrency = 1; items_in_progress = [] })
          record.Run.item_index
      in
      apply t
        (Runner.Step_started
           {
             skill = record.Run.skill;
             position = { Run.entry_index; iterations_used = 0; step_index = 0; for_each };
             item_key = record.Run.item_key;
             tier = record.Run.tier;
           });
      apply t (Runner.Step_finished record))
    assigned;
  (* A run standing inside a loop that has produced no step yet still knows
     its items - show them as pending rather than an empty forEach line. *)
  (if run.Run.position.Run.entry_index < entry_count then
     match run.Run.position.Run.for_each with
     | Some _ -> announce run.Run.position.Run.entry_index
     | None -> ());
  (* Started items no longer in progress passed; stopped ones keep status. *)
  (let entry_index = run.Run.position.Run.entry_index in
   match parallel_state entry_index with
   | None -> ()
   | Some state ->
       let in_progress index =
         List.find_opt
           (fun (item : Run.item_in_progress) -> item.Run.started_item_index = index)
           state.Run.items_in_progress
       in
       for item_index = 0 to state.Run.item_index - 1 do
         let status =
           match in_progress item_index with
           | None -> Some Run.Completed
           | Some { Run.item_status = Run.Running; _ } -> None
           | Some item -> Some item.Run.item_status
         in
         Option.iter
           (fun status -> apply t (Runner.Item_finished { entry_index; item_index; status }))
           status
       done);
  t

(* Called when the run completes: anything still Working actually finished. *)
let finalize t =
  Array.iter
    (fun node ->
      match node with
      | Step_node s -> if s.status = Working then s.status <- Done
      | Loop_node l ->
          if l.lstatus = Working then l.lstatus <- Done;
          List.iter
            (fun it ->
              if it.istatus = Working then it.istatus <- Done;
              List.iter (fun b -> if b.bstatus = Working then b.bstatus <- Done) it.body)
            l.items)
    t.nodes

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let spinner_frame tick = spinner_frames.(tick mod Array.length spinner_frames)

let glyph ~working = function
  | Pending -> "○"
  | Working -> working
  | Done -> "✓"
  | Failed -> "✗"
  | Paused -> "◉"

(* One row of the tree: how far to indent, the status (for glyph + colour),
   the label, and an optional right-aligned annotation (duration/cost). The
   caller turns these into terminal output; [render] below is the
   plain-string form used by tests. *)
type row = { indent : int; status : status; label : string; detail : string option }

let rows t =
  let out = ref [] in
  let add ?detail indent status label = out := { indent; status; label; detail } :: !out in
  Array.iter
    (fun node ->
      match node with
      | Step_node s -> add ?detail:s.detail 0 s.status s.skill
      | Loop_node l ->
          let total = List.length l.items in
          let done_count = List.length (List.filter (fun it -> it.istatus = Done) l.items) in
          let head =
            if total = 0 then "forEach" else Printf.sprintf "forEach (%d/%d)" done_count total
          in
          add 0 l.lstatus head;
          List.iter
            (fun it ->
              add ?detail:it.idetail 2 it.istatus it.key;
              if it.istatus = Working then
                List.iter (fun b -> add ?detail:b.bdetail 4 b.bstatus b.bskill) it.body)
            l.items)
    t.nodes;
  List.rev !out

(* [working] is the glyph for an in-progress step - a static marker, or a
   spinner frame the caller advances. Returns one line per rendered row. *)
let render ~working t =
  List.map
    (fun { indent; status; label; detail = _ } ->
      Printf.sprintf "%*s%s %s" indent "" (glyph ~working status) label)
    (rows t)
