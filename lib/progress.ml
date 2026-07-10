(* Pure rendering of a run's progress as a step tree. The runner's
   lifecycle events (Runner.run_event) drive the model via [apply]; [render]
   turns it into display lines. The working glyph (a static marker, or an
   animated spinner frame) and any colour are supplied by the caller, so
   this module stays pure and testable.

   forEach density: top-level steps are one line each; inside a loop, done
   and pending items are one line each, and only the active item expands to
   show its body steps. *)

type status = Pending | Working | Done | Failed | Paused

type body_step = { bskill : string; mutable bstatus : status }
type item = { key : string; mutable istatus : status; body : body_step list }

type node =
  | Step_node of { skill : string; mutable status : status }
  | Loop_node of {
      body_skills : string list;
      mutable items : item list;
      mutable lstatus : status;
    }

type t = { nodes : node array }

let init (entries : Workflow.entry list) =
  let node_of_entry = function
    | Workflow.Step step -> Step_node { skill = step.Workflow.skill; status = Pending }
    | Workflow.Retry retry ->
        let skill =
          match retry.Workflow.retry_steps with
          | step :: _ -> step.Workflow.skill
          | [] -> "retry"
        in
        Step_node { skill; status = Pending }
    | Workflow.For_each for_each ->
        Loop_node
          {
            body_skills =
              List.map
                (fun step -> step.Workflow.skill)
                (Workflow.steps_of_entries for_each.Workflow.body);
            items = [];
            lstatus = Pending;
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
        List.iter
          (fun it -> if not (is_terminal it.istatus) then it.istatus <- Done)
          l.items
  done

let apply t (event : Runner.run_event) =
  match event with
  | Runner.Items_resolved { entry_index; item_keys } -> (
      match t.nodes.(entry_index) with
      | Loop_node l ->
          l.items <-
            List.map
              (fun key ->
                {
                  key;
                  istatus = Pending;
                  body =
                    List.map
                      (fun bskill -> { bskill; bstatus = Pending })
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
                  if idx < fe.Run.item_index then (
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
                        else if not !reached && not (is_terminal b.bstatus) then
                          b.bstatus <- Done)
                      it.body))
                l.items))
  | Runner.Step_finished record ->
      let outcome_status =
        match record.Run.outcome with
        | Run.Pass -> Done
        | Run.Fail | Run.Invalid_handoff -> Failed
        | Run.Escalate -> Paused
      in
      (* Apply to the leaf currently working; the run is sequential, so
         exactly one leaf is Working when a step finishes. *)
      Array.iter
        (fun node ->
          match node with
          | Step_node s -> if s.status = Working then s.status <- outcome_status
          | Loop_node l ->
              List.iter
                (fun it ->
                  if it.istatus = Working then
                    List.iter
                      (fun b -> if b.bstatus = Working then b.bstatus <- outcome_status)
                      it.body)
                l.items;
              if is_terminal outcome_status && l.lstatus = Working then
                l.lstatus <- outcome_status)
        t.nodes

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
              List.iter
                (fun b -> if b.bstatus = Working then b.bstatus <- Done)
                it.body)
            l.items)
    t.nodes

let glyph ~working = function
  | Pending -> "○"
  | Working -> working
  | Done -> "✓"
  | Failed -> "✗"
  | Paused -> "◉"

(* One row of the tree: how far to indent, the status (for glyph + colour),
   and the label. The caller turns these into terminal output; [render]
   below is the plain-string form used by tests. *)
type row = { indent : int; status : status; label : string }

let rows t =
  let out = ref [] in
  let add indent status label = out := { indent; status; label } :: !out in
  Array.iter
    (fun node ->
      match node with
      | Step_node s -> add 0 s.status s.skill
      | Loop_node l ->
          let total = List.length l.items in
          let done_count =
            List.length (List.filter (fun it -> it.istatus = Done) l.items)
          in
          let head =
            if total = 0 then "forEach"
            else Printf.sprintf "forEach (%d/%d)" done_count total
          in
          add 0 l.lstatus head;
          List.iter
            (fun it ->
              add 2 it.istatus it.key;
              if it.istatus = Working then
                List.iter (fun b -> add 4 b.bstatus b.bskill) it.body)
            l.items)
    t.nodes;
  List.rev !out

(* [working] is the glyph for an in-progress step - a static marker, or a
   spinner frame the caller advances. Returns one line per rendered row. *)
let render ~working t =
  List.map
    (fun { indent; status; label } ->
      Printf.sprintf "%*s%s %s" indent "" (glyph ~working status) label)
    (rows t)
