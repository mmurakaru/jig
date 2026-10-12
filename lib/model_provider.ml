module type S = sig
  val resolve :
    config:Config.t ->
    skill:string ->
    tier:string option ->
    (string list, string) result
end

(* The wrapper (e.g. an OS sandbox like srt) is configuration, prepended to
   every harness invocation; workflows stay ignorant of it.

   A step's tier selects a named command from config.tiers. A tier the
   config does not map falls back to the default harness: the tier is a
   local cost concern, and a workflow written elsewhere must still run
   here - `jig validate` surfaces the unmapped name. *)
module Default : S = struct
  let resolve ~config ~skill:_ ~tier =
    let harness =
      match tier with
      | Some name -> (
          match List.assoc_opt name config.Config.tiers with
          | Some command -> command
          | None -> config.Config.harness)
      | None -> config.Config.harness
    in
    Ok (config.Config.wrapper @ harness)
end
