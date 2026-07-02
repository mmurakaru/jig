module type S = sig
  val resolve :
    config:Config.t -> skill:string -> (string list, string) result
end

(* The wrapper (e.g. an OS sandbox like srt) is configuration, prepended to
   every harness invocation; workflows stay ignorant of it. *)
module Default : S = struct
  let resolve ~config ~skill:_ =
    Ok (config.Config.wrapper @ config.Config.harness)
end
