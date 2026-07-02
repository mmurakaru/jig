module type S = sig
  val resolve :
    config:Config.t -> skill:string -> (string list, string) result
end

module Default : S = struct
  let resolve ~config ~skill:_ = Ok config.Config.harness
end
