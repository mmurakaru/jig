module type S = sig
  val record :
    run_id:string -> skill:string -> exec_result:Executor.exec_result -> unit
end

module Noop : S = struct
  let record ~run_id:_ ~skill:_ ~exec_result:_ = ()
end
