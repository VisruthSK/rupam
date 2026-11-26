module type BACKEND = sig
  val names : unit -> Host.names
  val preamble : string
  val script : TransformIR.item list -> string * Document.location list
  val statements : Host.names -> TransformIR.item list -> string
  val result : Host.names -> TransformIR.table -> path:string -> string
  val table : Host.names -> TransformIR.table -> string
end

module R_dplyr = struct
  let names = R_host.names
  let preamble = Adapters.r ^ Adapters.dplyr
  let script = R_host.script
  let render statements = fst (R_syntax.program statements)
  let statements names items = render (R_host.statements names items)
  let result names table ~path = render [ R_host.result names table ~path ]
  let table names table = R_syntax.render (Dplyr.table names table)
end

module Python_polars = struct
  let names = Python_host.names
  let preamble = Adapters.python ^ Adapters.polars
  let script = Python_host.script
  let render statements = fst (Python_syntax.program statements)
  let statements names items = render (Python_host.statements names items)
  let result names table ~path = render [ Python_host.result names table ~path ]
  let table names table = Python_syntax.render (Python_host.table names table)
end

let of_target (target : Target.t) : (module BACKEND) =
  match target.dialect with
  | Dplyr -> (module R_dplyr)
  | Polars -> (module Python_polars)
;;
