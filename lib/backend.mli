module type BACKEND = sig
  val names : unit -> Host.names
  val preamble : string
  val script : TransformIR.item list -> string * Document.location list
  val statements : Host.names -> TransformIR.item list -> string
  val result : Host.names -> TransformIR.table -> path:string -> string
  val table : Host.names -> TransformIR.table -> string
end

module R_dplyr : BACKEND
module Python_polars : BACKEND

val of_target : Target.t -> (module BACKEND)
