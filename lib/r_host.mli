val names : unit -> Host.names
val statements : Host.names -> TransformIR.item list -> R_syntax.statement list
val script : TransformIR.item list -> string * Document.location list

val preview
  :  Host.names
  -> TransformIR.table
  -> offset:int
  -> limit:int
  -> path:string
  -> R_syntax.statement
