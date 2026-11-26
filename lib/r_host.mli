val names : unit -> Host.names
val statements : Host.names -> TransformIR.item list -> R_syntax.statement list
val script : TransformIR.item list -> string * Document.location list
val result : Host.names -> TransformIR.table -> path:string -> R_syntax.statement
