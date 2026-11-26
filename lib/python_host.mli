val names : unit -> Host.names
val table : Host.names -> TransformIR.table -> Python_syntax.expr
val statements : Host.names -> TransformIR.item list -> Python_syntax.statement list
val script : TransformIR.item list -> string * Document.location list
val result : Host.names -> TransformIR.table -> path:string -> Python_syntax.statement
