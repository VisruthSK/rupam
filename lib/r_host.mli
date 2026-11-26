val names : unit -> Host.names
val statements : Host.names -> TransformIR.item list -> R_syntax.statement list
val script : TransformIR.item list -> string * Document.location list
val result : Host.names -> TransformIR.table -> id:string -> R_syntax.statement
val page : id:string -> offset:int -> limit:int -> path:string -> R_syntax.statement
val release : id:string -> R_syntax.statement
