val names : unit -> Host.names
val table : Host.names -> Core.table -> Python_syntax.expr
val statements : Host.names -> Core.item list -> Python_syntax.statement list
val script : Core.item list -> string * Document.location list

val preview
  :  Host.names
  -> Core.table
  -> offset:int
  -> limit:int
  -> path:string
  -> Python_syntax.statement
