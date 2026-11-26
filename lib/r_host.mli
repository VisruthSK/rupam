val names : unit -> Host.names
val statements : Host.names -> Core.item list -> R_syntax.statement list
val script : Core.item list -> string * Document.location list

val preview
  :  Host.names
  -> Core.table
  -> offset:int
  -> limit:int
  -> path:string
  -> R_syntax.statement
