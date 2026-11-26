val names : Target.language -> Host.names
val script : Target.t -> TransformIR.item list -> string * Document.location list
val preamble : Target.t -> string
val statements : Target.t -> Host.names -> TransformIR.item list -> string

val preview
  :  Target.t
  -> Host.names
  -> TransformIR.table
  -> offset:int
  -> limit:int
  -> path:string
  -> string

val table : Target.t -> Host.names -> TransformIR.table -> string
