val names : Target.language -> Host.names
val script : Target.t -> TransformIR.item list -> string * Document.location list
val preamble : Target.t -> string
val statements : Target.t -> Host.names -> TransformIR.item list -> string
val result : Target.t -> Host.names -> TransformIR.table -> path:string -> string
val table : Target.t -> Host.names -> TransformIR.table -> string
