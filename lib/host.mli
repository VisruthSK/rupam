type names

val names : string list -> names
val name : names -> TransformIR.id -> string
val column_kind : Types.t -> string
val descriptor : Types.t -> Yojson.Basic.t
val float : TransformIR.expr -> bool
val risky : TransformIR.expr -> bool
val nonportable : TransformIR.item list -> string list
