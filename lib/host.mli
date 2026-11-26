type names

val names : string list -> names
val name : names -> TransformIR.id -> string
val column_kind : Types.t -> string
val descriptor : Types.t -> Yojson.Basic.t
val nonportable : TransformIR.item list -> string list
