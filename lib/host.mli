type names

val names : string list -> names
val name : names -> Core.id -> string
val descriptor : Types.t -> Yojson.Basic.t
val float : Core.expr -> bool
val risky : Core.expr -> bool
val nonportable : Core.item list -> string list
