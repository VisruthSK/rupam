type t

val start : Runtime.t -> Target.language -> t
val language : t -> Target.language
val send : t -> string -> unit
val evaluate : t -> string -> string
val stop : t -> unit
val cancel : t -> unit
