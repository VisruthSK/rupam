type t =
  { command : string array
  ; prelude : string
  }

val resolve : Project.t -> Target.language -> t
val available : t -> Target.language -> bool
val suffix : Target.language -> string
val run : t -> Target.language -> string -> unit
