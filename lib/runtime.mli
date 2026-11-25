type language =
  | Python
  | R

type t =
  { command : string array
  ; prelude : string
  }

val of_dialect : Project.dialect -> language
val resolve : Project.t -> language -> t
val suffix : language -> string
val run : t -> language -> string -> unit
