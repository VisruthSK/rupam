type location =
  { line : int
  ; column : int
  ; source : Syntax.span
  }

type t

val create : unit -> t
val text : t -> string -> unit
val newline : t -> int -> unit
val mark : t -> Syntax.span -> unit
val fits : t -> (t -> unit) -> bool
val separated : t -> string -> ('a -> unit) -> 'a list -> unit
val contents : t -> string * location list
