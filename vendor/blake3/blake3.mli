val file : ?parallel:(int -> (int -> unit) -> unit) -> string -> string
val string : string -> string

type hasher

val hasher : unit -> hasher
val feed : hasher -> Bytes.t -> int -> int -> unit
val digest : hasher -> string
