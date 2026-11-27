type t

val open_ipc : string -> t
val rows : t -> int
val names : t -> string array

type value =
  | Missing
  | Unsupported
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string

val value : t -> int -> int -> value
val cells : t -> int -> int -> int array -> string option array array
val histogram : t -> int -> int -> float * float * int array * int
val numbers : t -> int -> int -> float array
val close : t -> unit
