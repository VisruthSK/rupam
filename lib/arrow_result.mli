type t

val open_ipc : string -> t
val rows : t -> int
val names : t -> string array
val cells : t -> offset:int -> limit:int -> string option array array

type histogram =
  { low : float
  ; high : float
  ; counts : int array
  ; missing : int
  }

val histogram : t -> column:int -> bins:int -> histogram
val close : t -> unit
