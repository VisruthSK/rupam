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
val cells : t -> columns:int array -> offset:int -> limit:int -> string option array array

type histogram =
  { low : float
  ; high : float
  ; counts : int array
  ; missing : int
  }

val histogram : t -> column:int -> bins:int -> histogram

type summary =
  { histogram : histogram
  ; q1 : float option
  ; median : float option
  ; q3 : float option
  ; mean : float option
  ; sd : float option
  ; skewness : float option
  ; distinct : int
  }

val summary : t -> column:int -> summary

type frequencies =
  { missing : int
  ; distinct : int
  ; top : (string * int) list
  }

type profile =
  | Numeric of summary
  | Frequencies of frequencies

val frequencies : t -> column:int -> frequencies
val close : t -> unit
