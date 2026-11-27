type t

external open_ipc : string -> t = "rupam_arrow_open"
external rows : t -> int = "rupam_arrow_rows"
external names : t -> string array = "rupam_arrow_names"

type value =
  | Missing
  | Unsupported
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string

external value : t -> int -> int -> value = "rupam_arrow_value"

external cells
  :  t
  -> int
  -> int
  -> int array
  -> string option array array
  = "rupam_arrow_cells"

external histogram
  :  t
  -> int
  -> int
  -> float * float * int array * int
  = "rupam_arrow_histogram"

external numbers : t -> int -> int -> float array = "rupam_arrow_numbers"
external close : t -> unit = "rupam_arrow_close"
