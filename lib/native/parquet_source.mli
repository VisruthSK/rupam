val columns : string -> Input.column list

type cell =
  | Missing
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Date of int

val rows : string -> (string * cell array) list
