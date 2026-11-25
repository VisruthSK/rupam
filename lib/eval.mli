type value =
  | Int of int64
  | Float of float
  | Bool of bool
  | Text of string
  | Tuple of value list
  | List of value list
  | Record of (string * value) list
  | Variant of string * value list
  | Constructor of string * int
  | Option of value option
  | Closure of string list * Syntax.expr * (string * value) list

val display : value -> string

val run
  :  (string * value) list
  -> Syntax.declaration list
  -> (string * value) list * value option
