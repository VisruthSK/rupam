type t =
  { wider : bool
  ; ids : (string * Shape.expr) list
  ; values : (string * Shape.expr) list
  ; key : string
  ; value : string
  ; labels : string list
  }

val parse
  :  (string * Shape.expr) list
  -> string
  -> (string option * Syntax.expr) list
  -> t * (string * Shape.typ) list

val execute
  :  t
  -> rows:int
  -> eval:(Shape.expr -> Column_eval.value array)
  -> int * Column_eval.value array list
