type value =
  | I of int64
  | F of float
  | B of bool
  | S of string
  | Missing

type result =
  | One of value
  | Many of value array

val aggregate : Prelude.reduction -> value array -> value
val evaluate : rows:int -> input:(int -> result) -> Shape.expr -> result
