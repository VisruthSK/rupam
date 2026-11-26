open Rupam

type value

val display : value -> string
val rows : value -> (string list * string list list) option

val run
  :  (int * value) list
  -> TransformIR.item list
  -> (int * value) list * (TransformIR.expr * value) list
