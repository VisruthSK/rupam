type value

val display : value -> string

val run
  :  (int * value) list
  -> TransformIR.item list
  -> (int * value) list * (TransformIR.expr * value) list
