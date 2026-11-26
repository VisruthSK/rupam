type value

val display : value -> string

val run
  :  (int * value) list
  -> Core.item list
  -> (int * value) list * (Core.expr * value) list
