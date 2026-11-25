type setting =
  | Missing
  | Group_missing

type binding =
  | Module of string
  | Value of Shape.expr
  | Function of string list * Syntax.expr * env
  | Default of setting * Shape.policy

and env = (string * binding) list

val setting_key : setting -> string
val default : (string * binding) list -> setting -> Shape.policy option
val closest : string -> string list -> string option
val did_you_mean : string option -> string
val column : string -> (string * 'a) list -> 'a
val policy_of_name : string -> Shape.policy
val policy : Syntax.expr -> Shape.policy

val expression
  :  ?missing:Shape.policy
  -> env
  -> (string * Shape.expr) list
  -> Syntax.expr
  -> Shape.expr

val define : env -> Syntax.declaration -> (string * binding) list

val mutate
  :  env
  -> (string * Shape.expr) list
  -> (string option * Syntax.expr) list
  -> (string * Shape.expr) list
