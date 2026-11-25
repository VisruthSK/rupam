type constraint_ =
  | Any
  | Numeric

type typ =
  | Variant of string * typ list
  | List of typ
  | Option of typ
  | Tuple of typ list
  | Record of (string * typ) list * typ option
  | Int
  | Float
  | Bool
  | String
  | Arrow of typ list * typ
  | Var of cell ref

and cell =
  | Free of int * constraint_
  | Link of typ

type scheme = cell ref list * typ

type env =
  { values : (string * scheme) list
  ; types : (string * scheme) list
  ; variants : (string * (string * scheme) list) list
  ; columns : (string * typ) list
  }

val extend : env -> (string * scheme) list -> env
val converted : Syntax.expr -> bool
val promoted : Syntax.expr -> bool
val lifted : Syntax.expr -> bool list option
val operand : Syntax.expr -> typ option
val shown : Syntax.expr -> (typ * bool) list
val unsettled : Syntax.expr -> bool
val display : typ -> string
val generalize : int -> typ -> scheme
val signature : Prelude.builtin -> scheme
val empty : env
val rows : env -> (string * typ) list -> env
val expression : env -> int -> Syntax.expr -> typ
val declaration : env -> Syntax.declaration -> env * typ
val program : env -> Syntax.declaration list -> env
val descriptor : typ -> Yojson.Basic.t
