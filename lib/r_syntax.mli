type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Power
  | Modulo
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Equal
  | Not_equal
  | Both
  | Either
  | Both_scalar
  | Either_scalar
  | In_set
  | Pipe
  | Tilde

type expr =
  | Number of string
  | String of string
  | Logical of bool
  | Null
  | Na
  | Name of string
  | Namespaced of string * string
  | Call of expr * argument list
  | Binary of operator * expr * expr
  | Not of expr
  | Negate of expr
  | Element of expr * expr
  | Dollar of expr * string
  | Function of string list * expr
  | If of expr * expr * expr
  | Block of (string * expr) list * expr
  | Located of Syntax.span * expr

and argument =
  | Positional of expr
  | Named of string * expr

type statement =
  | Assign of string * expr
  | Expression of expr
  | Verbatim of string

val call : string -> expr list -> expr
val qualified : string -> string -> argument list -> expr
val strings : string list -> expr
val number : float -> string
val pipe : expr -> expr -> expr
val program : statement list -> string * Document.location list
val render : expr -> string
