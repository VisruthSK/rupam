type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Power
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Equal
  | Not_equal
  | Both
  | Either
  | And
  | Or
  | Is
  | Is_not
  | In

type unary =
  | Negate
  | Invert
  | Not

type expr =
  | Int of string
  | Float of string
  | Str of string
  | Bool of bool
  | Null
  | Name of string
  | Attribute of expr * string
  | Call of expr * argument list
  | Binary of operator * expr * expr
  | Unary of unary * expr
  | List of expr list
  | Dict of (expr * expr) list
  | Tuple of expr list
  | Subscript of expr * expr
  | Walrus of string * expr
  | Lambda of string list * expr
  | If of expr * expr * expr
  | Chain of expr * link list
  | Located of Syntax.span * expr

and link =
  { span : Syntax.span option
  ; name : string
  ; arguments : argument list
  }

and argument =
  | Positional of expr
  | Keyword of string * expr

type statement =
  | Import of string * string option
  | Assign of string * expr
  | Expression of expr
  | Verbatim of string

val call : string -> expr list -> expr
val meth : expr -> string -> argument list -> expr
val names : string list -> expr
val link : ?span:Syntax.span -> string -> argument list -> link
val float : float -> string
val expr : Document.t -> int -> int -> expr -> unit
val program : statement list -> string * Document.location list
val render : expr -> string
