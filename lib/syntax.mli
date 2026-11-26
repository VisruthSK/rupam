type span = Lexing.position * Lexing.position

val sexp_of_span : span -> Sexplib0.Sexp.t

type reference =
  | Bare
  | Lexical
  | Column
[@@deriving sexp_of]

type literal =
  | Int of string
  | Float of string
  | Text of string
  | Bool of bool
  | Absent
[@@deriving sexp_of]

type unary =
  | Negate
  | Not
[@@deriving sexp_of]

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Power
  | Less
  | Greater
  | Less_equal
  | Greater_equal
  | Equal
  | Not_equal
  | And
  | Or
[@@deriving sexp_of]

type pattern =
  | Precord of string list
  | Wildcard
  | Bind of string
  | Tag of string * pattern list
  | Ptuple of pattern list
  | Pliteral of literal
  | Prange of range
  | Palt of pattern list
  | Pas of string * pattern
[@@deriving sexp_of]

and range =
  { low : literal option
  ; high : literal option
  ; inclusive : bool
  }

type annotation =
  | Arow of (string * annotation) list * string option
  | Atuple of annotation list
  | Aarrow of annotation * annotation
  | Named of string * annotation list
  | Optional of annotation
[@@deriving sexp_of]

type variant = string * (string * annotation) list [@@deriving sexp_of]

type expr =
  { form : form
  ; span : span
  }
[@@deriving sexp_of]

and arm =
  { pattern : pattern
  ; guard : expr option
  ; body : expr
  }

and piece =
  | Text_piece of string
  | Slot of slot

and slot =
  { value : expr
  ; format : string option
  ; echo : string option
  }

and form =
  | Interpolate of piece list
  | Match of expr * arm list
  | Member of expr * expr
  | Annotated of expr * annotation
  | Name of reference * string
  | Tuple of expr list
  | List of expr list
  | Record of (string * expr) list
  | Local of string * expr * expr
  | Literal of literal
  | Field of expr * string
  | Call of expr * (string option * expr) list
  | Pipe of expr * expr
  | Formula of expr option * expr
  | Range of expr * expr
  | Binary of binary * expr * expr
  | Chain of expr * (binary * expr) list
  | Fallback of expr * expr
  | Match_present of expr * arm list
  | Unary of unary * expr
  | If of expr * expr * expr
  | Lambda of string list * expr

type declaration =
  | Alias of string * string list * annotation
  | Type of string * string list * variant list
  | Import of string * string
  | Contract of string * string
  | Setting of string * string
  | Let of string * string list option * expr
  | Evaluate of expr
[@@deriving sexp_of]

type item =
  { declaration : declaration
  ; span : span
  }
[@@deriving sexp_of]

val literal_text : literal -> string
val unary_symbol : unary -> string
val binary_symbol : binary -> string
val map : (expr -> expr) -> expr -> expr
