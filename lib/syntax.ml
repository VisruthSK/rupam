type span = Lexing.position * Lexing.position

type reference =
  | Bare
  | Lexical
  | Column

type pattern =
  | Precord of string list
  | Wildcard
  | Bind of string
  | Tag of string * pattern list
  | Ptuple of pattern list
  | Pliteral of string

type annotation =
  | Arow of (string * annotation) list * string option
  | Atuple of annotation list
  | Aarrow of annotation * annotation
  | Named of string * annotation list
  | Optional of annotation

type variant = string * (string * annotation) list

type expr =
  { form : form
  ; span : span
  }

and form =
  | Match of expr * (pattern * expr) list
  | Annotated of expr * annotation
  | Name of reference * string
  | Tuple of expr list
  | List of expr list
  | Record of (string * expr) list
  | Local of string * expr * expr
  | Literal of string
  | Field of expr * string
  | Call of expr * (string option * expr) list
  | Binary of string * expr * expr
  | Unary of string * expr
  | If of expr * expr * expr
  | Lambda of string list * expr

type declaration =
  | Alias of string * string list * annotation
  | Type of string * string list * variant list
  | Import of string * string
  | Contract of string * string
  | Let of string * string list option * expr
  | Evaluate of expr
