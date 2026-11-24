type span = Lexing.position * Lexing.position
type reference = Bare | Lexical | Column
type expr = { form : form; span : span }
and form =
  | Name of reference * string
  | Literal of string
  | Field of expr * string
  | Call of expr * (string option * expr) list
  | Binary of string * expr * expr
  | Unary of string * expr
  | If of expr * expr * expr
  | Lambda of string list * expr
type declaration =
  | Contract of string * string
  | Let of string * string list option * expr
  | Evaluate of expr
