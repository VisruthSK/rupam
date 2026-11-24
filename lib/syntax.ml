type span = Lexing.position * Lexing.position

type reference =
  | Bare
  | Lexical
  | Column

type literal =
  | Int of string
  | Float of string
  | Text of string
  | Bool of bool
  | Absent

type unary =
  | Negate
  | Not

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | Greater
  | Less_equal
  | Greater_equal
  | Equal
  | Not_equal
  | And
  | Or

type pattern =
  | Precord of string list
  | Wildcard
  | Bind of string
  | Tag of string * pattern list
  | Ptuple of pattern list
  | Pliteral of literal

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
  | Literal of literal
  | Field of expr * string
  | Call of expr * (string option * expr) list
  | Pipe of expr * expr
  | Range of expr * expr
  | Binary of binary * expr * expr
  | Unary of unary * expr
  | If of expr * expr * expr
  | Lambda of string list * expr

type declaration =
  | Alias of string * string list * annotation
  | Type of string * string list * variant list
  | Import of string * string
  | Contract of string * string
  | Let of string * string list option * expr
  | Evaluate of expr

let literal_text = function
  | Int s | Float s -> s
  | Text s -> Printf.sprintf "%S" s
  | Bool b -> string_of_bool b
  | Absent -> "None"
;;

let unary_symbol = function
  | Negate -> "-"
  | Not -> "not"
;;

let binary_symbol = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Less -> "<"
  | Greater -> ">"
  | Less_equal -> "<="
  | Greater_equal -> ">="
  | Equal -> "=="
  | Not_equal -> "!="
  | And -> "and"
  | Or -> "or"
;;

let comparison = function
  | Less | Greater | Less_equal | Greater_equal | Equal | Not_equal -> true
  | Add | Sub | Mul | Div | Mod | And | Or -> false
;;
