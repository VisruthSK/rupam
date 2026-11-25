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
  | Power
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
  | Prange of range
  | Palt of pattern list
  | Pas of string * pattern

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

type variant = string * (string * annotation) list

type expr =
  { form : form
  ; span : span
  }

and arm =
  { pattern : pattern
  ; guard : expr option
  ; body : expr
  }

and form =
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
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
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
  | Power -> "^"
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
  | Add | Sub | Mul | Div | Mod | Power | And | Or -> false
;;

let map f x =
  let arm a = { a with guard = Option.map f a.guard; body = f a.body } in
  let form =
    match x.form with
    | Match (s, arms) -> Match (f s, List.map arm arms)
    | Member (a, b) -> Member (f a, f b)
    | Annotated (a, t) -> Annotated (f a, t)
    | Tuple xs -> Tuple (List.map f xs)
    | List xs -> List (List.map f xs)
    | Record fs -> Record (List.map (fun (n, v) -> n, f v) fs)
    | Local (n, a, b) -> Local (n, f a, f b)
    | Field (a, n) -> Field (f a, n)
    | Call (g, args) -> Call (f g, List.map (fun (n, v) -> n, f v) args)
    | Pipe (a, b) -> Pipe (f a, f b)
    | Formula (a, b) -> Formula (Option.map f a, f b)
    | Range (a, b) -> Range (f a, f b)
    | Binary (op, a, b) -> Binary (op, f a, f b)
    | Unary (op, a) -> Unary (op, f a)
    | If (c, a, b) -> If (f c, f a, f b)
    | Lambda (ps, b) -> Lambda (ps, f b)
    | (Name _ | Literal _) as form -> form
  in
  { x with form }
;;
