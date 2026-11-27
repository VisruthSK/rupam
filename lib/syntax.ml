open Sexplib0.Sexp_conv

type span = Lexing.position * Lexing.position

let sexp_of_span ((start, finish) : span) =
  let point (p : Lexing.position) =
    Printf.sprintf "%d:%d" p.pos_lnum (p.pos_cnum - p.pos_bol + 1)
  in
  Sexplib0.Sexp.Atom (point start ^ "-" ^ point finish)
;;

type reference =
  | Bare
  | Lexical
  | Column
[@@deriving sexp_of]

type label =
  | Plain of string
  | Quoted of string
[@@deriving sexp_of]

let label_name = function
  | Plain name | Quoted name -> name
;;

type tap =
  | Print
  | View
  | Schema
  | Hist
  | Bar
[@@deriving sexp_of]

let taps = [ "print", Print; "view", View; "schema", Schema; "hist", Hist; "bar", Bar ]
let tap_name tap = fst (List.find (fun (_, t) -> t = tap) taps)

type literal =
  | Int of string
  | Float of string
  | Text of string
  | Bool of bool
  | Date of string
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
  | Approx
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
  { low : bound option
  ; high : bound option
  }

and bound =
  { value : literal
  ; closed : bool
  }

type annotation =
  | Arow of (string * annotation) list * string option
  | Atuple of annotation list
  | Alist of annotation
  | Aarrow of annotation * annotation
  | Named of string * annotation list
  | Optional of annotation
[@@deriving sexp_of]

type variant = string * (string * annotation) list [@@deriving sexp_of]

let american name =
  match name with
  | "summarise" -> "summarize"
  | "standardise" -> "standardize"
  | "normalise" -> "normalize"
  | "categorise" -> "categorize"
  | "colour" -> "color"
  | "colours" -> "colors"
  | name -> name
;;

type expr =
  { form : form
  ; span : span
  }
[@@deriving sexp_of]

and interval =
  { first : expr
  ; first_closed : bool
  ; last : expr
  ; last_closed : bool
  }

and index =
  | Element of expr
  | From of expr * bool
  | Upto of expr * bool

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
  | Call of expr * (label option * expr) list
  | Pipe of expr * expr
  | Formula of expr option * expr
  | Range of expr * expr
  | Binary of binary * expr * expr
  | Chain of expr * (binary * expr) list
  | Fallback of expr * expr
  | Sequence of interval
  | Index of expr * index
  | Last
  | Tap of tap * (label option * expr) list
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

let literal_text = function
  | Int s | Float s -> s
  | Text s ->
    Printf.sprintf "%S" s
    |> String.split_on_char '{'
    |> String.concat "{{"
    |> String.split_on_char '}'
    |> String.concat "}}"
  | Date s -> "d\"" ^ s ^ "\""
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
  | Approx -> "~="
  | And -> "and"
  | Or -> "or"
;;

let map f x =
  let arm a = { a with guard = Option.map f a.guard; body = f a.body } in
  let form =
    match x.form with
    | Match (s, arms) -> Match (f s, List.map arm arms)
    | Interpolate pieces ->
      Interpolate
        (List.map
           (function
             | Slot s -> Slot { s with value = f s.value }
             | piece -> piece)
           pieces)
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
    | Fallback (a, b) -> Fallback (f a, f b)
    | Sequence r -> Sequence { r with first = f r.first; last = f r.last }
    | Index (x, Element i) -> Index (f x, Element (f i))
    | Index (x, From (i, closed)) -> Index (f x, From (f i, closed))
    | Index (x, Upto (i, inclusive)) -> Index (f x, Upto (f i, inclusive))
    | Last -> Last
    | Tap (t, args) -> Tap (t, List.map (fun (n, x) -> n, f x) args)
    | Match_present (x, arms) ->
      Match_present
        ( f x
        , List.map
            (fun a -> { a with guard = Option.map f a.guard; body = f a.body })
            arms )
    | Chain (a, links) -> Chain (f a, List.map (fun (op, x) -> op, f x) links)
    | Unary (op, a) -> Unary (op, f a)
    | If (c, a, b) -> If (f c, f a, f b)
    | Lambda (ps, b) -> Lambda (ps, f b)
    | (Name _ | Literal _) as form -> form
  in
  { x with form }
;;
