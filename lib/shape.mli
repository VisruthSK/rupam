type primitive =
  | Int64
  | Float64
  | Bool
  | String

type shape =
  | Scalar
  | Column of int

type typ =
  { primitive : primitive
  ; nullable : bool
  ; shape : shape
  }

type operator = Syntax.binary =
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

type policy =
  | Skip
  | Propagate
  | Fail

type node =
  | Constant of [ `Bool of bool | `Float of float | `Int of int64 | `String of string ]
  | Null
  | Choose of expr * expr * expr
  | Coalesce of expr * expr
  | Polars_sqrt of expr
  | Apply of Prelude.row * expr list
  | Unary of Syntax.unary * expr
  | Required of expr
  | Input of int
  | Broadcast of int * expr
  | Binary of operator * expr * expr
  | Reduce of Prelude.reduction * policy * expr
  | Member of expr * expr list
  | Convert of expr
  | Count
  | Text of text list

and text =
  | Fixed of string
  | Shown of expr * string option

and expr =
  { node : node
  ; typ : typ
  }

val input : int -> typ -> expr
val type_name : typ -> string
val broadcast : shape -> expr -> expr
val exact_float : string -> float
val null : expr

val constant
  :  [ `Bool of bool | `Float of float | `Int of int64 | `String of string ]
  -> expr

val binary : operator -> expr -> expr -> expr
val unary : Syntax.unary -> expr -> expr
val apply : Prelude.row -> expr list -> expr
val reduce : ?policy:policy -> Prelude.reduction -> expr -> expr
val coalesce : expr -> expr -> expr
val choose : expr -> expr -> expr -> expr
val filter : int -> policy option -> expr -> expr
val member : expr -> expr list -> expr
val count : expr
val exists : (expr -> bool) -> expr -> bool
val nonfinite : expr -> bool
val overflows : expr -> bool
val text : text list -> expr
