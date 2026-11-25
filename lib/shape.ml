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
  | Constant of [ `Int of int64 | `Float of float | `Bool of bool | `String of string ]
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

let input id typ = { node = Input id; typ }

let primitive_name = function
  | Int64 -> "Int64"
  | Float64 -> "Float64"
  | Bool -> "Bool"
  | String -> "String"
;;

let type_name t = primitive_name t.primitive ^ if t.nullable then "?" else ""

let align left right =
  match left, right with
  | Scalar, shape | shape, Scalar -> shape
  | Column a, Column b when a = b -> left
  | _ -> Diagnostic.fail Row_domain_mismatch "Columns belong to different row contexts"
;;

let broadcast shape expr =
  match shape, expr.typ.shape with
  | Column id, Scalar -> { node = Broadcast (id, expr); typ = { expr.typ with shape } }
  | _ -> expr
;;

let adopt target expr =
  match target, expr.node with
  | Float64, Constant (`Int i) ->
    let f = Int64.to_float i in
    if f >= 2. ** 63. || Int64.of_float f <> i
    then
      Diagnostic.fail
        Inexact_float
        "Integer literal is not exactly representable as Float64";
    { node = Constant (`Float f); typ = { expr.typ with primitive = Float64 } }
  | primitive, Null -> { expr with typ = { expr.typ with primitive } }
  | _ -> expr
;;

let exact_float s =
  match Int64.of_string_opt s with
  | Some i when Int64.to_float i < 2. ** 63. && Int64.of_float (Int64.to_float i) = i ->
    Int64.to_float i
  | _ -> Diagnostic.fail Inexact_float (s ^ " is not exactly representable as Float64")
;;

let common left right = adopt right.typ.primitive left, adopt left.typ.primitive right
let null = { node = Null; typ = { primitive = Bool; nullable = true; shape = Scalar } }

let constant value =
  let primitive =
    match value with
    | `Int _ -> Int64
    | `Float _ -> Float64
    | `Bool _ -> Bool
    | `String _ -> String
  in
  { node = Constant value; typ = { primitive; nullable = false; shape = Scalar } }
;;

let to_float x =
  match x.typ.primitive, x.node with
  | Int64, Constant (`Int i) ->
    { node = Constant (`Float (Int64.to_float i))
    ; typ = { x.typ with primitive = Float64 }
    }
  | Int64, _ -> { node = Convert x; typ = { x.typ with primitive = Float64 } }
  | _ -> x
;;

let binary op left right =
  let left, right = common left right in
  let left, right =
    match op, left.typ.primitive, right.typ.primitive with
    | (Div | Power), _, _ -> to_float left, to_float right
    | _, Int64, Float64 -> to_float left, right
    | _, Float64, Int64 -> left, to_float right
    | _ -> left, right
  in
  let a = left.typ
  and b = right.typ in
  if a.primitive <> b.primitive
  then
    Diagnostic.fail
      Type_mismatch
      (Printf.sprintf
         "%s cannot combine %s and %s"
         (Syntax.binary_symbol op)
         (primitive_name a.primitive)
         (primitive_name b.primitive));
  let primitive =
    match op with
    | Less | Greater | Less_equal | Greater_equal | Equal | Not_equal | And | Or -> Bool
    | Add | Sub | Mul | Div | Mod | Power -> a.primitive
  in
  let shape = align a.shape b.shape in
  { node = Binary (op, broadcast shape left, broadcast shape right)
  ; typ = { primitive; shape; nullable = a.nullable || b.nullable }
  }
;;

let unary op x = { x with node = Unary (op, x) }

let apply (row : Prelude.row) args =
  let args =
    match args with
    | [ a; b ] ->
      let a, b = common a b in
      [ a; b ]
    | args -> args
  in
  let shape = List.fold_left (fun s a -> align s a.typ.shape) Scalar args in
  let args = List.map (broadcast shape) args in
  let first = (List.hd args).typ in
  let primitive =
    match row with
    | Abs | Pmin | Pmax | Lower | Upper | Trim -> first.primitive
    | Sign | Length -> Int64
    | Sqrt | Exp | Log | Log2 | Log10 | Floor | Ceiling | Trunc | Round -> Float64
    | Present | Absent | Starts | Ends | Detect -> Bool
  in
  let nullable =
    match row with
    | Present | Absent -> false
    | _ -> List.exists (fun a -> a.typ.nullable) args
  in
  { node = Apply (row, args); typ = { primitive; nullable; shape } }
;;

let reduce ?policy (reduction : Prelude.reduction) x =
  let name = Prelude.name (Reduction reduction) in
  match x.typ.shape with
  | Scalar ->
    Diagnostic.fail Column_required (name ^ " needs a column, not a single value")
  | Column _ ->
    let policy =
      match policy, x.typ.nullable with
      | Some p, true -> p
      | _, false -> Skip
      | None, true ->
        Diagnostic.fail
          Missing_policy_required
          (name
           ^ " reads a column that can be missing. Say what to do with missing values: \
              missing = skip ignores them, missing = propagate makes the result missing, \
              missing = error stops the run")
    in
    let primitive, nullable =
      match reduction with
      | Sum | Min | Max -> x.typ.primitive, true
      | Mean | Median | Var | Sd -> Float64, true
      | Distinct | Count -> Int64, policy = Propagate
    in
    let x =
      match reduction with
      | Mean | Median | Var | Sd -> to_float x
      | Sum | Min | Max | Distinct | Count -> x
    in
    { node = Reduce (reduction, policy, x)
    ; typ = { primitive; nullable; shape = Scalar }
    }
;;

let coalesce x fallback =
  let x, fallback = common x fallback in
  let shape = align x.typ.shape fallback.typ.shape in
  { node = Coalesce (broadcast shape x, broadcast shape fallback)
  ; typ = { x.typ with shape; nullable = x.typ.nullable && fallback.typ.nullable }
  }
;;

let choose test yes no =
  if test.typ.nullable
  then Diagnostic.fail Optional_condition "if needs a condition that is never missing";
  let yes, no = common yes no in
  let shape = align test.typ.shape (align yes.typ.shape no.typ.shape) in
  { node = Choose (broadcast shape test, broadcast shape yes, broadcast shape no)
  ; typ = { yes.typ with shape; nullable = yes.typ.nullable || no.typ.nullable }
  }
;;

let filter context policy predicate =
  if predicate.typ.shape <> Column context
  then
    Diagnostic.fail Row_domain_mismatch "filter needs a condition on this table's columns";
  match predicate.typ.nullable, policy with
  | false, _ -> predicate
  | true, Some Skip -> coalesce predicate (constant (`Bool false))
  | true, Some Propagate -> coalesce predicate (constant (`Bool true))
  | true, Some Fail ->
    { node = Required predicate; typ = { predicate.typ with nullable = false } }
  | true, None ->
    Diagnostic.fail
      Optional_condition
      "This condition is missing where its inputs are missing. Say what happens to those \
       rows: missing = skip drops them, missing = propagate keeps them, missing = error \
       stops the run"
;;

let member x items =
  let items = List.map (adopt x.typ.primitive) items in
  if List.exists (fun i -> i.typ.shape <> Scalar) items
  then Diagnostic.fail Wrong_arguments "%in% needs a list of values";
  { node = Member (x, items); typ = { x.typ with primitive = Bool } }
;;

let count =
  { node = Count; typ = { primitive = Int64; nullable = false; shape = Scalar } }
;;

let rec exists f e =
  f e
  ||
  match e.node with
  | Constant _ | Null | Input _ | Count -> false
  | Choose (a, b, c) -> List.exists (exists f) [ a; b; c ]
  | Coalesce (a, b) | Binary (_, a, b) -> exists f a || exists f b
  | Apply (_, xs) -> List.exists (exists f) xs
  | Member (x, xs) -> List.exists (exists f) (x :: xs)
  | Polars_sqrt x
  | Unary (_, x)
  | Required x
  | Broadcast (_, x)
  | Reduce (_, _, x)
  | Convert x -> exists f x
  | Text parts ->
    List.exists
      (function
        | Shown (x, _) -> exists f x
        | Fixed _ -> false)
      parts
;;

let nonfinite =
  exists (fun e ->
    e.typ.primitive = Float64
    &&
    match e.node with
    | Binary ((Add | Sub | Mul | Div | Power), _, _)
    | Apply ((Exp | Log | Log2 | Log10 | Sqrt), _)
    | Reduce ((Sum | Mean | Var | Sd), _, _) -> true
    | _ -> false)
;;

let overflows =
  exists (fun e ->
    e.typ.primitive = Int64
    &&
    match e.node with
    | Binary ((Add | Sub | Mul), _, _) | Unary (Negate, _) | Reduce (Sum, _, _) -> true
    | _ -> false)
;;

let text parts =
  let shown =
    List.filter_map
      (function
        | Shown (x, _) -> Some x
        | Fixed _ -> None)
      parts
  in
  let shape = List.fold_left (fun s (x : expr) -> align s x.typ.shape) Scalar shown in
  let parts =
    List.map
      (function
        | Shown (x, f) -> Shown (broadcast shape x, f)
        | fixed -> fixed)
      parts
  in
  { node = Text parts
  ; typ =
      { primitive = String
      ; nullable = List.exists (fun (x : expr) -> x.typ.nullable) shown
      ; shape
      }
  }
;;
