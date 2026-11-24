type primitive = Int64 | Float64 | Bool | String
type shape = Scalar | Column of int
type typ = { primitive : primitive; nullable : bool; shape : shape }
type operator = Add | Mul | Sub | Div | Greater
type reducer = Mean | Sd | Sum

type node =
  | Constant of [ `Int of int64 | `Float of float | `Bool of bool ]
  | Coalesce of expr * expr
  | Abs of expr
  | Input of int
  | Broadcast of int * expr
  | Binary of operator * expr * expr
  | Reduce of reducer * expr
and expr = { node : node; typ : typ }

exception Error of string

let input id typ = { node = Input id; typ }

let align left right =
  match left, right with
  | Scalar, shape | shape, Scalar -> shape
  | Column a, Column b when a = b -> left
  | _ -> raise (Error "Columns belong to different row contexts")

let broadcast shape expr =
  match shape, expr.typ.shape with
  | Column id, Scalar ->
      { node = Broadcast (id, expr); typ = { expr.typ with shape } }
  | _ -> expr

let binary op left right =
  let a = left.typ and b = right.typ in
  if a.primitive <> b.primitive || (a.primitive = Bool || a.primitive = String) then
    raise (Error "Expected matching numeric types");
  if op = Div && a.primitive = Int64 then
    raise (Error "Integer division requires a checked conversion");
  let shape = align a.shape b.shape in
  let primitive = if op = Greater then Bool else a.primitive in
  { node = Binary (op, broadcast shape left, broadcast shape right);
    typ = { primitive; shape; nullable = a.nullable || b.nullable } }

let reduce reducer expr =
  match expr.typ.shape, expr.typ.primitive with
  | Column _, Float64 ->
      { node = Reduce (reducer, expr);
        typ = { primitive = Float64; nullable = true; shape = Scalar } }
  | _ -> raise (Error "Expected a Float64 column")

let filter context predicate =
  if predicate.typ <> { primitive = Bool; nullable = false; shape = Column context } then
    raise (Error "Expected a nonnullable Boolean column from this table")

let constant value =
  let primitive = match value with `Int _ -> Int64 | `Float _ -> Float64 | `Bool _ -> Bool in
  { node = Constant value; typ = { primitive; nullable = false; shape = Scalar } }

let coalesce x fallback =
  if x.typ.primitive <> fallback.typ.primitive then raise (Error "coalesce requires matching types");
  let shape = align x.typ.shape fallback.typ.shape in
  { node = Coalesce (broadcast shape x, broadcast shape fallback);
    typ = { x.typ with shape; nullable = x.typ.nullable && fallback.typ.nullable } }

let abs x =
  if x.typ.primitive = Bool || x.typ.primitive = String then raise (Error "abs requires a number");
  { x with node = Abs x }

