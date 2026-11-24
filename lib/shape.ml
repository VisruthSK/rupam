type primitive = Int64 | Float64 | Bool
type shape = Scalar | Column of int
type typ = { primitive : primitive; nullable : bool; shape : shape }
type operator = Mul | Sub | Div | Greater
type reducer = Mean | Sd

type node =
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
  if a.primitive <> b.primitive || a.primitive = Bool then
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
