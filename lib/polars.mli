module P = Python_syntax

val preview
  :  runtime:Runtime.t
  -> Analysis_program.dataset
  -> offset:int
  -> limit:int
  -> Page.t

module Values : sig
  val math : string -> P.expr -> P.expr
  val as_float : P.expr -> P.expr
  val missing : P.expr -> P.expr
  val native : (unit -> string) -> Prelude.builtin -> P.expr list -> P.expr
  val arity : Prelude.builtin -> int
  val operator : Syntax.binary -> P.operator
  val literal : Syntax.literal -> P.expr
  val values : Syntax.declaration list -> P.statement list
end

val program
  :  strict:bool
  -> directory:string
  -> Syntax.declaration list
  -> string * Document.location list

val value : Syntax.declaration list -> string
val table : Analysis_program.dataset -> string
