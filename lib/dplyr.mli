val dates : Prelude.row -> R_syntax.expr list -> R_syntax.expr option

val date_arithmetic
  :  Syntax.binary
  -> Types.t
  -> Types.t
  -> R_syntax.expr
  -> R_syntax.expr
  -> R_syntax.expr option

val table : Host.names -> TransformIR.table -> R_syntax.expr
