module R = R_syntax

val statements : Syntax.declaration list -> R.statement list
val render : Syntax.declaration list -> string
