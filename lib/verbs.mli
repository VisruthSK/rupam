type context =
  { expression :
      ?missing:TransformIR.policy -> Types.row -> Syntax.expr -> TransformIR.expr
  ; table : Syntax.expr -> TransformIR.table
  ; missing : TransformIR.policy option
  ; group_missing : TransformIR.policy option
  }

type arguments = (string option * Syntax.expr) list

val mem : string -> bool

val apply
  :  context
  -> Diagnostic.span
  -> string
  -> TransformIR.table
  -> arguments
  -> TransformIR.table
