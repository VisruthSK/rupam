type context =
  { expression : ?missing:Core.policy -> Types.row -> Syntax.expr -> Core.expr
  ; table : Syntax.expr -> Core.table
  ; missing : Core.policy option
  ; group_missing : Core.policy option
  }

type arguments = (string option * Syntax.expr) list

val mem : string -> bool
val apply : context -> Diagnostic.span -> string -> Core.table -> arguments -> Core.table
