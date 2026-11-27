type t =
  { kind : Syntax.tap
  ; subject : Syntax.expr
  ; arguments : (Syntax.label option * Syntax.expr) list
  }

val split : Syntax.item -> Syntax.item option * t list
