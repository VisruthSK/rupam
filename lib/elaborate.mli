val item : Scope.env -> Syntax.declaration -> Scope.env * Core.item list
val program : Scope.env -> Syntax.declaration list -> Scope.env * Core.item list
val builtins : unit -> (string * string) list
