val item : Scope.env -> Syntax.declaration -> Scope.env * TransformIR.item list
val program : Scope.env -> Syntax.declaration list -> Scope.env * TransformIR.item list
val builtins : unit -> (string * string) list
