val item : Scope.env -> Syntax.item -> Scope.env * TransformIR.item list
val program : Scope.env -> Syntax.item list -> Scope.env * TransformIR.item list
val builtins : unit -> (string * string) list
