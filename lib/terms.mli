val expect : Diagnostic.span -> Types.t -> Types.t -> unit
val make : Syntax.span -> Core.shape -> Types.t -> Core.node -> Core.expr
val align : 'a -> Core.shape -> Core.shape -> Core.shape
val shape_of : 'a -> Core.expr list -> Core.shape
val closest : string -> string list -> string option
val did_you_mean : string option -> string
val column : int -> Diagnostic.span -> Types.row -> string -> Types.t
val exact_float : Diagnostic.span -> string -> float
val constant : Diagnostic.span -> Syntax.literal -> Core.constant
val to_float : Core.expr -> Core.expr
val common : Diagnostic.span -> Core.expr -> Core.expr -> Core.expr * Core.expr * Types.t

val lift
  :  Scope.env
  -> Syntax.span
  -> Core.shape
  -> Core.expr list
  -> (Core.expr list -> Core.expr)
  -> Core.expr

val row_signature : int -> Prelude.row -> Types.t list * Types.t
val builtin_type : int -> Prelude.builtin -> Types.t

val annotation
  :  Scope.env
  -> (string * Types.t) list
  -> int
  -> Syntax.annotation
  -> Types.t
