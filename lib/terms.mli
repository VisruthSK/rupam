val expect : Diagnostic.span -> Types.t -> Types.t -> unit

val make
  :  Syntax.span
  -> TransformIR.shape
  -> Types.t
  -> TransformIR.node
  -> TransformIR.expr

val align : 'a -> TransformIR.shape -> TransformIR.shape -> TransformIR.shape
val shape_of : 'a -> TransformIR.expr list -> TransformIR.shape
val closest : string -> string list -> string option
val did_you_mean : string option -> string
val column : int -> Diagnostic.span -> Types.row -> string -> Types.t
val exact_float : Diagnostic.span -> string -> float
val constant : Diagnostic.span -> Syntax.literal -> TransformIR.constant
val to_float : TransformIR.expr -> TransformIR.expr

val common
  :  Diagnostic.span
  -> TransformIR.expr
  -> TransformIR.expr
  -> TransformIR.expr * TransformIR.expr * Types.t

val lift
  :  Scope.env
  -> Syntax.span
  -> TransformIR.shape
  -> TransformIR.expr list
  -> (TransformIR.expr list -> TransformIR.expr)
  -> TransformIR.expr

val row_signature : int -> Prelude.row -> Types.t list * Types.t
val builtin_type : int -> Prelude.builtin -> Types.t

val annotation
  :  Scope.env
  -> (string * Types.t) list
  -> int
  -> Syntax.annotation
  -> Types.t
