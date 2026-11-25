type step =
  | Arrange of (Shape.expr * bool) list
  | Summarize of (string * Shape.expr) list * (string * Shape.expr option) list
  | Take of int
  | Distinct of Shape.expr list
  | Filter of Shape.expr
  | Window of string * Shape.expr * (string * Shape.expr) list
  | Pivot of Pivot.t
  | Rename of (string * string * int) list
  | Bind of string * Shape.expr
  | Select of string list
  | Assert_key of string list

type key =
  { columns : string list
  ; present : bool
  }

type t =
  { locations : (step * Syntax.span) list
  ; context : int
  ; schema : (string * Shape.expr) list
  ; steps : step list
  ; next : int
  ; keys : key list
  ; order : string list option
  }

val source : ?keys:key list -> int -> (string * Shape.primitive * bool) list -> t
val names : t -> string list
val key : t -> string list -> key
val unique : t -> string list -> bool

type options =
  { missing : Shape.policy option
  ; group_missing : Shape.policy option
  ; rest : (string option * Syntax.expr) list
  }

val stage
  :  ?span:Syntax.span
  -> Check.env
  -> t
  -> string
  -> (string option * Syntax.expr) list
  -> t

val execute
  :  t
  -> rows:int
  -> Column_eval.value array list
  -> int * (string * Column_eval.value array) list

val pipeline : Check.env -> (string * t) list -> Syntax.expr -> t
