type t
type result

type event =
  | Bound of string * string
  | Output of string
  | Value of string * Types.t
  | Table of Page.t
  | Viewed of string * result
  | Schema of (string * Types.t) list
  | Typed of Types.t
  | Histogram of string * Arrow_result.histogram

val create : project:Project.t -> directory:string -> t
val load : t -> string -> unit
val close : t -> unit
val env : t -> Scope.env
val directory : t -> string
val project : t -> Project.t
val target : t -> Target.t option
val set_target : t -> Target.t -> origin:string -> unit
val path : t -> string option
val revision : t -> int
val items : t -> TransformIR.item list
val submit : t -> Syntax.item list -> event list
val check : t -> Syntax.item -> TransformIR.item list
val table : t -> string -> TransformIR.table
val result : t -> TransformIR.table -> result
val page : result -> offset:int -> limit:int -> Page.t
val total : result -> int
val preview : t -> TransformIR.table -> offset:int -> limit:int -> Page.t
val code : t -> Target.t option -> TransformIR.table -> string
val undo : t -> bool
val redo : t -> bool
val source : t -> string
val save : t -> string -> unit
val unsaved : t -> bool
val lock : t -> Lock.entry
