type t
type result
type draft

type event =
  | Bound of string * string
  | Output of string
  | Value of string * Types.t
  | Table of Page.t
  | Viewed of Syntax.expr * result
  | Schema of int * (string * Types.t) list
  | Typed of Types.t
  | Histogram of string * Arrow_result.summary
  | Bars of string * Arrow_result.frequencies
  | Inspection_failed of Diagnostic.t

val create : project:Project.t -> directory:string -> t
val load : t -> string -> unit
val close : t -> unit
val env : t -> Scope.env
val directory : t -> string
val project : t -> Project.t
val target : t -> Target.t option
val warm : t -> unit
val set_target : t -> Target.t -> origin:string -> unit
val path : t -> string option
val revision : t -> int
val items : t -> TransformIR.item list
val submit : t -> Syntax.item list -> event list
val verify : t -> TransformIR.item list -> unit
val check : t -> Syntax.item -> TransformIR.item list
val table : t -> string -> TransformIR.table
val table_of : t -> Syntax.expr -> TransformIR.table
val open_draft : t -> draft
val preview_draft : draft -> string -> result
val cancel_draft : draft -> unit
val reset_cancelled_draft : draft -> unit
val close_draft : draft -> unit
val release_draft : result -> unit
val result : t -> TransformIR.table -> result
val ordering_of : result -> string
val page : ?columns:string list -> result -> offset:int -> limit:int -> Page.t
val histogram : result -> string -> Arrow_result.summary
val column_summary : result -> string -> Arrow_result.profile
val cell : result -> row:int -> string -> Types.t * Arrow_result.value
val total : result -> int
val table_of_result : result -> TransformIR.table
val preview : t -> TransformIR.table -> offset:int -> limit:int -> Page.t
type code =
  | Definition
  | Full
  | Expression

val code : t -> ?mode:code -> Target.t option -> string -> string
val undo : t -> bool
val redo : t -> bool
val source : t -> string
val save : t -> string -> unit
val unsaved : t -> bool
val lock : t -> Lock.entry
