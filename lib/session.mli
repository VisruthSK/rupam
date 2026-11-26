type t

type event =
  | Bound of string * string
  | Output of string
  | Value of string * Types.t
  | Table of Page.t

val create : project:Project.t -> directory:string -> t
val load : t -> string -> unit
val close : t -> unit
val env : t -> Scope.env
val project : t -> Project.t
val target : t -> Target.t option
val set_target : t -> Target.t -> origin:string -> unit
val path : t -> string option
val revision : t -> int
val items : t -> Core.item list
val submit : t -> Syntax.declaration list -> event list
val check : t -> Syntax.declaration -> Core.item list
val table : t -> string -> Core.table
val preview : t -> Core.table -> offset:int -> limit:int -> Page.t
val code : t -> Target.t option -> Core.table -> string
val undo : t -> bool
val redo : t -> bool
val source : t -> string
val save : t -> string -> unit
val unsaved : t -> bool
val lock : t -> Lock.entry
