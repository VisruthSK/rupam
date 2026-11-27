type move =
  | Rows of int
  | Columns of int
  | Pan_rows of int
  | Pan_columns of int
  | Cell of int * int
  | Top
  | Bottom

type action =
  | Sort of Rupam.TransformIR.direction
  | Keep_value
  | Undo
  | Distribution
  | Missing_policy of Rupam.TransformIR.policy

type t =
  { text : string
  ; generation : int
  ; displayed : int
  ; settled : int
  ; quiet_ticks : int
  ; base : Rupam_native.Session.result
  ; context : Rupam_native.Session.draft
  ; result : Rupam_native.Session.result
  ; names : string array
  ; pinned : string list
  ; row : int
  ; column : int
  ; top : int
  ; left : int
  ; width : int
  ; height : int
  ; window : Rupam_native.Page.t
  ; history : string list
  ; summarizing : bool
  ; profiles : (string * Rupam_native.Arrow_result.profile) list
  ; policy : Rupam.Syntax.expr option
  ; diagnostic : Rupam.Diagnostic.t option
  ; binding : string
  }

val source : t -> string
val panel_height : int
val rows : t -> int
val index_width : t -> int
val cell_width : t -> int
val current : t -> string option
val take : int -> 'a list -> 'a list
val frozen : t -> int option

val open_
  :  Rupam_native.Session.t
  -> Rupam.Syntax.expr
  -> Rupam_native.Session.result
  -> width:int
  -> height:int
  -> t

val resize : t -> width:int -> height:int -> t
val move : t -> move -> t
val close : t -> unit
val publish : t -> int -> (Rupam_native.Session.result, Rupam.Diagnostic.t) result -> t
val edit : t -> string -> t
val named : t -> bool
val submission : t -> string -> string
val act : t -> action -> t
val visible : t -> Rupam_native.Page.t
val bars : digits:int -> t -> string list
val remember_profile : t -> string -> Rupam_native.Arrow_result.profile -> t
val needs_summary : t -> string option
