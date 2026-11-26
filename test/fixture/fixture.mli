type status =
  | Active
  | Pending
  | Dialect of string list

type outcome =
  { typ : string option
  ; value : string option
  ; diagnostic : string option
  ; message : string option
  }

type t =
  { id : string
  ; status : status
  ; source : string option
  ; expect : outcome
  }

type result =
  | Passed
  | Failed of string list
  | Unspecified

val all : string -> t list
val evaluate : string -> outcome
val on_session : Rupam_native.Session.t -> string -> outcome
val targets : string list
val on_target : string -> t -> result
val check : ?session:Rupam_native.Session.t -> t -> result
val cell : Rupam_native.Arrow_result.value -> string
val rows : Rupam_native.Session.t -> string -> string list * string list list
val capture : directory:string -> id:string -> string -> outcome
