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
val check : ?session:Rupam.Session.t -> t -> result
val cell : Rupam.Arrow_result.value -> string
val rows : Rupam.Session.t -> string -> string list * string list list
val capture : directory:string -> id:string -> string -> outcome
