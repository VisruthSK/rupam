type status =
  | Active
  | Pending

type outcome =
  { typ : string option
  ; value : string option
  ; diagnostic : string option
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
val check : t -> result
val capture : directory:string -> id:string -> string -> outcome
