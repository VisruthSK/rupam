type column =
  { name : string
  ; typ : Types.t
  ; cells : string option array
  }

type t =
  { columns : column list
  ; offset : int
  ; total : int
  ; ordered : bool
  }

type part =
  | Plain
  | Heading
  | Types
  | Missing
  | Index
  | Note

val read
  :  path:string
  -> total:int
  -> ordered:bool
  -> offset:int
  -> (string * Types.t) list
  -> t

val lines : ?limit:int -> digits:int -> t -> (part * string) list list
val plain : digits:int -> t -> string list
