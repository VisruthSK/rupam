type column =
  { name : string
  ; typ : Types.t
  ; cells : string option array
  }

type t =
  { columns : column list
  ; offset : int
  ; total : int
  ; ordering : string
  ; width : int
  }

type part =
  | Plain
  | Heading
  | Types
  | Missing
  | Index
  | Note

val of_arrow
  :  ?columns:string list
  -> Arrow_result.t
  -> ordering:string
  -> offset:int
  -> limit:int
  -> (string * Types.t) list
  -> t

val lines : ?limit:int -> digits:int -> t -> (part * string) list list
val plain : digits:int -> t -> string list
val format_cell : digits:int -> Types.t -> string option -> string option
val summary : t -> string
