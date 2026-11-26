type origin =
  | Default
  | Global of string
  | Project_file of string
  | Session of string

type 'a setting =
  { value : 'a
  ; origin : origin
  }

type t =
  { root : string option
  ; target : Target.t option setting
  ; preview_digits : int setting
  ; strict : bool setting
  }

val standalone : t
val options : t -> (string * string * string) list
val load : string -> t
val require_target : t -> Target.t
