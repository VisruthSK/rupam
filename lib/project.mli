type dialect =
  | R_dplyr
  | Python_polars

val dialect_name : dialect -> string
val dialects : (string * dialect) list
val yojson_of_dialect : dialect -> [> `String of string ]
val dialect_of_yojson : Yojson.Safe.t -> dialect

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
  ; dialect : dialect option setting
  ; preview_digits : int setting
  ; strict : bool setting
  }

val standalone : t
val options : t -> (string * string * string) list
val load : string -> t
val require_dialect : t -> dialect
