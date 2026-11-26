type language =
  | R
  | Python

type dialect =
  | Dplyr
  | Polars

type t = private
  { language : language
  ; dialect : dialect
  }

val all : t list
val r_dplyr : t
val python_polars : t
val make : language -> dialect -> t option
val language_name : language -> string
val dialect_name : dialect -> string
val name : t -> string
val languages : (string * language) list
val dialects : (string * dialect) list
val targets : (string * t) list
val dialects_of : language -> dialect list
val language_of : dialect -> language
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
