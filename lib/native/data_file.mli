external query : string -> string array * string option array array = "rupam_duckdb_query"

val literal : string -> string
val digest : string -> string
val columns : string -> Input.column list
