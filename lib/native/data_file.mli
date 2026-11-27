external query : string -> string array * string option array array = "rupam_duckdb_query"
val literal : string -> string
val digest : string -> string
val columns : missing:string list -> string -> Input.column list
