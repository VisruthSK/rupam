val query : string -> string array * string option array array
val parquet_columns : string -> Input.column list
val rows : ?keys:string list -> string -> string array * string option array array
