external query : string -> string array * string option array array = "rupam_duckdb_query"

type format =
  | Parquet
  | Csv
  | Tsv

type column =
  { name : string
  ; physical : string
  ; nullable : bool
  }

val format : string -> format
val format_name : format -> string
val literal : string -> string
val digest : string -> string
val columns : string -> column list
