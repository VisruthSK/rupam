external query : string -> string array * string option array array = "rupam_duckdb_query"

type format =
  | Parquet
  | Csv
  | Tsv
[@@deriving sexp_of]

type physical =
  | Supported of Types.t
  | Unsupported of string

type column =
  { name : string
  ; typ : physical
  ; nullable : bool
  }

val format : string -> format
val format_name : format -> string
val literal : string -> string
val digest : string -> string
val columns : string -> column list
