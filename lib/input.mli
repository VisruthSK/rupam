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

type snapshot =
  { digest : string
  ; columns : column list
  }

val format : string -> format
val format_name : format -> string
val default_missing : string list
