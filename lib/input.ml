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

let format path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".parquet" -> Parquet
  | ".csv" -> Csv
  | ".tsv" | ".tab" -> Tsv
  | extension ->
    Diagnostic.fail Unreadable_input ("Unsupported data file extension " ^ extension)
;;

let format_name = function
  | Parquet -> "parquet"
  | Csv -> "csv"
  | Tsv -> "tsv"
;;
