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

let literal text = "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"

let reader path =
  match format path with
  | Parquet -> "read_parquet(" ^ literal path ^ ")"
  | Csv -> "read_csv(" ^ literal path ^ ")"
  | Tsv -> "read_csv(" ^ literal path ^ ", delim = '\\t')"
;;

let digest path =
  In_channel.with_open_bin path (fun channel ->
    let buffer = Bytes.create 65536 in
    let rec feed context =
      match In_channel.input channel buffer 0 (Bytes.length buffer) with
      | 0 -> Digestif.SHA256.(to_hex (get context))
      | n -> feed (Digestif.SHA256.feed_bytes context buffer ~off:0 ~len:n)
    in
    feed Digestif.SHA256.empty)
;;

let columns path =
  if not (Sys.file_exists path)
  then Diagnostic.fail Unreadable_input ("No such data file " ^ path);
  let names, rows = query ("DESCRIBE SELECT * FROM " ^ reader path) in
  let field name row =
    match Array.find_index (String.equal name) names with
    | Some i -> Option.value row.(i) ~default:""
    | None -> invalid_arg ("DuckDB DESCRIBE has no " ^ name ^ " column")
  in
  let described =
    Array.to_list rows
    |> List.map (fun row -> field "column_name" row, field "column_type" row)
  in
  let quote name = "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\"" in
  let missing (name, physical) =
    let column = quote name in
    if physical = "DOUBLE"
    then "count(*) FILTER (WHERE " ^ column ^ " IS NULL OR isnan(" ^ column ^ "))"
    else "count(*) - count(" ^ column ^ ")"
  in
  let _, counts =
    query
      ("SELECT "
       ^ String.concat ", " (List.map missing described)
       ^ " FROM "
       ^ reader path)
  in
  let typ = function
    | "BIGINT" | "INTEGER" -> Supported Types.Int64
    | "DOUBLE" -> Supported Types.Float64
    | "BOOLEAN" -> Supported Types.Bool
    | "VARCHAR" -> Supported Types.String
    | other -> Unsupported other
  in
  List.mapi
    (fun i (name, physical) ->
       { name
       ; typ = typ physical
       ; nullable = Option.value counts.(0).(i) ~default:"0" <> "0"
       })
    described
;;
