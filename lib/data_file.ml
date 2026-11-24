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

let format path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".parquet" -> Parquet
  | ".csv" -> Csv
  | ".tsv" | ".tab" -> Tsv
  | extension -> failwith ("Unsupported data file extension " ^ extension)
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

let columns path =
  if not (Sys.file_exists path) then failwith ("No such data file " ^ path);
  let names, rows = query ("DESCRIBE SELECT * FROM " ^ reader path) in
  let field name row =
    match Array.find_index (String.equal name) names with
    | Some i -> Option.value row.(i) ~default:""
    | None -> failwith ("DuckDB DESCRIBE has no " ^ name ^ " column")
  in
  Array.to_list rows
  |> List.map (fun row ->
    { name = field "column_name" row
    ; physical = field "column_type" row
    ; nullable = field "null" row <> "NO"
    })
;;
