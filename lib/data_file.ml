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

let digests : (string * float * int, string) Hashtbl.t = Hashtbl.create 16

let digest path =
  let stats = Unix.stat path in
  let identity = path, stats.st_mtime, stats.st_size in
  match Hashtbl.find_opt digests identity with
  | Some digest -> digest
  | None ->
    let _, rows = query ("SELECT sha256(content) FROM read_blob(" ^ literal path ^ ")") in
    let digest = Option.get rows.(0).(0) in
    Hashtbl.replace digests identity digest;
    digest
;;

let columns path =
  if not (Sys.file_exists path) then failwith ("No such data file " ^ path);
  let names, rows = query ("DESCRIBE SELECT * FROM " ^ reader path) in
  let field name row =
    match Array.find_index (String.equal name) names with
    | Some i -> Option.value row.(i) ~default:""
    | None -> failwith ("DuckDB DESCRIBE has no " ^ name ^ " column")
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
  List.mapi
    (fun i (name, physical) ->
       { name; physical; nullable = Option.value counts.(0).(i) ~default:"0" <> "0" })
    described
;;
