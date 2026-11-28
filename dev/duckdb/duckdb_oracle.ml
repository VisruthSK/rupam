external query : string -> string array * string option array array = "rupam_duckdb_query"

let literal text = "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"
let parquet path = "read_parquet(" ^ literal path ^ ")"
let quote name = "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

let parquet_columns path : Input.column list =
  let names, rows = query ("DESCRIBE SELECT * FROM " ^ parquet path) in
  let field name row =
    match Array.find_index (String.equal name) names with
    | Some i -> Option.value row.(i) ~default:""
    | None -> invalid_arg ("DuckDB DESCRIBE has no " ^ name ^ " column")
  in
  let described =
    Array.to_list rows
    |> List.map (fun row -> field "column_name" row, field "column_type" row)
  in
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
       ^ parquet path)
  in
  let typ = function
    | "BIGINT" | "INTEGER" -> Input.Supported Types.Int
    | "DOUBLE" -> Supported Float
    | "BOOLEAN" -> Supported Bool
    | "VARCHAR" -> Supported String
    | "DATE" -> Supported Date
    | other -> Unsupported other
  in
  List.mapi
    (fun i (name, physical) ->
       ({ name
        ; typ = typ physical
        ; nullable = Option.value counts.(0).(i) ~default:"0" <> "0"
        }
        : Input.column))
    described
;;

let rows ?(keys = []) path =
  let order =
    match keys with
    | [] -> ""
    | keys ->
      " ORDER BY " ^ String.concat ", " (List.map (fun k -> quote k ^ " NULLS LAST") keys)
  in
  let names, _ = query ("SELECT * FROM " ^ parquet path ^ " LIMIT 0") in
  let text name = "CAST(" ^ quote name ^ " AS VARCHAR) AS " ^ quote name in
  query
    ("SELECT "
     ^ String.concat ", " (List.map text (Array.to_list names))
     ^ " FROM "
     ^ parquet path
     ^ order)
;;
