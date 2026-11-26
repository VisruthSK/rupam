module Json = Yojson.Basic
module J = Json.Util

type column =
  { name : string
  ; logical_type : string
  ; required : bool
  ; primary : bool
  ; unique : bool
  ; restricted : bool
  ; metadata : Json.t
  }

type table =
  { name : string
  ; parquet : string option
  ; columns : column list
  ; metadata : Json.t
  }

type t =
  { path : string
  ; tables : table list
  ; metadata : Json.t
  }

let string key json = J.member key json |> J.to_string
let optional key json = J.member key json |> J.to_string_option

let list key json =
  match J.member key json with
  | `Null -> []
  | value -> J.to_list value
;;

let decode path metadata =
  if string "$version" metadata <> "0.1.0"
  then Diagnostic.fail Contract_failure "Unsupported data-dict export version";
  let column metadata =
    { name = string "name" metadata
    ; logical_type = string "type" metadata
    ; required =
        List.exists
          (fun c -> List.mem (`String c) (list "constraints" metadata))
          [ "required"; "primary_key" ]
    ; primary = List.mem (`String "primary_key") (list "constraints" metadata)
    ; unique = List.mem (`String "unique") (list "constraints" metadata)
    ; restricted = optional "display" metadata = Some "restricted"
    ; metadata
    }
  in
  let table metadata =
    let parquet =
      match J.member "source" metadata with
      | `Null -> None
      | source ->
        Option.map (Filename.concat (Filename.dirname path)) (optional "parquet" source)
    in
    { name = string "name" metadata
    ; parquet
    ; columns = List.map column (J.member "columns" metadata |> J.to_list)
    ; metadata
    }
  in
  { path; tables = List.map table (J.member "tables" metadata |> J.to_list); metadata }
;;

let describe_table (table : table) =
  print_endline table.name;
  let width =
    List.fold_left (fun w (c : column) -> max w (String.length c.name)) 0 table.columns
  in
  List.iter
    (fun (column : column) ->
       Printf.printf
         "  %-*s  %s%s%s\n"
         width
         column.name
         column.logical_type
         (if column.required then "" else "?")
         (if column.restricted then "  restricted" else ""))
    table.columns
;;

let describe t =
  Printf.printf "contract %s\n" (Filename.basename t.path);
  List.iter
    (fun table ->
       print_newline ();
       describe_table table)
    t.tables
;;
