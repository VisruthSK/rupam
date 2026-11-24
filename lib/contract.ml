module Json = Yojson.Basic
module J = Json.Util

type column = {
  name : string;
  logical_type : string;
  required : bool;
  restricted : bool;
  metadata : Json.t;
}

type table = {
  name : string;
  parquet : string option;
  columns : column list;
  metadata : Json.t;
}

type t = { path : string; tables : table list; metadata : Json.t }

let string key json = J.member key json |> J.to_string
let optional key json = J.member key json |> J.to_string_option
let list key json = match J.member key json with `Null -> [] | value -> J.to_list value

let decode path metadata =
  if string "$version" metadata <> "0.1.0" then failwith "Unsupported data-dict export version";
  let column metadata =
    { name = string "name" metadata;
      logical_type = string "type" metadata;
      required = List.mem (`String "required") (list "constraints" metadata);
      restricted = optional "display" metadata = Some "restricted";
      metadata } in
  let table metadata =
    let parquet = match J.member "source" metadata with
      | `Null -> None
      | source -> Option.map (Filename.concat (Filename.dirname path)) (optional "parquet" source) in
    { name = string "name" metadata; parquet;
      columns = List.map column (J.member "columns" metadata |> J.to_list); metadata } in
  { path; tables = List.map table (J.member "tables" metadata |> J.to_list); metadata }

let import path =
  let command = "data-dict" in
  let channel = Unix.open_process_args_in command [| command; "export-spec"; path |] in
  let result = try Ok (In_channel.input_all channel) with exn -> Error exn in
  match Unix.close_process_in channel, result with
  | Unix.WEXITED 0, Ok text -> decode path (Json.from_string text)
  | _, Error exn -> raise exn
  | _ -> failwith "data-dict export-spec failed"

let describe t =
  List.iter (fun (table : table) ->
    Printf.printf "%s (values pending)\n" table.name;
    List.iter (fun (column : column) ->
      Printf.printf "  %s : %s%s%s\n" column.name column.logical_type
        (if column.required then "" else "?")
        (if column.restricted then " [restricted]" else "")) table.columns) t.tables
