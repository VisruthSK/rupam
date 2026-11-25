open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type point =
  { line : int
  ; column : int
  }
[@@deriving yojson_of]

type mapping =
  { generated_line : int
  ; generated_column : int
  ; source : string
  ; start : point
  ; end_ : point [@key "end"]
  }
[@@deriving yojson_of]

type source_map = { mappings : mapping list } [@@deriving yojson_of]

type manifest =
  { compiler : string
  ; dialect : Project.dialect
  ; source : string
  ; program : string
  ; inputs : Lock.input list
  ; dependencies : string list
  ; nonportable : string list
  }
[@@deriving yojson_of]

let point (p : Lexing.position) =
  { line = p.pos_lnum; column = 1 + p.pos_cnum - p.pos_bol }
;;

let dependencies (dialect : Project.dialect) ~tabular =
  match dialect, tabular with
  | R_dplyr, true -> [ "dplyr"; "tidyr"; "bit64"; "jsonlite"; "arrow" ]
  | R_dplyr, false -> [ "bit64"; "jsonlite" ]
  | Python_polars, true -> [ "polars"; "pyarrow" ]
  | Python_polars, false -> []
;;

let write_json path json =
  Out_channel.with_open_text path (fun channel ->
    output_string channel (Yojson.Safe.pretty_to_string json ^ "\n"))
;;

let write
      ~(entry : Lock.entry)
      ~program
      ~locations
      ~(dialect : Project.dialect)
      ~output
      ~code
      declarations
  =
  let extension = Runtime.suffix (Runtime.of_dialect dialect) in
  if not (Filename.check_suffix output extension)
  then Diagnostic.fail Unreadable_input ("Expected a " ^ extension ^ " output file");
  let mappings =
    List.map
      (fun (location : Document.location) ->
         let start, finish = location.source in
         { generated_line = location.line
         ; generated_column = location.column
         ; source = entry.source
         ; start = point start
         ; end_ = point finish
         })
      locations
  in
  let manifest =
    { compiler = entry.compiler
    ; dialect = entry.dialect
    ; source = entry.source
    ; program
    ; inputs = entry.inputs
    ; dependencies = dependencies dialect ~tabular:(Analysis_program.tabular declarations)
    ; nonportable =
        List.filter_map
          (function
            | Syntax.Import ("polars", _) -> Some "polars.sqrt"
            | _ -> None)
          declarations
    }
  in
  Out_channel.with_open_text output (fun channel -> output_string channel code);
  write_json (output ^ ".map.json") (yojson_of_source_map { mappings });
  write_json (output ^ ".manifest.json") (yojson_of_manifest manifest)
;;
