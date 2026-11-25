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
  let dependencies =
    match dialect, Analysis_program.tabular declarations with
    | R_dplyr, true -> [ "dplyr"; "tidyr"; "bit64"; "jsonlite"; "arrow" ]
    | R_dplyr, false -> [ "bit64"; "jsonlite" ]
    | Python_polars, true -> [ "polars"; "pyarrow" ]
    | Python_polars, false -> []
  in
  if not (Filename.check_suffix output extension)
  then Diagnostic.fail Unreadable_input ("Expected a " ^ extension ^ " output file");
  let strings xs = `List (List.map (fun s -> `String s) xs) in
  let point (p : Lexing.position) =
    `Assoc [ "line", `Int p.pos_lnum; "column", `Int (1 + p.pos_cnum - p.pos_bol) ]
  in
  let mappings =
    List.map
      (fun (location : Document.location) ->
         let start, finish = location.source in
         `Assoc
           [ "generated_line", `Int location.line
           ; "generated_column", `Int location.column
           ; "source", `String entry.source
           ; "start", point start
           ; "end", point finish
           ])
      locations
  in
  let nonportable =
    List.filter_map
      (function
        | Syntax.Import ("polars", _) -> Some "polars.sqrt"
        | _ -> None)
      declarations
  in
  let manifest =
    `Assoc
      [ "compiler", `String entry.compiler
      ; "dialect", `String entry.dialect
      ; "source", `String entry.source
      ; "program", `String program
      ; ( "inputs"
        , `List
            (List.map
               (fun (i : Lock.input) ->
                  `Assoc [ "path", `String i.path; "sha256", `String i.sha256 ])
               entry.inputs) )
      ; "dependencies", strings dependencies
      ; "nonportable", strings nonportable
      ]
  in
  let json path value =
    Out_channel.with_open_text path (fun channel ->
      output_string channel (Yojson.Basic.pretty_to_string value ^ "\n"))
  in
  Out_channel.with_open_text output (fun channel -> output_string channel code);
  json (output ^ ".map.json") (`Assoc [ "mappings", `List mappings ]);
  json (output ^ ".manifest.json") manifest
;;
