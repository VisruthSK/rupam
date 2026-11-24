let write ~locations ~target ~source ~output ~code declarations =
  let extension =
    match target with
    | "r" -> "R"
    | "python" -> "py"
    | _ -> invalid_arg "target"
  in
  if not (Filename.check_suffix output ("." ^ extension))
  then failwith ("Expected ." ^ extension ^ " output file");
  let print path body =
    Out_channel.with_open_text path (fun ch -> output_string ch body)
  in
  let json path value = print path (Yojson.Basic.pretty_to_string value ^ "\n") in
  let lines = String.split_on_char '\n' code in
  let indexed = List.mapi (fun i s -> i + 1, s) lines in
  let contains s needle =
    let rec loop i =
      i + String.length needle <= String.length s
      && (String.sub s i (String.length needle) = needle || loop (i + 1))
    in
    loop 0
  in
  let contracted =
    List.exists
      (function
        | Syntax.Contract _ -> true
        | _ -> false)
      declarations
  in
  let strings xs = `List (List.map (fun s -> `String s) xs) in
  let outputs =
    List.filter_map
      (function
        | Syntax.Evaluate
            { form = Call ({ form = Name (Bare, "write_parquet"); _ }, _); span; _ } ->
          Some span
        | _ -> None)
      declarations
  in
  let emitted =
    List.filter_map
      (fun (i, line) ->
         if contains line (if target = "r" then "rp_write(" else ".write_parquet(")
         then Some i
         else None)
      indexed
  in
  let point (p : Lexing.position) =
    `Assoc [ "line", `Int p.pos_lnum; "column", `Int (1 + p.pos_cnum - p.pos_bol) ]
  in
  let maps =
    if not contracted
    then []
    else
      List.map2
        (fun line (start, finish) ->
           `Assoc
             [ "generated_line", `Int line
             ; "source", `String source
             ; "start", point start
             ; "end", point finish
             ])
        emitted
        outputs
  in
  let maps =
    maps
    @ List.map
        (fun (entry : Document.location) ->
           let start, finish = entry.source in
           `Assoc
             [ "generated_line", `Int entry.line
             ; "generated_column", `Int entry.column
             ; "source", `String source
             ; "start", point start
             ; "end", point finish
             ])
        locations
  in
  let callback = List.exists (fun (_, s) -> contains s ").map_elements(") indexed in
  let nonportable =
    if List.exists (fun (_, s) -> contains s ".sqrt()") indexed
    then [ "polars.sqrt" ]
    else []
  in
  let dependencies =
    match target, contracted with
    | "python", true -> [ "polars"; "data-dict-yaml==0.0.3" ]
    | "r", true ->
      [ "dplyr"; "tidyr"; "bit64"; "jsonlite"; "arrow"; "data-dict-yaml==0.0.3" ]
    | "r", false -> [ "gmp" ]
    | _ -> []
  in
  let warnings =
    (if contracted then [ "Contract validation scans all source rows" ] else [])
    @
    if callback
    then [ "Checked scalar fallback executes a Python callback per row" ]
    else []
  in
  let manifest =
    `Assoc
      [ "language", `String "rupam"
      ; "language_version", `String "0.1"
      ; "compiler_version", `String "0.1"
      ; "target", `String target
      ; "dependencies", strings dependencies
      ; ( "validation"
        , strings (if contracted then [ "validate-meta"; "validate-data" ] else []) )
      ; ( "fidelity"
        , strings
            ((if contracted then [ "guarded" ] else [ "exact" ])
             @ if callback then [ "fallback" ] else []) )
      ; "warnings", strings warnings
      ; "nonportable", strings nonportable
      ]
  in
  print output code;
  json (output ^ ".map.json") (`Assoc [ "mappings", `List maps ]);
  json (output ^ ".manifest.json") manifest
;;
