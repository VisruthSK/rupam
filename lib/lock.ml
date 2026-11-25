module Json = Yojson.Basic
module J = Json.Util

let compiler = "rupam 0.1.0"
let file_name = "rupam.lock"

type input =
  { path : string
  ; sha256 : string
  }

type entry =
  { source : string
  ; dialect : string
  ; compiler : string
  ; strict : bool
  ; program : string
  ; inputs : input list
  ; schemas : (string * (string * string) list) list
  }

let directory (project : Project.t) source =
  Option.value project.root ~default:(Filename.dirname source)
;;

exception Stale of string

let relative root path =
  let strip prefix path =
    if String.starts_with ~prefix path
    then String.sub path (String.length prefix) (String.length path - String.length prefix)
    else path
  in
  strip "./" (strip (Filename.concat root "") path)
;;

let input_json i = `Assoc [ "path", `String i.path; "sha256", `String i.sha256 ]

let entry_json e =
  `Assoc
    [ "source", `String e.source
    ; "dialect", `String e.dialect
    ; "compiler", `String e.compiler
    ; "strict", `Bool e.strict
    ; "program", `String e.program
    ; "inputs", `List (List.map input_json e.inputs)
    ; ( "schemas"
      , `Assoc
          (List.map
             (fun (path, columns) ->
                path, `Assoc (List.map (fun (n, t) -> n, `String t) columns))
             e.schemas) )
    ]
;;

let entry_of json =
  let text key = J.member key json |> J.to_string in
  { source = text "source"
  ; dialect = text "dialect"
  ; compiler = text "compiler"
  ; strict = J.member "strict" json |> J.to_bool
  ; program = text "program"
  ; inputs =
      J.member "inputs" json
      |> J.to_list
      |> List.map (fun i ->
        { path = J.member "path" i |> J.to_string; sha256 = J.member "sha256" i |> J.to_string })
  ; schemas =
      J.member "schemas" json
      |> J.to_assoc
      |> List.map (fun (path, columns) ->
        path, J.to_assoc columns |> List.map (fun (n, t) -> n, J.to_string t))
  }
;;

let load root =
  let path = Filename.concat root file_name in
  if Sys.file_exists path
  then J.member "entries" (Json.from_file path) |> J.to_list |> List.map entry_of
  else []
;;

let save root entries =
  let json = `Assoc [ "version", `Int 1; "entries", `List (List.map entry_json entries) ] in
  Out_channel.with_open_text (Filename.concat root file_name) (fun channel ->
    output_string channel (Json.pretty_to_string json ^ "\n"))
;;

let analysis ~(project : Project.t) ~source declarations =
  let root = directory project source in
  let env, outputs =
    if Analysis_program.tabular declarations
    then
      Analysis_program.analyze
        ~strict:project.strict
        ~directory:(Filename.dirname source)
        declarations
    else [], []
  in
  let datasets =
    List.filter_map
      (function
        | _, Analysis_program.Dataset d -> Some d
        | _ -> None)
      env
    @ List.map fst outputs
  in
  let paths = List.concat_map Analysis_program.sources datasets |> List.sort_uniq compare in
  let schemas =
    List.concat_map Analysis_program.files datasets
    |> List.sort_uniq compare
    |> List.map (fun (file : Analysis_program.source) ->
      ( relative root file.path
      , List.map
          (fun (n, t, nullable) -> n, Analysis_program.kind t ^ if nullable then "?" else "")
          file.columns ))
  in
  let contracts = List.concat_map Analysis_program.contracts datasets |> List.sort_uniq compare in
  let entry =
    { source = relative root source
    ; dialect = Project.dialect_name (Project.require_dialect project)
    ; compiler
    ; strict = project.strict
    ; program = Data_file.digest source
    ; inputs =
        List.map (fun path -> { path = relative root path; sha256 = Data_file.digest path }) paths
    ; schemas
    }
  in
  root, entry, contracts
;;

let write root entry =
  let others =
    List.filter
      (fun e -> not (e.source = entry.source && e.dialect = entry.dialect))
      (load root)
  in
  save root (others @ [ entry ])
;;

let lock ~project ~source declarations =
  let root, entry, contracts = analysis ~project ~source declarations in
  List.iter Contract.validate contracts;
  write root entry;
  entry
;;

let differences locked current =
  let field name a b = if a = b then [] else [ name ] in
  field "the program" locked.program current.program
  @ field "the compiler" locked.compiler current.compiler
  @ field "strict mode" locked.strict current.strict
  @ List.filter_map
      (fun (i : input) ->
         match List.find_opt (fun (l : input) -> l.path = i.path) locked.inputs with
         | Some l when l.sha256 = i.sha256 -> None
         | Some _ -> Some i.path
         | None -> Some (i.path ^ " (new)"))
      current.inputs
;;

let require ~project ~source declarations =
  let root, current, contracts = analysis ~project ~source declarations in
  match
    List.find_opt
      (fun e -> e.source = current.source && e.dialect = current.dialect)
      (load root)
  with
  | Some locked ->
    (match differences locked current with
     | [] -> locked
     | changed ->
       raise
       @@ Stale
         (Printf.sprintf
            "%s changed since rupam lock. Run rupam lock %s, then compile again."
            (String.concat ", " changed)
            current.source))
  | None when current.schemas = [] ->
    List.iter Contract.validate contracts;
    write root current;
    current
  | None ->
    raise
    @@ Stale
      (Printf.sprintf
         "%s reads files whose schemas rupam inferred. Review them with :schema, then \
          run rupam lock %s to commit them."
         current.source
         current.source)
;;
