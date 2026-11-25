open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let compiler = "rupam 0.1.0"
let file_name = "rupam.lock"

type input =
  { path : string
  ; sha256 : string
  }
[@@deriving yojson]

type column =
  { name : string
  ; type_ : string [@key "type"]
  }
[@@deriving yojson]

type schema =
  { path : string
  ; columns : column list
  }
[@@deriving yojson]

type entry =
  { source : string
  ; dialect : Project.dialect
  ; compiler : string
  ; strict : bool
  ; inputs : input list
  ; schemas : schema list
  }
[@@deriving yojson]

type file =
  { version : int
  ; entries : entry list
  }
[@@deriving yojson]

let version = 1

let directory (project : Project.t) source =
  Option.value project.root ~default:(Filename.dirname source)
;;

let relative root path =
  let strip prefix path =
    if String.starts_with ~prefix path
    then String.sub path (String.length prefix) (String.length path - String.length prefix)
    else path
  in
  strip "./" (strip (Filename.concat root "") path)
;;

let load root =
  let path = Filename.concat root file_name in
  if not (Sys.file_exists path)
  then []
  else (
    let file = file_of_yojson (Yojson.Safe.from_file path) in
    if file.version <> version
    then
      Diagnostic.fail
        Stale_lock
        (Printf.sprintf
           "%s was written by an older rupam. Run rupam lock again to replace it."
           path);
    file.entries)
;;

let save root entries =
  Out_channel.with_open_text (Filename.concat root file_name) (fun channel ->
    output_string
      channel
      (Yojson.Safe.pretty_to_string (yojson_of_file { version; entries }) ^ "\n"))
;;

let print_schemas entry =
  List.iter
    (fun schema ->
       print_endline schema.path;
       List.iter (fun c -> Printf.printf "  %s  %s\n" c.name c.type_) schema.columns)
    entry.schemas
;;

let analysis ~(project : Project.t) ~source declarations =
  let root = directory project source in
  let env, outputs =
    if Analysis_program.tabular declarations
    then
      Analysis_program.analyze
        ~strict:project.strict.value
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
  let paths =
    List.concat_map Analysis_program.sources datasets |> List.sort_uniq compare
  in
  let schemas =
    List.concat_map Analysis_program.files datasets
    |> List.sort_uniq compare
    |> List.map (fun (file : Analysis_program.source) ->
      { path = relative root file.path
      ; columns =
          List.map
            (fun (name, t, nullable) ->
               { name; type_ = (Shape.primitive_name t ^ if nullable then "?" else "") })
            file.columns
      })
  in
  let contracts =
    List.concat_map Analysis_program.contracts datasets |> List.sort_uniq compare
  in
  let entry =
    { source = relative root source
    ; dialect = Project.require_dialect project
    ; compiler
    ; strict = project.strict.value
    ; inputs =
        List.map
          (fun path : input ->
             { path = relative root path; sha256 = Data_file.digest path })
          paths
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

let establish ~project ~source declarations =
  let root, entry, contracts = analysis ~project ~source declarations in
  List.iter Contract.validate contracts;
  write root entry;
  entry
;;

let differences locked current =
  let field name a b = if a = b then [] else [ name ] in
  field "the compiler" locked.compiler current.compiler
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
       Diagnostic.fail
         Stale_lock
         (Printf.sprintf
            "%s changed since rupam lock. Run rupam lock %s, then compile again."
            (String.concat ", " changed)
            current.source))
  | None when current.schemas = [] ->
    List.iter Contract.validate contracts;
    write root current;
    current
  | None ->
    Diagnostic.fail
      Stale_lock
      (Printf.sprintf
         "%s reads files whose schemas rupam inferred. Review them with :schema, then \
          run rupam lock %s to commit them."
         current.source
         current.source)
;;
