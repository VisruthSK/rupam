open Rupam

type view =
  | Schema
  | Preview
  | Explain
  | Contract
  | Validate

type command =
  | Quit
  | Help
  | Reload
  | Bindings
  | Target of Project.dialect option
  | Type of string
  | View of view * string
  | Code of Project.dialect option * string
  | Source of string

let views =
  [ "schema", Schema
  ; "preview", Preview
  ; "explain", Explain
  ; "contract", Contract
  ; "validate", Validate
  ]
;;

let help =
  [ ":type EXPR", "inferred type, without running anything"
  ; ":schema [TABLE]", "columns and types; every table when no name is given"
  ; ":preview TABLE", "first rows, after contract validation"
  ; ":explain TABLE", "plan from source to result"
  ; ":contract TABLE", "where a table's rows come from"
  ; ":validate TABLE", "run data-dict validation now"
  ; ":code [DIALECT] TABLE", "generated code for a table"
  ; ":target [DIALECT]", "show or set the dialect: r-dplyr or python-polars"
  ; ":bindings", "everything defined in this session"
  ; ":reload", "re-read the loaded file"
  ; ":quit", "leave (or Ctrl+D)"
  ]
;;

let command_names =
  List.map (fun (usage, _) -> List.hd (String.split_on_char ' ' usage)) help
;;

let parse line =
  let line = String.trim line in
  let verb, rest =
    match String.index_opt line ' ' with
    | Some i ->
      String.sub line 0 i, String.trim (String.sub line i (String.length line - i))
    | None -> line, ""
  in
  let dialect name =
    match List.assoc_opt name Project.dialects with
    | Some d -> Ok d
    | None ->
      Error
        ("Unknown dialect "
         ^ name
         ^ ". Expected "
         ^ String.concat " or " (List.map fst Project.dialects)
         ^ ".")
  in
  let needs what k =
    if rest = "" then Error (verb ^ " expects " ^ what) else Ok (k rest)
  in
  if not (String.starts_with ~prefix:":" line)
  then Ok (Source line)
  else (
    match String.sub verb 1 (String.length verb - 1) with
    | "quit" | "q" -> Ok Quit
    | "help" | "h" -> Ok Help
    | "reload" -> Ok Reload
    | "bindings" -> Ok Bindings
    | "target" when rest = "" -> Ok (Target None)
    | "target" -> Result.map (fun d -> Target (Some d)) (dialect rest)
    | "type" | "t" -> needs "an expression" (fun x -> Type x)
    | "schema" when rest = "" -> Ok (View (Schema, ""))
    | "code" ->
      let first, after =
        match String.index_opt rest ' ' with
        | Some i ->
          String.sub rest 0 i, String.trim (String.sub rest i (String.length rest - i))
        | None -> rest, ""
      in
      (match List.assoc_opt first Project.dialects with
       | Some d when after <> "" -> Ok (Code (Some d, after))
       | _ -> needs "a table" (fun x -> Code (None, x)))
    | name ->
      (match List.assoc_opt name views with
       | Some v -> needs "a table" (fun x -> View (v, x))
       | None -> Error ("Unknown command " ^ verb ^ ". Type :help for the list.")))
;;

let report message = prerr_endline ("error: " ^ message)

let position (p : Lexing.position) =
  Printf.sprintf
    "%s%d:%d"
    (if p.pos_fname = "" then "" else p.pos_fname ^ ":")
    p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)
;;

let diagnostic buf f =
  try Ok (f ()) with
  | Sys.Break ->
    prerr_endline "Cancelled";
    Error ()
  | Infer.Error ((p, _), msg) | Eval.Error ((p, _), msg) ->
    report (position p ^ ": " ^ msg);
    Error ()
  | Parser.Error ->
    report (position buf.Lexing.lex_curr_p ^ ": syntax error");
    Error ()
  | Failure msg | Shape.Error msg ->
    report (position buf.Lexing.lex_curr_p ^ ": " ^ msg);
    Error ()
;;

let contracted = Analysis_program.tabular

let execute (types, values) ds =
  let types = Infer.program types ds in
  let values, result = Eval.run values ds in
  Option.iter (fun v -> print_endline (Eval.display v)) result;
  types, values
;;

type session =
  { scalar : Infer.env * (string * Eval.value) list
  ; history : Syntax.declaration list
  ; tables : Analysis_program.environment
  ; lexical : Check.env
  ; directory : string
  ; project : Project.t
  ; source_path : string option
  }

let empty =
  { scalar = Infer.empty, []
  ; history = []
  ; tables = []
  ; lexical = []
  ; directory = "."
  ; project = Project.standalone
  ; source_path = None
  }
;;

let dataset state x =
  Analysis_program.dataset ~directory:state.directory state.tables state.lexical x
;;

let preview state d =
  let dialect = Project.require_dialect state.project in
  let runtime = Runtime.resolve state.project (Runtime.of_dialect dialect) in
  let d = Analysis_program.public_view d in
  match dialect with
  | R_dplyr -> Tidyverse.preview_page ~runtime d ~offset:0 ~limit:20
  | Python_polars -> Analysis_program.preview_page ~runtime d ~offset:0 ~limit:20
;;

let echo ?(announce = true) state d =
  let types, values = state.scalar in
  let types, t = Infer.declaration types d in
  let values, result = Eval.run values [ d ] in
  (match d, result with
   | _ when not announce -> ()
   | Syntax.Let (name, _, _), _ -> Printf.printf "%s : %s\n" name (Infer.display t)
   | Evaluate _, Some v -> Printf.printf "%s : %s\n" (Eval.display v) (Infer.display t)
   | _ -> ());
  { state with scalar = types, values }
;;

let extend state ds =
  let tables, lexical, _ =
    Analysis_program.extend
      ~strict:state.project.strict
      ~directory:state.directory
      state.tables
      state.lexical
      ds
  in
  { state with tables; lexical }
;;

let tabular state =
  let table name =
    match List.assoc_opt name state.tables with
    | Some (Analysis_program.Dataset _ | Dictionary _) -> true
    | Some (Function _) | None -> false
  in
  function
  | Syntax.Contract _ | Import _ -> true
  | Let (_, None, x) | Evaluate x -> Analysis_program.uses table x
  | Let (_, Some _, _) | Alias _ | Type _ -> false
;;

let writes = function
  | Syntax.Evaluate { form = Call ({ form = Name (Bare, "write_parquet"); _ }, _); _ } ->
    true
  | _ -> false
;;

let observe state name =
  match List.assoc_opt name state.tables with
  | Some (Analysis_program.Dictionary contract) ->
    Contract.describe contract;
    true
  | Some (Function (params, _, _, _)) ->
    Printf.printf "%s : fn(%s)\n" name (String.concat ", " params);
    true
  | Some (Dataset _) | None -> false
;;

let declare ~announce state d =
  let state = { state with history = state.history @ [ d ] } in
  match d with
  | Syntax.Evaluate { form = Name (Bare, name); _ } when observe state name -> state
  | Syntax.Let (name, Some params, _) ->
    let state = extend state [ d ] in
    (match echo ~announce state d with
     | state -> state
     | exception Infer.Error _ ->
       if announce then Printf.printf "%s : fn(%s)\n" name (String.concat ", " params);
       state)
  | Evaluate x when tabular state d && not (writes d) ->
    preview state (dataset state x);
    state
  | d when tabular state d -> extend state [ d ]
  | Let (_, None, { form = Literal _; _ }) -> echo ~announce (extend state [ d ]) d
  | d -> echo ~announce state d
;;

let submit ?(announce = true) state ds = List.fold_left (declare ~announce) state ds

let names state =
  List.map fst state.tables @ List.map fst (fst state.scalar).values
  |> List.sort_uniq compare
;;

let keywords = [ "let"; "match"; "type"; "fn"; "if"; "else"; "use"; "contract" ]

let complete state line =
  let start =
    match String.rindex_opt line ' ' with
    | Some i -> i + 1
    | None -> 0
  in
  let word = String.sub line start (String.length line - start) in
  let candidates =
    if start = 0 && String.starts_with ~prefix:":" word
    then command_names
    else names state @ keywords
  in
  start, List.filter (fun c -> String.starts_with ~prefix:word c && c <> word) candidates
;;

let evaluate state source =
  let buf = Lexing.from_string source in
  Result.value
    (diagnostic buf (fun () -> submit state (Frontend.parse buf)))
    ~default:state
;;

let table state source =
  match Frontend.parse (Lexing.from_string source) with
  | [ Syntax.Evaluate x ] -> dataset state x
  | _ -> failwith "Expected a table expression"
;;

let schemas state =
  List.iter
    (fun (name, binding) ->
       match binding with
       | Analysis_program.Dataset d ->
         print_endline name;
         Analysis_program.schema d
       | Dictionary contract -> Contract.describe contract
       | Function _ -> ())
    (List.rev state.tables)
;;

let reload state =
  let history =
    match state.source_path with
    | None -> state.history
    | Some p ->
      In_channel.with_open_text p (fun ch -> Frontend.parse (Lexing.from_channel ch))
  in
  submit
    ~announce:false
    { state with history = []; tables = []; lexical = []; scalar = Infer.empty, [] }
    history
;;

let run state = function
  | Quit -> None
  | Help ->
    List.iter (fun (usage, text) -> Printf.printf "  %-24s %s\n" usage text) help;
    Some state
  | Target None ->
    print_endline
      (match state.project.dialect with
       | Some d -> Project.dialect_name d
       | None ->
         "No dialect yet. Choose one with :target r-dplyr or :target python-polars.");
    Some state
  | Target (Some dialect) ->
    Some { state with project = { state.project with dialect = Some dialect } }
  | Reload ->
    Some
      (Result.value
         (diagnostic (Lexing.from_string "") (fun () -> reload state))
         ~default:state)
  | Bindings ->
    List.iter
      (fun (name, b) ->
         Printf.printf
           "%s : %s\n"
           name
           (match b with
            | Analysis_program.Dataset _ -> "Table"
            | Dictionary _ -> "Contract"
            | Function _ -> "function"))
      (List.rev state.tables);
    List.iter
      (fun (name, (_, t)) -> Printf.printf "%s : %s\n" name (Infer.display t))
      (List.rev (fst state.scalar).values);
    Some state
  | Type source ->
    let buf = Lexing.from_string source in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ (Syntax.Evaluate x as d) ] when tabular state d ->
           Analysis_program.schema (dataset state x)
         | [ Syntax.Evaluate x ] ->
           print_endline (Infer.display (Infer.expression (fst state.scalar) 1 x))
         | _ -> failwith "Expected one expression"));
    Some state
  | Code (chosen, source) ->
    ignore
      (diagnostic (Lexing.from_string source) (fun () ->
         let d = table state source in
         print_endline
           (match
              Option.value chosen ~default:(Project.require_dialect state.project)
            with
            | Project.R_dplyr -> R_syntax.render (Tidyverse.table d)
            | Python_polars -> Python_syntax.render (Analysis_program.python_table d))));
    Some state
  | View (Schema, "") ->
    schemas state;
    Some state
  | View (Schema, name) when List.mem_assoc name state.tables ->
    (match List.assoc name state.tables with
     | Analysis_program.Dictionary contract -> Contract.describe contract
     | Dataset d -> Analysis_program.schema d
     | Function _ -> report (name ^ " is a function, not a table"));
    Some state
  | View (view, source) ->
    ignore
      (diagnostic (Lexing.from_string source) (fun () ->
         let d = table state source in
         match view with
         | Schema -> Analysis_program.schema d
         | Contract ->
           print_endline
             (match d.origin with
              | Contracted { dictionary; table; _ } ->
                Printf.sprintf "%s (%s), values pending" dictionary.path table.name
              | File { path; _ } -> path ^ " has no contract; its schema is inferred"
              | Joined _ -> "joined from several sources; see :explain")
         | Validate ->
           Analysis_program.validate
             ~runtime:(Runtime.resolve state.project Runtime.Python)
             d
         | Explain -> Analysis_program.explain d
         | Preview -> preview state d));
    Some state
  | Source "" -> Some state
  | Source source -> Some (evaluate state source)
;;

let open_session ~project ?path () =
  let session = { empty with project } in
  match path with
  | None -> Some session
  | Some path ->
    let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
    Lexing.set_filename buf path;
    diagnostic buf (fun () ->
      submit
        ~announce:false
        { session with directory = Filename.dirname path; source_path = Some path }
        (Frontend.parse buf))
    |> Result.to_option
;;

let incomplete source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | _ -> false
  | exception Parser.Error -> Lexing.lexeme buf = ""
  | exception Failure _ -> false
;;

let rec loop state pending =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    let entry = if pending = "" then line else pending ^ "\n" ^ line in
    if (not (String.starts_with ~prefix:":" (String.trim entry))) && incomplete entry
    then loop state entry
    else (
      match parse entry with
      | Error message ->
        report message;
        loop state ""
      | Ok command ->
        let next = run state command in
        flush stdout;
        Option.iter (fun state -> loop state "") next)
;;

let start ~project ?path () =
  Sys.catch_break true;
  Option.iter (fun state -> loop state "") (open_session ~project ?path ())
;;
