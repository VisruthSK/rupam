open Rupam

type view =
  | Schema
  | Preview
  | Inspect
  | Explain
  | Contract
  | Validate

type command =
  | Quit
  | Help
  | Reload
  | Bindings
  | Clear
  | Target of Project.dialect option
  | Type of string
  | View of view * string
  | Code of Project.dialect option * string
  | Source of string

let views =
  [ "schema", Schema
  ; "preview", Preview
  ; "inspect", Inspect
  ; "explain", Explain
  ; "contract", Contract
  ; "validate", Validate
  ]
;;

let help =
  [ ":type EXPR", "inferred type, without running anything"
  ; ":schema TABLE", "fields, types and nullability"
  ; ":preview TABLE", "first rows, after contract validation"
  ; ":inspect TABLE", "page through rows and columns"
  ; ":explain TABLE", "relational plan and lowering notes"
  ; ":contract TABLE", "contract source and validation status"
  ; ":validate TABLE", "run data-dict validation now"
  ; ":code [DIALECT] TABLE", "generated code for a table"
  ; ":target DIALECT", "r-dplyr or python-polars, used by :preview and :inspect"
  ; ":bindings", "everything defined in this session"
  ; ":prefix", "inside an unfinished input: check the complete part"
  ; ":reload", "re-read the loaded file"
  ; ":clear", "clear the screen"
  ; ":quit", "leave (or Ctrl-D)"
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
    | "clear" -> Ok Clear
    | "target" when rest = "" -> Ok (Target None)
    | "target" -> Result.map (fun d -> Target (Some d)) (dialect rest)
    | "type" | "t" -> needs "an expression" (fun x -> Type x)
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

let styled fd code text =
  if Unix.isatty fd then "\027[" ^ code ^ "m" ^ text ^ "\027[0m" else text
;;

let report message = prerr_endline (styled Unix.stderr "1;31" "error" ^ ": " ^ message)

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

let echo (types, values) ds =
  List.fold_left
    (fun (types, values) d ->
       let types, t = Infer.declaration types d in
       let values, result = Eval.run values [ d ] in
       let typ = styled Unix.stdout "36" (Infer.display t) in
       (match d, result with
        | Syntax.Let (name, _, _), _ -> Printf.printf "%s : %s\n" name typ
        | Evaluate _, Some v -> Printf.printf "%s : %s\n" (Eval.display v) typ
        | _ -> ());
       types, values)
    (types, values)
    ds
;;

type session =
  { scalar : Infer.env * (string * Eval.value) list
  ; history : Syntax.declaration list
  ; tables : Analysis_program.environment
  ; lexical : Check.env
  ; directory : string
  ; project : Project.t
  ; editor : Line.t
  ; source_path : string option
  }

let empty =
  { scalar = Infer.empty, []
  ; history = []
  ; tables = []
  ; lexical = []
  ; directory = "."
  ; project = Project.standalone
  ; editor = Line.ephemeral
  ; source_path = None
  }
;;

let submit state ds =
  let history = state.history @ ds in
  if contracted history
  then (
    let inputs = if contracted state.history then ds else history in
    let tables, lexical, _ =
      Analysis_program.extend
        ~strict:state.project.strict
        ~directory:state.directory
        state.tables
        state.lexical
        inputs
    in
    { state with history; tables; lexical })
  else { state with scalar = echo state.scalar ds; history }
;;

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

let read state prompt = Line.read state.editor ~complete:(complete state) prompt

let show_prefix state source =
  match Frontend.valid_prefix source with
  | None -> print_endline "No complete prefix yet"
  | Some (text, ds) ->
    print_endline (styled Unix.stdout "2" text);
    ignore
      (diagnostic (Lexing.from_string text) (fun () ->
         if contracted (state.history @ ds)
         then (
           let bindings, _ =
             Analysis_program.analyze
               ~strict:state.project.strict
               ~directory:state.directory
               (state.history @ ds)
           in
           match List.rev ds with
           | Syntax.Let (n, _, _) :: _ ->
             (match List.assoc_opt n bindings with
              | Some (Dataset d) -> Analysis_program.schema d
              | _ -> ())
           | _ -> ())
         else ignore (Infer.program (fst state.scalar) ds)))
;;

let rec continue state source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | ds -> Result.value (diagnostic buf (fun () -> submit state ds)) ~default:state
  | exception Parser.Error when Lexing.lexeme buf = "" ->
    (match read state "  ... " with
     | None | Some "" -> state
     | Some ":prefix" ->
       show_prefix state source;
       continue state source
     | Some more ->
       Line.remember state.editor more;
       continue state (source ^ "\n" ^ more))
  | exception (Parser.Error | Failure _) ->
    ignore (diagnostic buf (fun () -> Frontend.parse (Lexing.from_string source)));
    state
;;

let table state source =
  match Frontend.parse (Lexing.from_string source) with
  | [ Syntax.Evaluate x ] ->
    Analysis_program.dataset ~directory:state.directory state.tables state.lexical x
  | _ -> failwith "Expected a table expression"
;;

let page_size = 20
let page_width = 8

let browse state view d =
  let page =
    let dialect = Project.require_dialect state.project in
    let runtime = Runtime.resolve state.project (Runtime.of_dialect dialect) in
    match dialect with
    | R_dplyr -> Tidyverse.preview_page ~runtime
    | Python_polars -> Analysis_program.preview_page ~runtime
  in
  let d = Analysis_program.public_view d in
  let fields = Table.names d.plan in
  let rec show offset column =
    let ns = List.filteri (fun i _ -> i >= column && i < column + page_width) fields in
    let schema = List.map (fun n -> n, List.assoc n d.plan.schema) ns in
    page
      { d with plan = { d.plan with schema; steps = Select ns :: d.plan.steps } }
      ~offset
      ~limit:page_size;
    match view with
    | Inspect ->
      (match read state "[n]ext [p]rev [l]eft [h] right [q]uit > " with
       | Some "n" -> show (offset + page_size) column
       | Some "p" -> show (max 0 (offset - page_size)) column
       | Some "l" ->
         show offset (min (max 0 (List.length fields - page_width)) (column + page_width))
       | Some "h" -> show offset (max 0 (column - page_width))
       | _ -> ())
    | _ -> ()
  in
  show 0 0
;;

let reload state =
  let history =
    match state.source_path with
    | None -> state.history
    | Some p ->
      In_channel.with_open_text p (fun ch -> Frontend.parse (Lexing.from_channel ch))
  in
  submit
    { state with history = []; tables = []; lexical = []; scalar = Infer.empty, [] }
    history
;;

let run state = function
  | Quit -> None
  | Help ->
    List.iter (fun (usage, text) -> Printf.printf "  %-24s %s\n" usage text) help;
    Some state
  | Clear ->
    Line.clear ();
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
      state.tables;
    List.iter
      (fun (name, (_, t)) -> Printf.printf "%s : %s\n" name (Infer.display t))
      (fst state.scalar).values;
    Some state
  | Type source ->
    let buf = Lexing.from_string source in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ Syntax.Evaluate x ] ->
           (match Infer.expression (fst state.scalar) 1 x with
            | t -> print_endline (Infer.display t)
            | exception Infer.Error _ when contracted state.history ->
              Analysis_program.schema
                (Analysis_program.dataset
                   ~directory:state.directory
                   state.tables
                   state.lexical
                   x))
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
         | Preview | Inspect -> browse state view d));
    Some state
  | Source "" -> Some state
  | Source line -> Some (continue state line)
;;

let rec loop state =
  match read state "rupam> " with
  | None -> ()
  | Some line ->
    if String.trim line <> "" then Line.remember state.editor line;
    (match parse line with
     | Error message ->
       report message;
       loop state
     | Ok command -> Option.iter loop (run state command))
;;

let open_session ~project ?path editor =
  let session = { empty with project; editor } in
  match path with
  | None -> Some session
  | Some path ->
    let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
    Lexing.set_filename buf path;
    diagnostic buf (fun () ->
      submit
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

let start ~project ?path () =
  Sys.catch_break true;
  Option.iter loop (open_session ~project ?path (Line.persistent ()))
;;
