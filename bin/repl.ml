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
  | Options
  | Reload
  | Lock
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
  ; ":preview TABLE", "first rows"
  ; ":explain TABLE", "plan from source to result"
  ; ":contract TABLE", "where a table's rows come from"
  ; ":validate TABLE", "run data-dict validation now"
  ; ":code [DIALECT] TABLE", "generated code for a table"
  ; ":target [DIALECT]", "show or set the dialect: r-dplyr or python-polars"
  ; ":options", "effective settings and their sources"
  ; ":bindings", "everything defined in this session"
  ; ":reload", "re-read the loaded file"
  ; ":lock", "validate the loaded file's inputs and record them in rupam.lock"
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
    | "options" when rest = "" -> Ok Options
    | "lock" -> Ok Lock
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
let report_diagnostic d = prerr_endline (Diagnostic.render d)

let diagnostic buf f =
  try Ok (f ()) with
  | Sys.Break ->
    prerr_endline "Cancelled";
    Error ()
  | Parser.Error ->
    let token = Lexing.lexeme buf in
    report_diagnostic
      (if token = ""
       then
         { code = Incomplete_input
         ; span = Some (buf.lex_start_p, buf.lex_curr_p)
         ; message = "The input ends too early"
         }
       else
         { code = Syntax_error
         ; span = Some (buf.lex_start_p, buf.lex_curr_p)
         ; message = "Unexpected " ^ token
         });
    Error ()
  | exn ->
    let d = Diagnostic.of_exn exn in
    let span = buf.Lexing.lex_start_p, buf.Lexing.lex_curr_p in
    report_diagnostic { d with span = Some (Option.value d.span ~default:span) };
    Error ()
;;

type session =
  { env : Elaborate.env
  ; declarations : Syntax.declaration list
  ; worker : Dialect.session option
  ; directory : string
  ; project : Project.t
  ; source_path : string option
  ; show : digits:int -> Page.t -> unit
  }

let fresh_env ~directory (project : Project.t) =
  Elaborate.empty ~directory ~strict:project.strict.value
;;

let empty project =
  { env = fresh_env ~directory:"." project
  ; declarations = []
  ; worker = None
  ; directory = "."
  ; project
  ; source_path = None
  ; show = (fun ~digits page -> List.iter print_endline (Page.plain ~digits page))
  }
;;

let runtime state =
  let language = Runtime.of_dialect (Project.require_dialect state.project) in
  Runtime.resolve state.project language, language
;;

let execute state code =
  let runtime, language = runtime state in
  Worker.evaluate runtime language code
;;

let worker state =
  match state.worker with
  | Some w -> state, w
  | None ->
    let dialect = Project.require_dialect state.project in
    let state = { state with worker = Some (Dialect.session dialect) } in
    ignore (execute state (Dialect.preamble dialect));
    state, Option.get state.worker
;;

let preview state (t : Core.table) =
  let state, w = worker state in
  let path = Filename.temp_file "rupam" ".parquet" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
       let total = execute state (Dialect.preview w t ~offset:0 ~limit:20 ~path) in
       state.show
         ~digits:state.project.preview_digits.value
         (Page.read
            ~path
            ~total:(int_of_string (String.trim total))
            ~ordered:(t.order <> None)
            ~offset:0
            t.schema));
  state
;;

let print_schema state (t : Core.table) =
  let width = List.fold_left (fun w (n, _) -> max w (String.length n)) 0 t.schema in
  List.iter
    (fun (n, typ) -> Printf.printf "%-*s  %s\n" width n (Types.display typ))
    t.schema;
  Printf.printf
    "%d columns from %s\n"
    (List.length t.schema)
    (Elaborate.origin state.env t)
;;

let announce env (d : Syntax.declaration) =
  match d with
  | Let (name, _, _) ->
    (match Elaborate.lookup env name with
     | Some (Table_value _) | None -> ()
     | Some b ->
       Option.iter (fun t -> Printf.printf "%s : %s\n" name t) (Elaborate.describe b))
  | _ -> ()
;;

let observe state (d : Syntax.declaration) =
  match d with
  | Evaluate { form = Name (Bare, name); _ } ->
    (match Elaborate.lookup state.env name with
     | Some (Contract contract) ->
       Contract.describe contract;
       true
     | Some (Function _ as f) ->
       Option.iter (fun t -> Printf.printf "%s : %s\n" name t) (Elaborate.describe f);
       true
     | _ -> false)
  | Evaluate { form = Field ({ form = Name (Bare, owner); _ }, name); _ } ->
    (match Elaborate.lookup state.env owner with
     | Some (Contract contract) ->
       (match
          List.find_opt (fun (t : Contract.table) -> t.name = name) contract.tables
        with
        | Some t ->
          Contract.describe_table t;
          true
        | None -> false)
     | _ -> false)
  | _ -> false
;;

let declare ~announce:loud state d =
  if loud && observe state d
  then state
  else (
    let env, items = Elaborate.item state.env d in
    let definitions =
      List.filter
        (function
          | Core.Define _ | Write _ -> true
          | Show _ | Variant _ -> false)
        items
    in
    let state =
      if definitions = []
      then state
      else (
        let state, w = worker state in
        print_string (execute state (Dialect.statements w definitions));
        state)
    in
    let state = { state with env; declarations = state.declarations @ [ d ] } in
    if loud then announce env d;
    List.fold_left
      (fun state (item : Core.item) ->
         match item with
         | Show { node = Table t; _ } -> if loud then preview state t else state
         | Show e ->
           let state, w = worker state in
           let value = String.trim (execute state (Dialect.statements w [ item ])) in
           if loud then Printf.printf "%s : %s\n" value (Types.display e.typ);
           state
         | Define _ | Write _ | Variant _ -> state)
      state
      items)
;;

let submit ?(announce = true) state ds = List.fold_left (declare ~announce) state ds

type suggestion =
  { text : string
  ; detail : string
  }

let verbs =
  [ "filter", "condition, missing = skip | keep | error", [ "missing" ]
  ; "select", "columns to keep, in order", []
  ; "mutate", "name = value, ..., by = key", [ "by"; "missing"; "group_missing" ]
  ; "rename", "new = old", []
  ; "summarize", "name = reduction, ..., by = key", [ "by"; "missing"; "group_missing" ]
  ; "arrange", "key, desc(key)", []
  ; "slice_head", "n = rows of an arranged table", [ "n" ]
  ; "count", "columns to count rows by", []
  ; "distinct", "columns", []
  ; "left_join", "other, by = key, relationship = many_to_one", [ "by"; "relationship" ]
  ; "inner_join", "other, by = key, relationship = many_to_one", [ "by"; "relationship" ]
  ; "right_join", "other, by = key, relationship = one_to_many", [ "by"; "relationship" ]
  ; "full_join", "other, by = key, relationship = one_to_one", [ "by"; "relationship" ]
  ; "semi_join", "other, by = key", [ "by" ]
  ; "anti_join", "other, by = key", [ "by" ]
  ; ( "pivot_longer"
    , "cols = (a, b), names_to = k, values_to = v"
    , [ "cols"; "names_to"; "values_to" ] )
  ; ( "pivot_wider"
    , "names_from = k, values_from = v, levels = [..]"
    , [ "names_from"; "values_from"; "levels" ] )
  ; "print", "show the table", []
  ; "write_parquet", "path", []
  ]
;;

let choices = function
  | "missing" | "group_missing" -> [ "skip"; "keep"; "error" ]
  | "relationship" -> [ "one_to_one"; "many_to_one"; "one_to_many"; "many_to_many" ]
  | _ -> []
;;

let keywords = [ "let"; "match"; "type"; "fn"; "if"; "else"; "use"; "contract" ]

let identifier c =
  c = '_'
  || c = '$'
  || c = '^'
  || Char.lowercase_ascii c <> Char.uppercase_ascii c
  || (c >= '0' && c <= '9')
;;

let word_start line =
  let rec go i = if i > 0 && identifier line.[i - 1] then go (i - 1) else i in
  go (String.length line)
;;

let backward line from stop =
  let rec go i depth =
    if i < 0
    then None
    else (
      match line.[i] with
      | ')' | ']' | '}' -> go (i - 1) (depth + 1)
      | ('(' | '[' | '{') when depth > 0 -> go (i - 1) (depth - 1)
      | c when depth = 0 && stop line i c -> Some i
      | _ -> go (i - 1) depth)
  in
  go from 0
;;

let opening line from = backward line from (fun _ _ c -> c = '(' || c = '[' || c = '{')

let expression_start line from =
  match
    backward line from (fun line i c ->
      match c with
      | '(' | '[' | '{' | ',' -> true
      | '=' ->
        let before = if i > 0 then line.[i - 1] else ' ' in
        let after = if i + 1 < String.length line then line.[i + 1] else ' ' in
        after <> '=' && not (String.contains "=!<>" before)
      | _ -> false)
  with
  | Some i -> i + 1
  | None -> 0
;;

let trim_right s =
  let rec go n =
    if n > 0 && (s.[n - 1] = ' ' || s.[n - 1] = '\n') then go (n - 1) else n
  in
  String.sub s 0 (go (String.length s))
;;

let ends_with_pipe s = String.ends_with ~suffix:"|>" (trim_right s)

let table state source =
  match Frontend.parse (Lexing.from_string source) with
  | [ (Syntax.Evaluate _ as d) ] ->
    (match Elaborate.item state.env d with
     | _, [ Show { node = Table t; _ } ] -> t
     | _ -> Diagnostic.fail Not_a_table "Expected a table expression")
  | _ -> Diagnostic.fail Not_a_table "Expected a table expression"
;;

let piped state line pipe =
  let start = expression_start line (pipe - 1) in
  match table state (String.trim (String.sub line start (pipe - start))) with
  | t -> Some t
  | exception _ -> None
;;

let columns (t : Core.table) =
  List.map (fun (n, typ) -> { text = n; detail = Types.display typ }) t.schema
;;

let bindings state =
  List.map
    (fun (n, (b : Elaborate.binding), detail) ->
       let detail =
         match b with
         | Table_value _ -> "table"
         | _ -> detail
       in
       n, b, { text = n; detail })
    (Elaborate.bindings state.env)
;;

let functions state =
  List.map (fun (text, detail) -> { text; detail }) (Elaborate.builtins ())
  @ List.filter_map
      (fun (_, (b : Elaborate.binding), s) ->
         match b with
         | Function _ -> Some s
         | _ -> None)
      (bindings state)
;;

let values state =
  List.filter_map
    (fun (_, (b : Elaborate.binding), s) ->
       match b with
       | Function _ -> None
       | _ -> Some s)
    (bindings state)
;;

let tables state =
  List.filter_map
    (fun (_, (b : Elaborate.binding), s) ->
       match b with
       | Table_value _ | Contract _ -> Some s
       | _ -> None)
    (bindings state)
;;

let dialects = List.map (fun (n, _) -> { text = n; detail = "dialect" }) Project.dialects

let files state prefix =
  let directory = Filename.concat state.directory (Filename.dirname prefix) in
  match Sys.readdir directory with
  | entries ->
    Array.to_list entries
    |> List.sort compare
    |> List.map (fun e ->
      let path =
        if Filename.dirname prefix = "." && not (String.starts_with ~prefix:"./" prefix)
        then e
        else Filename.concat (Filename.dirname prefix) e
      in
      { text = path
      ; detail =
          (if Sys.is_directory (Filename.concat directory e) then "directory" else "file")
      })
  | exception Sys_error _ -> []
;;

let option_name argument =
  if String.ends_with ~suffix:"=" argument
  then String.trim (String.sub argument 0 (String.length argument - 1))
  else ""
;;

let command_candidates line =
  let words = String.split_on_char ' ' line |> List.filter (( <> ) "") in
  let fresh = String.ends_with ~suffix:" " line in
  match words, fresh with
  | ([] | [ _ ]), false ->
    `Commands
      (List.map
         (fun (usage, text) ->
            { text = List.hd (String.split_on_char ' ' usage); detail = text })
         help)
  | [ ":target" ], true | [ ":target"; _ ], false -> `Dialects
  | [ ":code" ], true | [ ":code"; _ ], false -> `Code
  | _ -> `Tables
;;

let complete state line =
  let start = word_start line in
  let word = String.sub line start (String.length line - start) in
  let quotes = List.length (String.split_on_char '"' line) - 1 in
  let candidates, start, word =
    if String.starts_with ~prefix:":" (String.trim line)
    then (
      match command_candidates line with
      | `Commands cs -> cs, start - 1, ":" ^ word
      | `Dialects -> dialects, start, word
      | `Code -> dialects @ tables state, start, word
      | `Tables -> tables state, start, word)
    else if quotes mod 2 = 1
    then (
      let quote = String.rindex line '"' + 1 in
      let prefix = String.sub line quote (String.length line - quote) in
      files state prefix, quote, prefix)
    else if start > 0 && line.[start - 1] = '.'
    then (
      let base = String.sub line 0 (start - 1) in
      let owner =
        String.sub base (word_start base) (String.length base - word_start base)
      in
      match Elaborate.lookup state.env owner with
      | Some (Contract contract) ->
        ( List.map
            (fun (t : Contract.table) -> { text = t.name; detail = "table" })
            contract.tables
        , start
        , word )
      | _ -> [], start, word)
    else if ends_with_pipe (String.sub line 0 start)
    then
      ( List.map (fun (v, d, _) -> { text = v; detail = d }) verbs
        @ List.filter (fun s -> String.contains s.detail '>') (functions state)
      , start
      , word )
    else (
      let inside =
        match opening line (start - 1) with
        | Some paren ->
          let callee = String.sub line 0 paren in
          let callee_start = word_start callee in
          let verb = String.sub callee callee_start (paren - callee_start) in
          let before = String.sub line 0 callee_start in
          (match List.find_opt (fun (v, _, _) -> v = verb) verbs with
           | Some (_, _, options) when ends_with_pipe before ->
             let pipe = String.length (trim_right before) - 2 in
             Some (options, piped state line pipe, paren)
           | _ -> None)
        | None -> None
      in
      let argument =
        match inside with
        | Some (_, _, paren) ->
          let from =
            match backward line (start - 1) (fun _ _ c -> c = ',') with
            | Some comma when comma > paren -> comma + 1
            | _ -> paren + 1
          in
          String.trim (String.sub line from (start - from))
        | None -> ""
      in
      match inside with
      | Some (options, _, _) when List.mem (option_name argument) options ->
        let option = option_name argument in
        List.map (fun c -> { text = c; detail = option }) (choices option), start, word
      | Some (options, table, _) ->
        ( Option.fold ~none:[] ~some:columns table
          @ List.map (fun o -> { text = o ^ " = "; detail = "option" }) options
          @ functions state
          @ values state
        , start
        , word )
      | None ->
        ( values state
          @ functions state
          @ List.map (fun k -> { text = k; detail = "keyword" }) keywords
        , start
        , word ))
  in
  let seen = Hashtbl.create 16 in
  ( start
  , List.filter
      (fun c ->
         let fresh = not (Hashtbl.mem seen c.text) in
         Hashtbl.replace seen c.text ();
         fresh && String.starts_with ~prefix:word c.text && c.text <> word)
      candidates )
;;

let evaluate state source =
  let buf = Lexing.from_string source in
  Result.value
    (diagnostic buf (fun () -> submit state (Frontend.parse buf)))
    ~default:state
;;

let replay state =
  let declarations =
    match state.source_path with
    | None -> state.declarations
    | Some p ->
      In_channel.with_open_text p (fun ch -> Frontend.parse (Lexing.from_channel ch))
  in
  submit
    ~announce:false
    { state with
      env = fresh_env ~directory:state.directory state.project
    ; declarations = []
    ; worker = None
    }
    declarations
;;

let schemas state =
  List.iter
    (fun (name, (b : Elaborate.binding), _) ->
       match b with
       | Table_value { table; _ } ->
         print_endline name;
         print_schema state table
       | Contract contract -> Contract.describe contract
       | _ -> ())
    (bindings state)
;;

let dispatch state = function
  | Quit -> None
  | Help ->
    List.iter (fun (usage, text) -> Printf.printf "  %-24s %s\n" usage text) help;
    Some state
  | Options ->
    List.iter
      (fun (name, value, source) -> Printf.printf "%-16s %-14s %s\n" name value source)
      (Project.options state.project);
    Some state
  | Target None ->
    print_endline
      (match state.project.dialect.value with
       | Some d -> Project.dialect_name d
       | None ->
         "No dialect yet. Choose one with :target r-dplyr or :target python-polars.");
    Some state
  | Target (Some dialect) ->
    let state =
      { state with
        project =
          { state.project with
            dialect = { value = Some dialect; origin = Session ":target" }
          }
      }
    in
    Some
      (if state.declarations = []
       then { state with worker = None }
       else
         Result.value
           (diagnostic (Lexing.from_string "") (fun () -> replay state))
           ~default:{ state with worker = None })
  | Lock ->
    (match state.source_path with
     | None -> report "No file loaded. Start the REPL with a program to lock it."
     | Some source ->
       ignore
         (diagnostic (Lexing.from_string "") (fun () ->
            let _, items =
              Elaborate.program
                (fresh_env ~directory:state.directory state.project)
                state.declarations
            in
            let entry = Lock.establish ~project:state.project ~source items in
            Lock.print_schemas entry;
            Printf.printf
              "Locked %s for %s\n"
              entry.source
              (Project.dialect_name entry.dialect))));
    Some state
  | Reload ->
    Some
      (Result.value
         (diagnostic (Lexing.from_string "") (fun () -> replay state))
         ~default:state)
  | Bindings ->
    List.iter
      (fun (name, _, s) -> Printf.printf "%s : %s\n" name s.detail)
      (bindings state);
    Some state
  | Type source ->
    let buf = Lexing.from_string source in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ (Syntax.Evaluate _ as d) ] ->
           (match Elaborate.item state.env d with
            | _, [ Show { node = Table t; _ } ] -> print_schema state t
            | _, [ Show e ] -> print_endline (Types.display e.typ)
            | _ -> ())
         | _ -> Diagnostic.fail Wrong_arguments "Expected one expression"));
    Some state
  | Code (chosen, source) ->
    ignore
      (diagnostic (Lexing.from_string source) (fun () ->
         let dialect =
           Option.value chosen ~default:(Project.require_dialect state.project)
         in
         let session =
           match state.worker with
           | Some w when w.dialect = dialect -> w
           | _ -> Dialect.session dialect
         in
         print_endline (Dialect.table session (table state source))));
    Some state
  | View (Schema, "") ->
    schemas state;
    Some state
  | View (view, source) ->
    Some
      (Result.value
         (diagnostic (Lexing.from_string source) (fun () ->
            match view, Elaborate.lookup state.env source with
            | Schema, Some (Contract contract) ->
              Contract.describe contract;
              state
            | (Schema | Contract), Some (Table_value { table; _ }) ->
              if view = Schema
              then print_schema state table
              else print_endline (Elaborate.origin state.env table);
              state
            | _ ->
              let t = table state source in
              (match view with
               | Schema ->
                 print_schema state t;
                 state
               | Contract ->
                 print_endline (Elaborate.origin state.env t);
                 state
               | Validate ->
                 List.iter Contract.validate (List.sort_uniq compare (Core.contracts t));
                 print_endline "The data matches its contract";
                 state
               | Explain ->
                 List.iteri
                   (fun i s -> Printf.printf "%2d  %s\n" (i + 1) s)
                   (Elaborate.explain t);
                 state
               | Preview -> preview state t)))
         ~default:state)
  | Source "" -> Some state
  | Source source -> Some (evaluate state source)
;;

let run state command =
  try dispatch state command with
  | Sys.Break ->
    prerr_endline "Cancelled";
    Some state
  | exn ->
    report_diagnostic (Diagnostic.of_exn exn);
    Some state
;;

let open_session ~project ?path () =
  let session = empty project in
  match path with
  | None -> Some session
  | Some path ->
    let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
    Lexing.set_filename buf path;
    let directory = Filename.dirname path in
    diagnostic buf (fun () ->
      submit
        ~announce:false
        { session with
          directory
        ; source_path = Some path
        ; env = fresh_env ~directory project
        }
        (Frontend.parse buf))
    |> Result.to_option
;;

let incomplete source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | _ -> false
  | exception Parser.Error -> Lexing.lexeme buf = ""
  | exception Diagnostic.Error _ -> false
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
