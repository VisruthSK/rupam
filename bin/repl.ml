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
  ; ":preview TABLE", "first rows, after contract validation"
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
  { scalar : Infer.env
  ; history : Syntax.declaration list
  ; tables : Analysis_program.environment
  ; lexical : Check.env
  ; directory : string
  ; project : Project.t
  ; source_path : string option
  ; definitions : Syntax.declaration list
  ; show : digits:int -> Page.t -> unit
  }

let empty =
  { scalar = Infer.empty
  ; history = []
  ; tables = []
  ; lexical = []
  ; directory = "."
  ; project = Project.standalone
  ; source_path = None
  ; definitions = []
  ; show = (fun ~digits page -> List.iter print_endline (Page.plain ~digits page))
  }
;;

let dataset state x =
  Analysis_program.dataset ~directory:state.directory state.tables state.lexical x
;;

let preview state d =
  let dialect = Project.require_dialect state.project in
  let runtime = Runtime.resolve state.project (Runtime.of_dialect dialect) in
  state.show
    ~digits:state.project.preview_digits.value
    (match dialect with
     | dialect -> Dialect.preview dialect ~runtime d ~offset:0 ~limit:20)
;;

let compute state d =
  let dialect = Project.require_dialect state.project in
  let language = Runtime.of_dialect dialect in
  let script =
    match dialect with
    | dialect -> Dialect.value dialect (state.definitions @ [ d ])
  in
  String.trim (Worker.evaluate (Runtime.resolve state.project language) language script)
;;

let echo ?(announce = true) state d =
  let scalar, t = Infer.declaration state.scalar d in
  let state = { state with scalar } in
  match d with
  | Syntax.Evaluate _ ->
    if announce then Printf.printf "%s : %s\n" (compute state d) (Infer.display t);
    state
  | Let (name, _, _) ->
    if announce then Printf.printf "%s : %s\n" name (Infer.display t);
    { state with definitions = state.definitions @ [ d ] }
  | Type _ | Alias _ -> { state with definitions = state.definitions @ [ d ] }
  | Import _ | Contract _ | Setting _ -> state
;;

let extend state ds =
  let tables, lexical, _ =
    Analysis_program.extend
      ~strict:state.project.strict.value
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
  | Syntax.Contract _ | Import _ | Setting _ -> true
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
  | Syntax.Evaluate { form = Field ({ form = Name (Bare, owner); _ }, name); _ }
    when match List.assoc_opt owner state.tables with
         | Some (Analysis_program.Dictionary contract) ->
           List.exists (fun (t : Contract.table) -> t.name = name) contract.tables
         | _ -> false ->
    (match List.assoc owner state.tables with
     | Analysis_program.Dictionary contract ->
       Contract.describe_table
         (List.find (fun (t : Contract.table) -> t.name = name) contract.tables)
     | _ -> ());
    state
  | Syntax.Evaluate { form = Name (Bare, name); _ } when observe state name -> state
  | Syntax.Let (name, Some params, _) ->
    let state = extend state [ d ] in
    (match echo ~announce state d with
     | state -> state
     | exception Diagnostic.Error _ ->
       if announce then Printf.printf "%s : fn(%s)\n" name (String.concat ", " params);
       state)
  | d when Option.is_some (Analysis_program.printed d) ->
    let x = Option.get (Analysis_program.printed d) in
    if tabular state (Syntax.Evaluate x)
    then (
      preview state (dataset state x);
      state)
    else echo ~announce state (Syntax.Evaluate x)
  | Evaluate x when tabular state d && not (writes d) ->
    preview state (dataset state x);
    state
  | d when tabular state d -> extend state [ d ]
  | Let (_, None, { form = Literal _; _ }) -> echo ~announce (extend state [ d ]) d
  | d -> echo ~announce state d
;;

let submit ?(announce = true) state ds = List.fold_left (declare ~announce) state ds

let user_values state =
  let builtins = List.length Infer.empty.values in
  List.filteri
    (fun i _ -> i < List.length state.scalar.values - builtins)
    state.scalar.values
;;

type suggestion =
  { text : string
  ; detail : string
  }

let verbs =
  [ "filter", "condition, missing = skip | propagate | error", [ "missing" ]
  ; "select", "columns to keep, in order", []
  ; "mutate", "name = value, ..., by = key", [ "by" ]
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
  | "missing" | "group_missing" -> [ "skip"; "propagate"; "error" ]
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

let ends_with_pipe s =
  let s = trim_right s in
  String.ends_with ~suffix:"|>" s
;;

let piped state line pipe =
  let start = expression_start line (pipe - 1) in
  let source = String.trim (String.sub line start (pipe - start)) in
  match Frontend.parse (Lexing.from_string source) with
  | [ Syntax.Evaluate x ] ->
    (try Some (dataset state x) with
     | _ -> None)
  | _ -> None
  | exception _ -> None
;;

let columns (d : Analysis_program.dataset) =
  List.map
    (fun n -> { text = n; detail = Shape.type_name (List.assoc n d.plan.schema).typ })
    (Table.names d.plan)
;;

let functions state =
  List.map
    (fun (n, b) -> { text = n; detail = Infer.display (snd (Infer.signature b)) })
    Prelude.all
  @ List.filter_map
      (fun (n, b) ->
         match b with
         | Analysis_program.Function (ps, _, _, _) ->
           Some { text = n; detail = "fn(" ^ String.concat ", " ps ^ ")" }
         | _ -> None)
      state.tables
;;

let values state =
  List.filter_map
    (fun (n, b) ->
       match b with
       | Analysis_program.Dataset _ -> Some { text = n; detail = "table" }
       | Dictionary _ -> Some { text = n; detail = "contract" }
       | Function _ -> None)
    state.tables
  @ List.map
      (fun (n, (_, t)) -> { text = n; detail = Infer.display t })
      (user_values state)
;;

let tables state =
  List.filter_map
    (fun (n, b) ->
       match b with
       | Analysis_program.Dataset _ -> Some { text = n; detail = "table" }
       | Dictionary _ -> Some { text = n; detail = "contract" }
       | Function _ -> None)
    state.tables
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
      match List.assoc_opt owner state.tables with
      | Some (Analysis_program.Dictionary contract) ->
        ( List.map
            (fun (t : Contract.table) -> { text = t.name; detail = "table" })
            contract.tables
        , start
        , word )
      | _ -> [], start, word)
    else if ends_with_pipe (String.sub line 0 start)
    then
      ( List.map (fun (v, d, _) -> { text = v; detail = d }) verbs
        @ List.filter
            (fun s -> String.starts_with ~prefix:"fn" s.detail)
            (functions state)
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

let table state source =
  match Frontend.parse (Lexing.from_string source) with
  | [ Syntax.Evaluate x ] -> dataset state x
  | _ -> Diagnostic.fail Not_a_table "Expected a table expression"
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
    { state with
      history = []
    ; tables = []
    ; lexical = []
    ; scalar = Infer.empty
    ; definitions = []
    }
    history
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
    Some
      { state with
        project =
          { state.project with
            dialect = { value = Some dialect; origin = Session ":target" }
          }
      }
  | Lock ->
    (match state.source_path with
     | None -> report "No file loaded. Start the REPL with a program to lock it."
     | Some source ->
       ignore
         (diagnostic (Lexing.from_string "") (fun () ->
            let entry = Lock.lock ~project:state.project ~source state.history in
            List.iter
              (fun (file, columns) ->
                 print_endline file;
                 List.iter (fun (n, t) -> Printf.printf "  %s  %s\n" n t) columns)
              entry.schemas;
            Printf.printf "Locked %s for %s\n" entry.source entry.dialect)));
    Some state
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
      (List.rev (user_values state));
    Some state
  | Type source ->
    let buf = Lexing.from_string source in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ (Syntax.Evaluate x as d) ] when tabular state d ->
           Analysis_program.schema (dataset state x)
         | [ Syntax.Evaluate x ] ->
           print_endline (Infer.display (Infer.expression state.scalar 1 x))
         | _ -> Diagnostic.fail Wrong_arguments "Expected one expression"));
    Some state
  | Code (chosen, source) ->
    ignore
      (diagnostic (Lexing.from_string source) (fun () ->
         let d = table state source in
         print_endline
           (match
              Option.value chosen ~default:(Project.require_dialect state.project)
            with
            | dialect -> Dialect.table dialect d)));
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
         | Contract -> print_endline (Analysis_program.origin d)
         | Validate -> Analysis_program.validate d
         | Explain -> Analysis_program.explain d
         | Preview -> preview state d));
    Some state
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
