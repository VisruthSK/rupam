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
  | Version
  | Lock
  | Save of string option
  | Undo
  | Redo
  | Bindings
  | Target of Target.t option
  | Language of Target.language
  | Dialect of Target.dialect
  | Type of string
  | View of view * string
  | Code of Target.t option * string
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
  ; ":target [TARGET]", "show or set the target: r-dplyr or python-polars"
  ; ":language LANGUAGE", "switch the host language: r or python"
  ; ":dialect DIALECT", "switch the dialect: dplyr or polars"
  ; ":options", "effective settings and their sources"
  ; ":bindings", "everything defined in this session"
  ; ":save [FILE]", "write the accepted analysis as a .rp file"
  ; ":undo", "go back one revision"
  ; ":redo", "go forward one revision"
  ; ":lock", "validate the saved analysis's inputs and record them in rupam.lock"
  ; ":version", "build identity"
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
  let lookup kind names name =
    match List.assoc_opt name names with
    | Some x -> Ok x
    | None ->
      Error
        (Printf.sprintf
           "Unknown %s %s. Expected %s."
           kind
           name
           (String.concat " or " (List.map fst names)))
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
    | "options" when rest = "" -> Ok Options
    | "version" -> Ok Version
    | "lock" -> Ok Lock
    | "save" -> Ok (Save (if rest = "" then None else Some rest))
    | "undo" -> Ok Undo
    | "redo" -> Ok Redo
    | "language" ->
      Result.map (fun l -> Language l) (lookup "language" Target.languages rest)
    | "dialect" -> Result.map (fun d -> Dialect d) (lookup "dialect" Target.dialects rest)
    | "bindings" -> Ok Bindings
    | "target" when rest = "" -> Ok (Target None)
    | "target" ->
      Result.map (fun t -> Target (Some t)) (lookup "target" Target.targets rest)
    | "type" | "t" -> needs "an expression" (fun x -> Type x)
    | "schema" when rest = "" -> Ok (View (Schema, ""))
    | "code" ->
      let first, after =
        match String.index_opt rest ' ' with
        | Some i ->
          String.sub rest 0 i, String.trim (String.sub rest i (String.length rest - i))
        | None -> rest, ""
      in
      (match List.assoc_opt first Target.targets with
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

type t =
  { session : Session.t
  ; directory : string
  ; show : digits:int -> Page.t -> unit
  }

let digits r = (Session.project r.session).preview_digits.value

let print_schema r (t : Core.table) =
  let width = List.fold_left (fun w (n, _) -> max w (String.length n)) 0 t.schema in
  List.iter
    (fun (n, typ) -> Printf.printf "%-*s  %s\n" width n (Types.display typ))
    t.schema;
  Printf.printf
    "%d columns from %s\n"
    (List.length t.schema)
    (Scope.origin (Session.env r.session) t)
;;

let observe r (d : Syntax.declaration) =
  let env = Session.env r.session in
  match d with
  | Evaluate { form = Name (Bare, name); _ } ->
    (match Scope.lookup env name with
     | Some (Contract contract) ->
       Contract.describe contract;
       true
     | Some (Function _ as f) ->
       Option.iter (fun t -> Printf.printf "%s : %s\n" name t) (Scope.describe f);
       true
     | _ -> false)
  | Evaluate { form = Field ({ form = Name (Bare, owner); _ }, name); _ } ->
    (match Scope.lookup env owner with
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

let present r (event : Session.event) =
  match event with
  | Bound (name, description) -> Printf.printf "%s : %s\n" name description
  | Output text -> print_string text
  | Value (text, typ) -> Printf.printf "%s : %s\n" text (Types.display typ)
  | Table page -> r.show ~digits:(digits r) page
;;

let submit r declarations =
  match declarations with
  | [ d ] when observe r d -> ()
  | _ -> List.iter (present r) (Session.submit r.session declarations)
;;

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

let piped state line pipe =
  let start = expression_start line (pipe - 1) in
  match
    Session.table state.session (String.trim (String.sub line start (pipe - start)))
  with
  | t -> Some t
  | exception _ -> None
;;

let columns (t : Core.table) =
  List.map (fun (n, typ) -> { text = n; detail = Types.display typ }) t.schema
;;

let bindings state =
  List.map
    (fun (n, (b : Scope.binding), detail) ->
       let detail =
         match b with
         | Table_value _ -> "table"
         | _ -> detail
       in
       n, b, { text = n; detail })
    (Scope.bindings (Session.env state.session))
;;

let functions state =
  List.map (fun (text, detail) -> { text; detail }) (Elaborate.builtins ())
  @ List.filter_map
      (fun (_, (b : Scope.binding), s) ->
         match b with
         | Function _ -> Some s
         | _ -> None)
      (bindings state)
;;

let values state =
  List.filter_map
    (fun (_, (b : Scope.binding), s) ->
       match b with
       | Function _ -> None
       | _ -> Some s)
    (bindings state)
;;

let tables state =
  List.filter_map
    (fun (_, (b : Scope.binding), s) ->
       match b with
       | Table_value _ | Contract _ -> Some s
       | _ -> None)
    (bindings state)
;;

let targets = List.map (fun (n, _) -> { text = n; detail = "target" }) Target.targets

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
  | [ ":target" ], true | [ ":target"; _ ], false -> `Targets
  | [ ":language" ], true | [ ":language"; _ ], false -> `Languages
  | [ ":dialect" ], true | [ ":dialect"; _ ], false -> `Dialects
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
      | `Targets -> targets, start, word
      | `Languages ->
        ( List.map (fun (n, _) -> { text = n; detail = "language" }) Target.languages
        , start
        , word )
      | `Dialects ->
        ( List.map (fun (n, _) -> { text = n; detail = "dialect" }) Target.dialects
        , start
        , word )
      | `Code -> targets @ tables state, start, word
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
      match Scope.lookup (Session.env state.session) owner with
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

let guard f = ignore (diagnostic (Lexing.from_string "") f)

let evaluate r source =
  let buf = Lexing.from_string source in
  ignore (diagnostic buf (fun () -> submit r (Frontend.parse buf)))
;;

let schemas r =
  List.iter
    (fun (name, (b : Scope.binding), _) ->
       match b with
       | Table_value { table; _ } ->
         print_endline name;
         print_schema r table
       | Contract contract -> Contract.describe contract
       | _ -> ())
    (bindings r)
;;

let retarget r target =
  Session.set_target r.session target ~origin:":target";
  print_endline (Target.name target)
;;

let dispatch r = function
  | Quit -> false
  | Help ->
    List.iter (fun (usage, text) -> Printf.printf "  %-24s %s\n" usage text) help;
    true
  | Options ->
    List.iter
      (fun (name, value, source) -> Printf.printf "%-16s %-14s %s\n" name value source)
      (Project.options (Session.project r.session));
    true
  | Version ->
    print_endline (Version.describe ());
    true
  | Target None ->
    print_endline
      (match Session.target r.session with
       | Some t -> Target.name t
       | None ->
         "No target yet. Choose one with :target r-dplyr or :target python-polars.");
    true
  | Target (Some target) ->
    retarget r target;
    true
  | Language language ->
    (match Target.dialects_of language with
     | [ d ] -> retarget r (Option.get (Target.make language d))
     | _ -> report "Choose a dialect for this language with :target");
    true
  | Dialect dialect ->
    retarget r (Option.get (Target.make (Target.language_of dialect) dialect));
    true
  | Save file ->
    (match file, Session.path r.session with
     | Some path, _ | None, Some path ->
       Session.save r.session path;
       Printf.printf "Saved %s\n" path
     | None, None -> report ":save needs a file name the first time");
    true
  | Undo ->
    if not (Session.undo r.session) then report "Nothing to undo";
    true
  | Redo ->
    if not (Session.redo r.session) then report "Nothing to redo";
    true
  | Lock ->
    guard (fun () ->
      let entry = Session.lock r.session in
      Lock.print_schemas entry;
      Printf.printf "Locked %s for %s\n" entry.source (Target.name entry.target));
    true
  | Bindings ->
    List.iter (fun (name, _, s) -> Printf.printf "%s : %s\n" name s.detail) (bindings r);
    true
  | Type source ->
    let buf = Lexing.from_string source in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ (Syntax.Evaluate _ as d) ] ->
           (match Session.check r.session d with
            | [ Show { node = Table t; _ } ] -> print_schema r t
            | [ Show e ] -> print_endline (Types.display e.typ)
            | _ -> ())
         | _ -> Diagnostic.fail Wrong_arguments "Expected one expression"));
    true
  | Code (chosen, source) ->
    guard (fun () ->
      print_endline (Session.code r.session chosen (Session.table r.session source)));
    true
  | View (Schema, "") ->
    schemas r;
    true
  | View (view, source) ->
    guard (fun () ->
      let env = Session.env r.session in
      match view, Scope.lookup env source with
      | Schema, Some (Contract contract) -> Contract.describe contract
      | Schema, Some (Table_value { table; _ }) -> print_schema r table
      | Contract, Some (Table_value { table; _ }) ->
        print_endline (Scope.origin env table)
      | _ ->
        let t = Session.table r.session source in
        (match view with
         | Schema -> print_schema r t
         | Contract -> print_endline (Scope.origin env t)
         | Validate ->
           List.iter Contract.validate (List.sort_uniq compare (Core.contracts t));
           print_endline "The data matches its contract"
         | Explain ->
           List.iteri (fun i s -> Printf.printf "%2d  %s\n" (i + 1) s) (Plan.explain t)
         | Preview ->
           r.show ~digits:(digits r) (Session.preview r.session t ~offset:0 ~limit:20)));
    true
  | Source "" -> true
  | Source source ->
    evaluate r source;
    true
;;

let run r command =
  try dispatch r command with
  | Sys.Break ->
    prerr_endline "Cancelled";
    true
  | exn ->
    report_diagnostic (Diagnostic.of_exn exn);
    true
;;

let plain ~digits page = List.iter print_endline (Page.plain ~digits page)

let open_session ~project ?path () =
  let directory = Option.fold ~none:"." ~some:Filename.dirname path in
  let session = Session.create ~project ~directory in
  let r = { session; directory; show = plain } in
  match path with
  | None -> Some r
  | Some path ->
    let buf = Lexing.from_string "" in
    Lexing.set_filename buf path;
    diagnostic buf (fun () ->
      Session.load session path;
      r)
    |> Result.to_option
;;

let incomplete source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | _ -> false
  | exception Parser.Error -> Lexing.lexeme buf = ""
  | exception Diagnostic.Error _ -> false
;;

let rec loop r pending =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    let entry = if pending = "" then line else pending ^ "\n" ^ line in
    if (not (String.starts_with ~prefix:":" (String.trim entry))) && incomplete entry
    then loop r entry
    else (
      match parse entry with
      | Error message ->
        report message;
        loop r ""
      | Ok command ->
        let continue = run r command in
        flush stdout;
        if continue then loop r "" else Session.close r.session)
;;

let start ~project ?path () =
  Sys.catch_break true;
  Option.iter (fun r -> loop r "") (open_session ~project ?path ())
;;
