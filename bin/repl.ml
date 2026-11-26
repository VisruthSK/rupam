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

let diagnostic f =
  try Ok (f ()) with
  | Sys.Break ->
    prerr_endline "Cancelled";
    Error ()
  | exn ->
    report_diagnostic (Diagnostic.of_exn exn);
    Error ()
;;

type t =
  { session : Session.t
  ; directory : string
  ; show : digits:int -> Page.t -> unit
  }

let digits r = (Session.project r.session).preview_digits.value

let print_schema r (t : TransformIR.table) =
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
  | [ (d : Syntax.item) ] when observe r d.declaration -> ()
  | _ -> List.iter (present r) (Session.submit r.session declarations)
;;

type suggestion = Completion.suggestion =
  { text : string
  ; detail : string
  }

let command_candidates line =
  let words = String.split_on_char ' ' line |> List.filter (( <> ) "") in
  let fresh = String.ends_with ~suffix:" " line in
  let named kind names = List.map (fun (text, _) -> { text; detail = kind }) names in
  match words, fresh with
  | ([] | [ _ ]), false ->
    List.map
      (fun (usage, text) ->
         { text = List.hd (String.split_on_char ' ' usage); detail = text })
      help
  | [ ":target" ], true | [ ":target"; _ ], false -> named "target" Target.targets
  | [ ":language" ], true | [ ":language"; _ ], false -> named "language" Target.languages
  | [ ":dialect" ], true | [ ":dialect"; _ ], false -> named "dialect" Target.dialects
  | [ ":code" ], true | [ ":code"; _ ], false -> named "target" Target.targets
  | _ -> []
;;

let complete r line =
  if not (String.starts_with ~prefix:":" (String.trim line))
  then Completion.complete r.session line
  else (
    let start = Completion.word_start line in
    let word = String.sub line start (String.length line - start) in
    let commands = String.contains (String.trim line) ' ' in
    let candidates =
      if commands
      then command_candidates line @ Completion.tables r.session
      else command_candidates line
    in
    let start, word = if commands then start, word else start - 1, ":" ^ word in
    ( start
    , List.filter
        (fun c -> String.starts_with ~prefix:word c.text && c.text <> word)
        candidates ))
;;

let bindings r = Completion.bindings r.session
let guard f = ignore (diagnostic f)
let evaluate r source = guard (fun () -> submit r (Frontend.read source))

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
    guard (fun () ->
      match Frontend.read source with
      | [ ({ declaration = Evaluate _; _ } as d) ] ->
        (match Session.check r.session d with
         | [ Show { node = Table t; _ } ] -> print_schema r t
         | [ Show e ] -> print_endline (Types.display e.typ)
         | _ -> ())
      | _ -> Diagnostic.fail Wrong_arguments "Expected one expression");
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
           List.iter Contract.validate (List.sort_uniq compare (TransformIR.contracts t));
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
    diagnostic (fun () ->
      Session.load session path;
      r)
    |> Result.to_option
;;

let incomplete = Frontend.incomplete

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
