open Rupam

type view =
  | Schema
  | Grid
  | Preview
  | Explain
  | Contract
  | Validate

type command =
  | Quit
  | Help of string option
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
  ; "view", Grid
  ; "explain", Explain
  ; "contract", Contract
  ; "validate", Validate
  ]
;;

let help =
  [ ":type EXPR", "inferred type, without running anything"
  ; ":schema [TABLE]", "columns and types; every table when no name is given"
  ; ":preview TABLE", "first rows"
  ; ":view TABLE", "open the full-screen viewer"
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
  ; ":help [NAME]", "commands and keys, or documentation for NAME (also ?NAME)"
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
  if String.starts_with ~prefix:"?" line
  then (
    let topic = String.trim (String.sub line 1 (String.length line - 1)) in
    Ok (Help (if topic = "" then None else Some topic)))
  else if not (String.starts_with ~prefix:":" line)
  then Ok (Source line)
  else (
    match String.sub verb 1 (String.length verb - 1) with
    | "quit" | "q" -> Ok Quit
    | "help" | "h" -> Ok (Help (if rest = "" then None else Some rest))
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
  ; view : Syntax.expr -> Session.result -> unit
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

let ticks ~count low high =
  let range = high -. low in
  if range <= 0.
  then [ low, Display.rounded ~digits:2 low ]
  else (
    let raw = range /. float_of_int (max 2 count) in
    let magnitude = 10. ** Float.floor (Float.log10 raw) in
    let step =
      magnitude
      *. List.find (fun m -> m *. magnitude >= raw) [ 1.; 2.; 2.5; 5.; 10. ]
    in
    let first = Float.ceil (low /. step) in
    let last = Float.floor (high /. step) in
    let decimals = max 0 (int_of_float (Float.ceil (-.Float.log10 step))) in
    List.init
      (int_of_float (last -. first) + 1)
      (fun i ->
         let tick = (first +. float_of_int i) *. step in
         tick, Printf.sprintf "%.*f" decimals tick))
;;

let histogram ~digits (s : Arrow_result.summary) =
  let h = s.histogram in
  let blocks =
    [| " "
     ; "\xe2\x96\x81"
     ; "\xe2\x96\x82"
     ; "\xe2\x96\x83"
     ; "\xe2\x96\x84"
     ; "\xe2\x96\x85"
     ; "\xe2\x96\x86"
     ; "\xe2\x96\x87"
     ; "\xe2\x96\x88"
    |]
  in
  let height = 8 in
  let bins = Array.length h.counts in
  let width = max 1 (56 / max 1 bins) in
  let most = Array.fold_left max 1 h.counts in
  let row level =
    String.concat
      ""
      (Array.to_list
         (Array.map
            (fun count ->
               let eighths = count * height * 8 / most in
               let cell = max 0 (min 8 (eighths - ((level - 1) * 8))) in
               String.concat "" (List.init width (fun _ -> blocks.(cell))))
            h.counts))
  in
  let span = bins * width in
  let axis = Bytes.make span ' ' in
  let place at label =
    let n = String.length label in
    let start = max 0 (min (span - n) (at - (n / 2))) in
    let lo = max 0 (start - 1) and hi = min span (start + n + 1) in
    if String.for_all (( = ) ' ') (Bytes.sub_string axis lo (hi - lo))
    then Bytes.blit_string label 0 axis start n
  in
  List.iter
    (fun (tick, label) ->
       let at =
         if h.high = h.low
         then 0
         else int_of_float ((tick -. h.low) /. (h.high -. h.low) *. float_of_int span)
       in
       place at label)
    (ticks ~count:(span / 8) h.low h.high);
  let number = Option.fold ~none:"" ~some:(Display.rounded ~digits) in
  let present = Array.fold_left ( + ) 0 h.counts in
  let stats =
    [ "n", string_of_int present ]
    @ (if h.missing > 0 then [ "None", string_of_int h.missing ] else [])
    @ [ "mean", number s.mean
      ; "sd", number s.sd
      ; "min", Display.rounded ~digits h.low
      ; "q1", number s.q1
      ; "median", number s.median
      ; "q3", number s.q3
      ; "max", Display.rounded ~digits h.high
      ]
  in
  let bars = List.init height (fun i -> row (height - i)) @ [ Bytes.to_string axis ] in
  List.mapi
    (fun i bar ->
       match List.nth_opt stats i with
       | Some (name, value) ->
         Printf.sprintf "%s%s   %-6s %s" bar (String.make (span - Display.width bar) ' ') name value
       | None -> bar)
    bars
;;

let bars column (f : Arrow_result.frequencies) =
  let rows = f.top @ if f.missing > 0 then [ "None", f.missing ] else [] in
  let most = List.fold_left (fun m (_, n) -> max m n) 1 rows in
  let label = List.fold_left (fun w (v, _) -> max w (Display.width v)) 0 rows in
  Printf.sprintf "%s  %d distinct" column f.distinct
  :: List.map
       (fun (value, count) ->
          let pad = String.make (label - Display.width value) ' ' in
          Printf.sprintf
            "%s%s  %s %d"
            value
            pad
            (String.concat
               ""
               (List.init (max 1 (count * 48 / most)) (fun _ -> "\xe2\x96\x88")))
            count)
       rows
;;

let present r (event : Session.event) =
  match event with
  | Bound (name, description) -> Printf.printf "%s : %s\n" name description
  | Output text -> print_string text
  | Value (text, typ) -> Printf.printf "%s : %s\n" text (Types.display typ)
  | Table page -> r.show ~digits:(digits r) page
  | Viewed (subject, result) -> r.view subject result
  | Inspection_failed d -> report_diagnostic d
  | Schema columns ->
    let width = List.fold_left (fun w (n, _) -> max w (String.length n)) 0 columns in
    List.iter (fun (n, t) -> Printf.printf "%-*s  %s\n" width n (Types.display t)) columns
  | Typed t -> print_endline (Types.display t)
  | Bars (column, f) -> List.iter print_endline (bars column f)
  | Histogram (_, s) -> List.iter print_endline (histogram ~digits:(digits r) s)
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
  let named names = List.map (fun (text, _) -> { text; detail = "" }) names in
  match words, fresh with
  | ([] | [ _ ]), false ->
    List.map
      (fun (usage, text) ->
         { text = List.hd (String.split_on_char ' ' usage); detail = text })
      help
  | [ ":target" ], true | [ ":target"; _ ], false -> named Target.targets
  | [ ":language" ], true | [ ":language"; _ ], false -> named Target.languages
  | [ ":dialect" ], true | [ ":dialect"; _ ], false -> named Target.dialects
  | [ ":code" ], true | [ ":code"; _ ], false -> named Target.targets
  | _ -> []
;;

let complete r line =
  if not (String.starts_with ~prefix:":" (String.trim line))
  then Completion.complete r.session line
  else (
    let colon = String.index line ':' in
    let argument = String.index_from_opt line colon ' ' in
    let start =
      match argument with
      | None -> colon
      | Some _ -> 1 + String.rindex line ' '
    in
    let word = String.sub line start (String.length line - start) in
    let candidates =
      match argument with
      | None -> command_candidates line
      | Some _ ->
        let command = List.hd (String.split_on_char ' ' (String.trim line)) in
        let takes_table =
          List.exists
            (fun (usage, _) ->
               match String.split_on_char ' ' usage with
               | name :: argument :: _ ->
                 name = command && Astring.String.is_infix ~affix:"TABLE" argument
               | _ -> false)
            help
        in
        command_candidates line @ if takes_table then Completion.tables r.session else []
    in
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

let usage command =
  List.find_opt
    (fun (usage, _) -> List.hd (String.split_on_char ' ' usage) = command)
    help
;;

let hint line =
  let line = String.trim line in
  match String.split_on_char ' ' line with
  | [ command ] when String.starts_with ~prefix:":" command ->
    Option.map (fun (usage, text) -> usage ^ "  " ^ text) (usage command)
  | _ -> None
;;

let missing_argument line =
  match String.split_on_char ' ' (String.trim line) with
  | [ command ] ->
    (match usage command with
     | Some (usage, _) ->
       (match String.split_on_char ' ' usage with
        | _ :: argument :: _ -> not (String.starts_with ~prefix:"[" argument)
        | _ -> false)
     | None -> false)
  | _ -> false
;;

let describe_topic r topic =
  let env = Session.env r.session in
  match Scope.lookup env topic with
  | Some (Table_value { table; _ }) -> print_schema r table
  | Some (Contract contract) -> Contract.describe contract
  | Some binding ->
    Option.iter (fun t -> Printf.printf "%s : %s\n" topic t) (Scope.describe binding)
  | None ->
    (match
       ( List.assoc_opt topic (Elaborate.builtins ())
       , Completion.verb topic
       , usage (if String.starts_with ~prefix:":" topic then topic else ":" ^ topic) )
     with
     | Some typ, _, _ -> Printf.printf "%s : %s\n" topic typ
     | None, Some signature, _ -> Printf.printf "%s(%s)\n" topic signature
     | None, None, Some (usage, text) -> Printf.printf "%s  %s\n" usage text
     | None, None, None -> report ("No documentation for " ^ topic))
;;

let dispatch r = function
  | Quit -> false
  | Help None ->
    List.iter (fun (usage, text) -> Printf.printf "  %-24s %s\n" usage text) help;
    true
  | Help (Some topic) ->
    describe_topic r topic;
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
           r.show ~digits:(digits r) (Session.preview r.session t ~offset:0 ~limit:20)
         | Grid ->
           (match Frontend.read source with
            | [ { declaration = Evaluate e; _ } ] -> r.view e (Session.result r.session t)
            | _ -> Diagnostic.fail Not_a_table "Expected a table expression")));
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
  let r =
    { session
    ; directory
    ; show = plain
    ; view = (fun _ result -> plain ~digits:2 (Session.page result ~offset:0 ~limit:20))
    }
  in
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
