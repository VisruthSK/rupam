open Rupam
open Rupam_native
open Cmdliner

let source path =
  let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
  Lexing.set_filename buf path;
  buf
;;

let span buf =
  Sexplib0.Sexp.to_string (Syntax.sexp_of_span (buf.Lexing.lex_start_p, buf.lex_curr_p))
;;

let token_lines buf =
  let rec loop acc =
    match Lexer.token buf with
    | Parser.EOF -> List.rev acc
    | _ -> loop (Printf.sprintf "%s  %s" (span buf) (Lexing.lexeme buf) :: acc)
  in
  loop []
;;

let tokens path = List.iter print_endline (token_lines (source path))
let sexp = Sexplib0.Sexp.to_string_hum
let syntax_lines items = List.map (fun item -> sexp (Syntax.sexp_of_item item)) items
let syntax path = List.iter print_endline (syntax_lines (Frontend.parse (source path)))

let elaborate_buffer directory buf =
  let project = Project.load directory in
  snd
    (Elaborate.program
       (Scope.empty ~inputs:Native.inputs ~directory ~strict:project.strict.value ())
       (Frontend.parse buf))
;;

let elaborate path = elaborate_buffer (Filename.dirname path) (source path)
let ir_lines items = List.map (fun item -> sexp (TransformIR.sexp_of_item item)) items

let transform_ir binding path =
  let items = elaborate path in
  let items =
    match binding with
    | None -> items
    | Some name ->
      (match
         List.find_opt
           (function
             | TransformIR.Define (id, _) -> id.name = name
             | _ -> false)
           (List.rev items)
       with
       | Some item -> [ item ]
       | None -> Diagnostic.fail Unknown_name ("No emitted binding named " ^ name))
  in
  List.iter print_endline (ir_lines items)
;;

let typed_lines items =
  List.map
    (function
      | TransformIR.Define (id, e) ->
        Printf.sprintf "%s : %s" id.name (Types.display e.typ)
      | Show e -> Printf.sprintf "show : %s" (Types.display e.typ)
      | Write (e, file) -> Printf.sprintf "write %s : %s" file (Types.display e.typ)
      | Variant (name, tags) ->
        Printf.sprintf "type %s = %s" name (String.concat " | " (List.map fst tags)))
    items
;;

let typed path = List.iter print_endline (typed_lines (elaborate path))

let generated dialect items =
  let module B = (val Backend.of_target dialect) in
  let text, locations = B.script items in
  let locations =
    List.map
      (fun (l : Document.location) ->
         Printf.sprintf
           "%d:%d <- %s"
           l.line
           l.column
           (Sexplib0.Sexp.to_string (Syntax.sexp_of_span l.source)))
      locations
  in
  String.concat "\n" (text :: "# source map" :: locations)
;;

let code dialect path = print_endline (generated dialect (elaborate path))

let mcp () =
  let open Dev_mcp in
  let directory = Sys.getcwd () in
  let get = List.assoc in
  let buffer args =
    let buf = Lexing.from_string (get "source" args) in
    Lexing.set_filename buf "<mcp>";
    buf
  in
  let items args = elaborate_buffer directory (buffer args) in
  let lines xs = `String (String.concat "\n" xs) in
  let source_tool name description call =
    { name; description; parameters = [ Text "source" ]; call }
  in
  let docs = Filename.concat directory "docs/spec" in
  let topics =
    Sys.readdir docs
    |> Array.to_list
    |> List.filter_map (fun file ->
      if Filename.check_suffix file ".md"
      then Some (Filename.chop_suffix file ".md")
      else None)
    |> List.sort String.compare
  in
  run
    [ source_tool "check" "Check a Rupam program." (fun args ->
        `List (List.filter_map Protocol.item (items args)))
    ; source_tool "tokens" "Inspect the lexer tokens and spans." (fun args ->
        lines (token_lines (buffer args)))
    ; source_tool "syntax" "Inspect the parsed Rupam syntax tree." (fun args ->
        lines (syntax_lines (Frontend.parse (buffer args))))
    ; source_tool "typed" "Inspect inferred types and schemas." (fun args ->
        lines (typed_lines (items args)))
    ; source_tool
        "transform_ir"
        "Inspect the checked intermediate representation."
        (fun args -> lines (ir_lines (items args)))
    ; { name = "code"
      ; description = "Generate native source and source map."
      ; parameters = [ Text "source"; Choice ("target", List.map fst Target.targets) ]
      ; call =
          (fun args ->
            `String
              (generated (List.assoc (get "target" args) Target.targets) (items args)))
      }
    ; source_tool
        "evaluate_reference"
        "Evaluate using the Rupam reference evaluator."
        (fun args ->
           let checked = items args in
           let env, shown = Reference.run [] checked in
           let value (e : TransformIR.expr) v =
             `Assoc
               [ "type", `String (Types.display e.typ)
               ; "value", `String (Reference.display v)
               ]
           in
           let bindings =
             List.filter_map
               (function
                 | TransformIR.Define (id, e) ->
                   Option.map (fun v -> id.name, value e v) (List.assoc_opt id.stamp env)
                 | _ -> None)
               checked
           in
           `Assoc
             [ "bindings", `Assoc bindings
             ; "shown", `List (List.map (fun (e, v) -> value e v) shown)
             ])
    ; { name = "conformance"
      ; description = "Run a recorded reference conformance fixture."
      ; parameters = [ Text "id" ]
      ; call =
          (fun args ->
            let id = get "id" args in
            let module F = Rupam_fixture.Fixture in
            let fixture =
              match
                List.find_opt
                  (fun (f : F.t) -> f.id = id)
                  (F.all (Filename.concat directory "test/conformance"))
              with
              | Some f -> f
              | None -> Diagnostic.fail Unknown_name ("No conformance fixture named " ^ id)
            in
            let status, problems =
              match F.check fixture with
              | Passed -> "passed", []
              | Failed errors -> "failed", errors
              | Unspecified -> "unspecified", []
            in
            `Assoc
              [ "id", `String id
              ; "status", `String status
              ; "problems", `List (List.map (fun s -> `String s) problems)
              ])
      }
    ; { name = "describe"
      ; description = "Read one section of the Rupam specification."
      ; parameters = [ Choice ("topic", topics) ]
      ; call =
          (fun args ->
            `String
              (In_channel.with_open_text
                 (Filename.concat docs (get "topic" args ^ ".md"))
                 In_channel.input_all))
      }
    ]
    stdin
    stdout
;;

let bench examples =
  let time f =
    let start = Unix.gettimeofday () in
    ignore (f ());
    (Unix.gettimeofday () -. start) *. 1000.
  in
  let percentile xs p =
    let a = Array.of_list (List.sort compare xs) in
    a.(min (Array.length a - 1) (int_of_float (p *. float_of_int (Array.length a))))
  in
  let startup =
    List.init 10 (fun _ -> time (fun () -> Sys.command "rupam --version > /dev/null"))
  in
  Printf.printf "%-28s p50 %7.1f ms\n%!" "rupam --version" (percentile startup 0.5);
  List.iter
    (fun name ->
       let session = Session.create ~project:Project.standalone ~directory:examples in
       Session.set_target session (List.assoc name Target.targets) ~origin:"bench";
       let submit source = Session.submit session (Frontend.read source) in
       let report label ms = Printf.printf "%-28s %7.1f ms\n%!" (name ^ " " ^ label) ms in
       report "first command, immediate" (time (fun () -> submit "1 + 1"));
       Session.close session;
       let session = Session.create ~project:Project.standalone ~directory:examples in
       Session.set_target session (List.assoc name Target.targets) ~origin:"bench";
       let submit source = Session.submit session (Frontend.read source) in
       Unix.sleepf 1.0;
       report "first command, after 1 s" (time (fun () -> submit "1 + 1"));
       let warm = List.init 30 (fun _ -> time (fun () -> submit "1 + 1")) in
       Printf.printf
         "%-28s p50 %7.1f ms  p95 %7.1f ms\n%!"
         (name ^ " round trip")
         (percentile warm 0.5)
         (percentile warm 0.95);
       report
         "read_csv penguins"
         (time (fun () -> submit {|let p = read_csv("penguins.csv")|}));
       report "show table" (time (fun () -> submit "p"));
       let trips = Filename.concat examples "trips.parquet" in
       if Sys.file_exists trips
       then (
         report
           "read_parquet trips"
           (time (fun () -> submit {|let trips = read_parquet("trips.parquet")|}));
         let opened = ref None in
         report
           "open trips"
           (time (fun () ->
              opened := Some (Session.result session (Session.table session "trips"))));
         let r = Option.get !opened in
         report "first page" (time (fun () -> Session.page r ~offset:0 ~limit:40));
         report "summary fare" (time (fun () -> Session.column_summary r "fare"));
         report "summary city" (time (fun () -> Session.column_summary r "city"));
         let draft = Session.open_draft session in
         let sorted = ref None in
         report
           "sort fare (draft)"
           (time (fun () ->
              sorted := Some (Session.preview_draft draft "trips |> arrange(fare)")));
         report
           "sorted first page"
           (time (fun () -> Session.page (Option.get !sorted) ~offset:0 ~limit:40));
         Session.close_draft draft);
       Session.close session)
    Rupam_fixture.Fixture.targets
;;

let eval source =
  let reference = Rupam_fixture.Fixture.evaluate source in
  let show (o : Rupam_fixture.Fixture.outcome) =
    match o.value, o.diagnostic with
    | Some v, _ -> v
    | None, Some d -> "diagnostic " ^ d
    | None, None -> "nothing"
  in
  Printf.printf "reference\n%s\n" (show reference);
  List.iter
    (fun name ->
       let session = Session.create ~project:Project.standalone ~directory:"." in
       Session.set_target session (List.assoc name Target.targets) ~origin:"eval";
       let actual = Rupam_fixture.Fixture.on_session session source in
       Session.close session;
       if actual.value <> reference.value || actual.diagnostic <> reference.diagnostic
       then Printf.printf "%s disagrees\n%s\n" name (show actual))
    Rupam_fixture.Fixture.targets
;;

let activate id =
  let path = Printf.sprintf "test/conformance/%s.toml" id in
  match
    List.find_opt
      (fun (f : Rupam_fixture.Fixture.t) -> f.id = id)
      (Rupam_fixture.Fixture.all "test/conformance")
  with
  | None -> failwith ("No fixture " ^ id)
  | Some f ->
    let failures =
      (match Rupam_fixture.Fixture.check f with
       | Failed p -> [ "reference: " ^ String.concat "; " p ]
       | Passed | Unspecified -> [])
      @ List.concat_map
          (fun name ->
             match Rupam_fixture.Fixture.on_target name f with
             | Failed p -> [ name ^ ": " ^ String.concat "; " p ]
             | Passed | Unspecified -> [])
          Rupam_fixture.Fixture.targets
    in
    if failures <> []
    then (
      List.iter prerr_endline failures;
      exit 1);
    let text = In_channel.with_open_text path In_channel.input_all in
    let active =
      Astring.String.cuts ~sep:{|status = "pending"|} text
      |> String.concat {|status = "active"|}
    in
    Out_channel.with_open_text path (fun oc -> output_string oc active);
    Printf.printf "%s passes everywhere and is active\n" id
;;

let fixture id path =
  let outcome =
    Rupam_fixture.Fixture.capture
      ~directory:"test/conformance"
      ~id
      (In_channel.with_open_text path In_channel.input_all)
  in
  let show name = Option.iter (Printf.printf "%s  %s\n" name) in
  Printf.printf "test/conformance/%s.toml\n" id;
  show "type" outcome.typ;
  show "value" outcome.value;
  show "diagnostic" outcome.diagnostic
;;

let guarded f path =
  match f path with
  | () -> Cmd.Exit.ok
  | exception exn ->
    prerr_endline (Diagnostic.render (Diagnostic.of_exn exn));
    Cmd.Exit.some_error
;;

let file = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE")

let show path keys =
  let typed (c : Input.column) =
    c.name
    ^ ": "
    ^
    match c.typ with
    | Supported t -> Types.display (if c.nullable then Types.optional t else t)
    | Unsupported raw -> raw
  in
  print_endline
    (String.concat
       ", "
       (List.map typed (Data_file.read ~missing:Input.default_missing path).columns));
  let _, rows = Duckdb_oracle.rows ~keys path in
  Array.iter
    (fun row ->
       print_endline
         (String.concat
            " | "
            (Array.to_list (Array.map (Option.value ~default:"None") row))))
    rows
;;

let frame () =
  In_channel.input_lines stdin
  |> List.iter (fun line ->
    Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length line) line)
;;

let unframe () =
  let rec go () =
    match In_channel.input_line stdin with
    | None -> ()
    | Some header when String.trim header = "" -> go ()
    | Some header ->
      let size =
        int_of_string (String.trim (List.nth (String.split_on_char ':' header) 1))
      in
      ignore (In_channel.input_line stdin);
      let body = really_input_string stdin size in
      print_endline
        (Yojson.Safe.to_string (Yojson.Safe.sort (Yojson.Safe.from_string body)));
      go ()
  in
  go ()
;;

let stage name doc f = Cmd.v (Cmd.info name ~doc) Term.(const (guarded f) $ file)

let binding =
  Arg.(
    value
    & opt (some string) None
    & info
        [ "binding" ]
        ~docv:"NAME"
        ~doc:"Dump the latest emitted definition of this binding.")
;;

let dialect =
  Arg.(
    value
    & opt (enum Target.targets) Target.r_dplyr
    & info [ "t"; "target" ] ~docv:"TARGET")
;;

let () =
  let argv = Array.map (fun arg -> if arg = "-h" then "--help" else arg) Sys.argv in
  exit
    (Cmd.eval'
       ~argv
       (Cmd.group
          (Cmd.info "rupam-dev" ~doc:"Internal tools for developing Rupam.")
          [ Cmd.v
              (Cmd.info
                 "show"
                 ~doc:"Print a Parquet file's schema and rows, sorted by KEYs.")
              Term.(
                const (fun path keys -> guarded (fun () -> show path keys) ())
                $ file
                $ Arg.(value & pos_right 0 string [] & info [] ~docv:"KEY"))
          ; Cmd.v
              (Cmd.info "frame" ~doc:"Frame JSON lines from stdin as LSP messages.")
              Term.(const (guarded frame) $ const ())
          ; Cmd.v
              (Cmd.info
                 "unframe"
                 ~doc:"Print LSP messages from stdin as sorted JSON lines.")
              Term.(const (guarded unframe) $ const ())
          ; stage "tokens" "Tokens with spans." tokens
          ; stage "syntax" "Syntax tree with spans." syntax
          ; stage "typed" "Inferred types or table schemas." typed
          ; Cmd.v
              (Cmd.info "transform-ir" ~doc:"The checked TransformIR with source spans.")
              Term.(const (fun name -> guarded (transform_ir name)) $ binding $ file)
          ; Cmd.v
              (Cmd.info "code" ~doc:"Generated code and source map.")
              Term.(const (fun d -> guarded (code d)) $ dialect $ file)
          ; Cmd.v
              (Cmd.info "mcp" ~doc:"Serve read-only compiler tools over stdio.")
              Term.(const (guarded mcp) $ const ())
          ; Cmd.v
              (Cmd.info
                 "eval"
                 ~doc:
                   "Run SOURCE on the reference evaluator and every target; print \
                    disagreements.")
              Term.(
                const (fun s -> guarded eval s)
                $ Arg.(required & pos 0 (some string) None & info [] ~docv:"SOURCE"))
          ; Cmd.v
              (Cmd.info
                 "bench"
                 ~doc:"Time startup, worker start and round trips on every target.")
              Term.(
                const (fun d -> guarded bench d)
                $ Arg.(value & pos 0 dir "examples" & info [] ~docv:"EXAMPLES"))
          ; Cmd.v
              (Cmd.info
                 "activate"
                 ~doc:"Mark fixture ID active once it passes on every target.")
              Term.(
                const (fun id -> guarded activate id)
                $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ID"))
          ; Cmd.v
              (Cmd.info
                 "fixture"
                 ~doc:
                   "Record a source file as a pending conformance fixture with its \
                    observed outcome.")
              Term.(
                const (fun id -> guarded (fixture id))
                $ Arg.(required & pos 0 (some string) None & info [] ~docv:"ID")
                $ Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE"))
          ]))
;;
