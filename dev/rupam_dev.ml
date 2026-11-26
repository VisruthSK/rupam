open Rupam
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
       (Scope.empty ~directory ~strict:project.strict.value)
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
  let text, locations = Dialect.script dialect items in
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
  let order =
    match keys with
    | [] -> ""
    | keys ->
      " ORDER BY "
      ^ String.concat ", " (List.map (fun k -> Printf.sprintf "%S NULLS LAST" k) keys)
  in
  let names, rows =
    Data_file.query ("SELECT * FROM read_parquet(" ^ Data_file.literal path ^ ")" ^ order)
  in
  let typed (c : Data_file.column) =
    c.name
    ^ ": "
    ^
    match c.typ with
    | Supported t -> Types.display (if c.nullable then Types.optional t else t)
    | Unsupported raw -> raw
  in
  print_endline (String.concat ", " (List.map typed (Data_file.columns path)));
  ignore names;
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
