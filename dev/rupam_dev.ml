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

let tokens path =
  let buf = source path in
  let rec loop () =
    match Lexer.token buf with
    | Parser.EOF -> ()
    | _ ->
      Printf.printf "%s  %s\n" (span buf) (Lexing.lexeme buf);
      loop ()
  in
  loop ()
;;

let syntax path =
  List.iter
    (fun d -> print_endline (Sexplib0.Sexp.to_string_hum (Syntax.sexp_of_declaration d)))
    (Frontend.parse (source path))
;;

let elaborate path =
  let project = Project.load (Filename.dirname path) in
  snd
    (Elaborate.program
       (Scope.empty ~directory:(Filename.dirname path) ~strict:project.strict.value)
       (Frontend.parse (source path)))
;;

let transform_ir path =
  List.iter
    (fun item ->
       print_endline (Sexplib0.Sexp.to_string_hum (TransformIR.sexp_of_item item)))
    (elaborate path)
;;

let typed path =
  List.iter
    (function
      | TransformIR.Define (id, e) ->
        Printf.printf "%s : %s\n" id.name (Types.display e.typ)
      | Show e -> Printf.printf "show : %s\n" (Types.display e.typ)
      | Write (e, file) -> Printf.printf "write %s : %s\n" file (Types.display e.typ)
      | Variant (name, tags) ->
        Printf.printf "type %s = %s\n" name (String.concat " | " (List.map fst tags)))
    (elaborate path)
;;

let code dialect path =
  let text, locations = Dialect.script dialect (elaborate path) in
  print_string text;
  print_endline "# source map";
  List.iter
    (fun (l : Document.location) ->
       Printf.printf
         "%d:%d <- %s\n"
         l.line
         l.column
         (Sexplib0.Sexp.to_string (Syntax.sexp_of_span l.source)))
    locations
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
let stage name doc f = Cmd.v (Cmd.info name ~doc) Term.(const (guarded f) $ file)

let dialect =
  Arg.(
    value
    & opt (enum Target.targets) Target.r_dplyr
    & info [ "t"; "target" ] ~docv:"TARGET")
;;

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "rupam-dev" ~doc:"Internal tools for developing Rupam.")
          [ stage "tokens" "Tokens with spans." tokens
          ; stage "syntax" "Syntax tree with spans." syntax
          ; stage "typed" "Inferred types or table schemas." typed
          ; stage "transform-ir" "The checked TransformIR of every item." transform_ir
          ; Cmd.v
              (Cmd.info "code" ~doc:"Generated code and source map.")
              Term.(const (fun d -> guarded (code d)) $ dialect $ file)
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
