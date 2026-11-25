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

let typed path =
  let declarations = Frontend.parse (source path) in
  if Analysis_program.tabular declarations
  then (
    let env, _ =
      Analysis_program.analyze ~directory:(Filename.dirname path) declarations
    in
    List.iter
      (function
        | name, Analysis_program.Dataset d ->
          print_endline name;
          Analysis_program.schema d
        | _ -> ())
      env)
  else
    ignore
      (List.fold_left
         (fun env d ->
            let env, t = Infer.declaration env d in
            (match d with
             | Syntax.Let (name, _, _) -> Printf.printf "%s : %s\n" name (Infer.display t)
             | _ -> Printf.printf "- : %s\n" (Infer.display t));
            env)
         Infer.empty
         declarations)
;;

let code dialect path =
  let project = Project.load (Filename.dirname path) in
  let text, locations =
    Dialect.program
      dialect
      ~strict:project.strict.value
      ~directory:(Filename.dirname path)
      (Frontend.parse (source path))
  in
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
    & opt (enum Project.dialects) Project.R_dplyr
    & info [ "t"; "target" ] ~docv:"DIALECT")
;;

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "rupam-dev" ~doc:"Internal tools for developing Rupam.")
          [ stage "tokens" "Tokens with spans." tokens
          ; stage "syntax" "Syntax tree with spans." syntax
          ; stage "typed" "Inferred types or table schemas." typed
          ; Cmd.v
              (Cmd.info "code" ~doc:"Generated code and source map.")
              Term.(const (fun d -> guarded (code d)) $ dialect $ file)
          ]))
;;
