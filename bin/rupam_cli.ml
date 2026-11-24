open Rupam
open Cmdliner

let with_program path action =
  let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
  Lexing.set_filename buf path;
  match Repl.diagnostic buf (fun () -> action (Frontend.parse buf)) with
  | Ok () -> Cmd.Exit.ok
  | Error () -> Cmd.Exit.some_error
;;

let run path =
  with_program path (fun ds ->
    if Repl.contracted ds
    then
      Analysis_program.run_python
        (Analysis_program.python ~directory:(Filename.dirname path) ds)
    else ignore (Repl.execute Repl.empty.scalar ds))
;;

let fmt path = with_program path (fun ds -> print_endline (Format.program ds))

let compile (target : Repl.backend) path output =
  with_program path (fun ds ->
    let directory = Filename.dirname path in
    let mapping = Source_map.create () in
    let annotate =
      match output with
      | None -> Source_map.plain
      | Some _ -> Source_map.mark mapping
    in
    let code =
      match target, Repl.contracted ds with
      | R, true -> Tidyverse.compile ~annotate ~directory ds
      | R, false -> R_scalar.compile ~annotate ds
      | Python, true -> Analysis_program.python ~annotate ~directory ds
      | Python, false -> Python.compile ~annotate ds
    in
    match output with
    | None -> print_string code
    | Some output ->
      let code, locations = Source_map.finish mapping code in
      let target =
        match target with
        | R -> "r"
        | Python -> "python"
      in
      Artifact.write ~locations ~target ~source:path ~output ~code ds)
;;

let program = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE.rp")

let target =
  Arg.(
    required
    & pos 0 (some (enum Repl.backends)) None
    & info [] ~docv:"TARGET" ~doc:"$(b,r) or $(b,python).")
;;

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let commands =
  [ command "run" "Check and run a program." Term.(const run $ program)
  ; command
      "repl"
      "Start the REPL, optionally loading a program."
      Term.(
        const (fun path ->
          Repl.start ?path ();
          Cmd.Exit.ok)
        $ Arg.(value & pos 0 (some file) None & info [] ~docv:"FILE.rp"))
  ; command "fmt" "Print a program in canonical form." Term.(const fmt $ program)
  ; command
      "compile"
      "Compile a program to a standalone R or Python script."
      Term.(
        const compile
        $ target
        $ Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE.rp")
        $ Arg.(
            value
            & opt (some string) None
            & info
                [ "o"; "output" ]
                ~docv:"OUT"
                ~doc:"Write the script with a manifest and source map."))
  ; command
      "schema"
      "Describe the tables in a data-dict contract."
      Term.(
        const (fun path ->
          Contract.describe (Contract.import path);
          Cmd.Exit.ok)
        $ Arg.(required & pos 0 (some file) None & info [] ~docv:"data-dict.yaml"))
  ; command
      "lsp"
      "Serve the language server over stdio."
      Term.(
        const (fun () ->
          Lsp.run ();
          Cmd.Exit.ok)
        $ const ())
  ]
;;

let () =
  let default =
    Term.(
      const (fun () ->
        Repl.start ();
        Cmd.Exit.ok)
      $ const ())
  in
  let info = Cmd.info "rupam" ~doc:"A typed language for interactive data analysis." in
  exit (Cmd.eval' (Cmd.group ~default info commands))
;;
