open Rupam
open Cmdliner

let with_program path action =
  let buf = Lexing.from_string (In_channel.with_open_text path In_channel.input_all) in
  Lexing.set_filename buf path;
  match Repl.diagnostic buf (fun () -> action (Frontend.parse buf)) with
  | Ok () -> Cmd.Exit.ok
  | Error () -> Cmd.Exit.some_error
;;

let settings directory target strict =
  let project = Project.load directory in
  { project with
    dialect = (if Option.is_some target then target else project.dialect)
  ; strict = project.strict && strict
  }
;;

let guarded action =
  match action () with
  | code -> code
  | exception Failure message ->
    Repl.report message;
    Cmd.Exit.some_error
;;

let targeted directory target strict action =
  guarded (fun () ->
    let project = settings directory target strict in
    ignore (Project.require_dialect project);
    action project)
;;

let generate (project : Project.t) ?(annotate = Source_map.plain) path ds =
  let directory = Filename.dirname path
  and strict = project.strict in
  match Project.require_dialect project, Repl.contracted ds with
  | R_dplyr, true -> Tidyverse.compile ~annotate ~strict ~directory ds
  | R_dplyr, false -> R_scalar.compile ~annotate ds
  | Python_polars, true -> Analysis_program.python ~annotate ~strict ~directory ds
  | Python_polars, false -> Python.compile ~annotate ds
;;

let run target strict path =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    let language = Runtime.of_dialect (Project.require_dialect project) in
    Runtime.run (Runtime.resolve project language) language (generate project path ds))
;;

let compile target strict path output =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    match output with
    | None -> print_string (generate project path ds)
    | Some output ->
      let mapping = Source_map.create () in
      let code = generate project ~annotate:(Source_map.mark mapping) path ds in
      let code, locations = Source_map.finish mapping code in
      let target =
        match Project.require_dialect project with
        | R_dplyr -> "r"
        | Python_polars -> "python"
      in
      Artifact.write ~locations ~target ~source:path ~output ~code ds)
;;

let fmt path = with_program path (fun ds -> print_endline (Format.program ds))
let program = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE.rp")

let target =
  Arg.(
    value
    & opt (some (enum Project.dialects)) None
    & info
        [ "t"; "target" ]
        ~docv:"DIALECT"
        ~doc:
          "Target dialect: $(b,r-dplyr) or $(b,python-polars). Overrides [target] \
           dialect in rupam.toml.")
;;

let strict =
  Term.(
    const not
    $ Arg.(
        value
        & flag
        & info
            [ "no-strict" ]
            ~doc:"Allow target-specific code, overriding [check] strict in rupam.toml."))
;;

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let repl target strict path =
  let directory = Option.fold ~none:(Sys.getcwd ()) ~some:Filename.dirname path in
  guarded (fun () ->
    Repl.start ~project:(settings directory target strict) ?path ();
    Cmd.Exit.ok)
;;

let commands =
  [ command
      "run"
      "Compile a program for the target dialect and run the generated script."
      Term.(const run $ target $ strict $ program)
  ; command
      "repl"
      "Start the REPL, optionally loading a program."
      Term.(
        const repl
        $ target
        $ strict
        $ Arg.(value & pos 0 (some file) None & info [] ~docv:"FILE.rp"))
  ; command "fmt" "Print a program in canonical form." Term.(const fmt $ program)
  ; command
      "compile"
      "Compile a program to a standalone script for the target dialect."
      Term.(
        const compile
        $ target
        $ strict
        $ program
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
  let default = Term.(const repl $ target $ strict $ const None) in
  let info = Cmd.info "rupam" ~doc:"A typed language for interactive data analysis." in
  exit (Cmd.eval' (Cmd.group ~default info commands))
;;
