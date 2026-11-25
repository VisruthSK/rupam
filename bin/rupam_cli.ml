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

let generate (project : Project.t) path ds =
  let directory = Filename.dirname path
  and strict = project.strict in
  let scalar compile =
    let mapping = Source_map.create () in
    Source_map.finish mapping (compile (Source_map.mark mapping) ds)
  in
  match Project.require_dialect project, Analysis_program.tabular ds with
  | R_dplyr, true -> R_syntax.program (Tidyverse.compile ~strict ~directory ds)
  | R_dplyr, false -> scalar (fun annotate -> R_scalar.compile ~annotate)
  | Python_polars, true ->
    Python_syntax.program (Analysis_program.python ~strict ~directory ds)
  | Python_polars, false -> scalar (fun annotate -> Python.compile ~annotate)
;;

let run target strict path =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    ignore (Lock.require ~project ~source:path ds);
    let language = Runtime.of_dialect (Project.require_dialect project) in
    Runtime.run
      (Runtime.resolve project language)
      language
      (fst (generate project path ds)))
;;

let lock target strict path =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    let entry = Lock.lock ~project ~source:path ds in
    List.iter
      (fun (file, columns) ->
         Printf.printf "%s
" file;
         List.iter (fun (n, t) -> Printf.printf "  %s  %s
" n t) columns)
      entry.schemas;
    Printf.printf
      "Locked %s for %s: %d input%s
"
      entry.source
      entry.dialect
      (List.length entry.inputs)
      (if List.length entry.inputs = 1 then "" else "s"))
;;

let compile target strict path output =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    let entry = Lock.require ~project ~source:path ds in
    let code, locations = generate project path ds in
    match output with
    | None -> print_string code
    | Some output ->
      Artifact.write
        ~entry
        ~locations
        ~dialect:(Project.require_dialect project)
        ~output
        ~code
        ds)
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
    let project = settings directory target strict in
    ignore (Project.require_dialect project);
    if Lazy.force Line.interactive
    then Console.start ~project ?path ()
    else Repl.start ~project ?path ();
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
  ; command
      "lock"
      "Validate a program's inputs and record them, with any inferred schemas, in        rupam.lock."
      Term.(const lock $ target $ strict $ program)
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
