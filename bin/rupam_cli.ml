open Rupam
open Cmdliner

let with_program path action =
  match
    Repl.diagnostic (fun () ->
      action
        (Frontend.read ~file:path (In_channel.with_open_text path In_channel.input_all)))
  with
  | Ok () -> Cmd.Exit.ok
  | Error () -> Cmd.Exit.some_error
;;

let settings directory target strict =
  let project = Project.load directory in
  { project with
    target =
      (match target with
       | None -> project.target
       | Some _ -> { value = target; origin = Session "--target" })
  ; strict =
      (if strict
       then project.strict
       else { value = false; origin = Session "--no-strict" })
  }
;;

let guarded action =
  match action () with
  | code -> code
  | exception exn ->
    Repl.report_diagnostic (Diagnostic.of_exn exn);
    Cmd.Exit.some_error
;;

let targeted directory target strict action =
  guarded (fun () ->
    let project = settings directory target strict in
    ignore (Project.require_target project);
    action project)
;;

let elaborate (project : Project.t) path ds =
  snd
    (Elaborate.program
       (Scope.empty ~directory:(Filename.dirname path) ~strict:project.strict.value)
       ds)
;;

let generate (project : Project.t) items =
  Dialect.script (Project.require_target project) items
;;

let run target strict path =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    let items = elaborate project path ds in
    ignore (Lock.require ~project ~source:path items);
    let language = (Project.require_target project).language in
    Runtime.run (Runtime.resolve project language) language (fst (generate project items)))
;;

let lock target strict path =
  targeted (Filename.dirname path) target strict
  @@ fun project ->
  with_program path (fun ds ->
    let entry = Lock.establish ~project ~source:path (elaborate project path ds) in
    Lock.print_schemas entry;
    Printf.printf
      "Locked %s for %s: %d input%s\n"
      entry.source
      (Target.name entry.target)
      (List.length entry.inputs)
      (if List.length entry.inputs = 1 then "" else "s"))
;;

type emit =
  | Script
  | Transform_ir

let transform_ir strict path =
  guarded (fun () ->
    let project = settings (Filename.dirname path) None strict in
    with_program path (fun ds ->
      List.iter
        (fun item -> print_endline (Sexplib0.Sexp.to_string_hum (Core.sexp_of_item item)))
        (elaborate project path ds)))
;;

let compile target strict path output = function
  | Transform_ir -> transform_ir strict path
  | Script ->
    targeted (Filename.dirname path) target strict
    @@ fun project ->
    with_program path (fun ds ->
      let items = elaborate project path ds in
      let entry = Lock.require ~project ~source:path items in
      let code, locations = generate project items in
      match output with
      | None -> print_string code
      | Some output ->
        Artifact.write
          ~entry
          ~program_sha256:(Data_file.digest path)
          ~locations
          ~output
          ~code
          items)
;;

let check strict path =
  guarded (fun () ->
    let project = settings (Filename.dirname path) None strict in
    with_program path (fun ds -> ignore (elaborate project path ds)))
;;

let fmt path = with_program path (fun ds -> print_endline (Format.program ds))
let program = Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE.rp")

let target =
  Arg.(
    value
    & opt (some (enum Target.targets)) None
    & info
        [ "t"; "target" ]
        ~docv:"TARGET"
        ~doc:
          "Target language and dialect: $(b,r-dplyr) or $(b,python-polars). Overrides \
           [target] in rupam.toml.")
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
    Option.iter (fun _ -> ignore (Project.require_target project)) path;
    if Lazy.force Line.interactive
    then Console.start ~project ?path ()
    else Repl.start ~project ?path ();
    Cmd.Exit.ok)
;;

let session target strict path =
  let directory = Option.fold ~none:(Sys.getcwd ()) ~some:Filename.dirname path in
  guarded (fun () ->
    let project = settings directory target strict in
    let session = Session.create ~project ~directory in
    Option.iter (Session.load session) path;
    Server.run session stdin stdout;
    Cmd.Exit.ok)
;;

let commands =
  [ command
      "session"
      "Serve the analysis session as JSON-RPC 2.0, one message per line."
      Term.(
        const session
        $ target
        $ strict
        $ Arg.(value & pos 0 (some file) None & info [] ~docv:"FILE.rp"))
  ; command
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
      "Validate a program's inputs and record them, with any inferred schemas, in        \
       rupam.lock."
      Term.(const lock $ target $ strict $ program)
  ; command
      "check"
      "Parse and type-check a program without running it."
      Term.(const check $ strict $ program)
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
                ~doc:"Write the script with a manifest and source map.")
        $ Arg.(
            value
            & opt (enum [ "script", Script; "transform-ir", Transform_ir ]) Script
            & info
                [ "emit" ]
                ~docv:"FORM"
                ~doc:"$(b,script) (default) or $(b,transform-ir), the checked IR."))
  ; command
      "schema"
      "Describe the tables in a data-dict contract."
      Term.(
        const (fun path ->
          Contract.describe (Contract.import path);
          Cmd.Exit.ok)
        $ Arg.(required & pos 0 (some file) None & info [] ~docv:"data-dict.yaml"))
  ; command
      "explain"
      "Explain a diagnostic code."
      Term.(
        const (fun code ->
          print_endline (Diagnostic.name code ^ "\n\n" ^ Diagnostic.explanation code);
          Cmd.Exit.ok)
        $ Arg.(
            required
            & pos
                0
                (some (enum (List.map (fun c -> Diagnostic.name c, c) Diagnostic.all)))
                None
            & info [] ~docv:"CODE"))
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
  let info =
    Cmd.info
      "rupam"
      ~version:Version.version
      ~doc:"A typed language for interactive data analysis."
  in
  exit (Cmd.eval' (Cmd.group ~default info commands))
;;
