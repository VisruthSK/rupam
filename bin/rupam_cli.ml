open Rupam
open Rupam_native
open Rupam_console
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
       (Scope.empty
          ~inputs:Native.inputs
          ~directory:(Filename.dirname path)
          ~strict:project.strict.value
          ())
       ds)
;;

let generate (project : Project.t) items =
  let module B = (val Backend.of_target (Project.require_target project)) in
  B.script items
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
    let items = elaborate project path ds in
    let session = Session.create ~project ~directory:(Filename.dirname path) in
    Fun.protect
      ~finally:(fun () -> Session.close session)
      (fun () -> Session.verify session items);
    let entry = Lock.establish ~project ~source:path items in
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
        (fun item ->
           print_endline (Sexplib0.Sexp.to_string_hum (TransformIR.sexp_of_item item)))
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

let choose (project : Project.t) =
  match project.target.value with
  | Some _ -> project
  | None when not (Lazy.force Line.interactive) ->
    ignore (Project.require_target project);
    project
  | None ->
    let label (target : Target.t) =
      match target.dialect with
      | Dplyr -> "R (dplyr)"
      | Polars -> "Python (Polars)"
    in
    let chosen =
      match
        Console.pick ~title:"Select an execution target" (List.map label Target.all)
      with
      | Some i -> List.nth Target.all i
      | None -> Diagnostic.fail Missing_target "No target chosen"
    in
    { project with target = { value = Some chosen; origin = Session "selection" } }
;;

let ready (project : Project.t) =
  let target = Project.require_target project in
  if not (Runtime.available (Runtime.resolve project target.language) target.language)
  then
    Diagnostic.fail
      Host_failure
      (Printf.sprintf
         "%s is not available here. Install it or choose another target with --target"
         (Target.language_name target.language))
;;

let repl target strict path =
  let directory = Option.fold ~none:(Sys.getcwd ()) ~some:Filename.dirname path in
  guarded (fun () ->
    let project = choose (settings directory target strict) in
    ready project;
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
          Contract.describe (Data_dict.import path);
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
  let argv = Array.map (fun arg -> if arg = "-h" then "--help" else arg) Sys.argv in
  exit (Cmd.eval' ~argv (Cmd.group ~default info commands))
;;
