open Rupam

let terminal_line () =
  let line = read_line () in
  if String.ends_with ~suffix:"\r" line
  then String.sub line 0 (String.length line - 1)
  else line
;;

let diagnostic buf f =
  try Ok (f ()) with
  | Sys.Break ->
    prerr_endline "Cancelled";
    Error ()
  | Infer.Error ((p, _), msg) | Eval.Error ((p, _), msg) ->
    Printf.eprintf
      "%s%d:%d: %s\n"
      (if p.pos_fname = "" then "" else p.pos_fname ^ ":")
      p.pos_lnum
      (p.pos_cnum - p.pos_bol + 1)
      msg;
    Error ()
  | (Parser.Error | Failure _ | Shape.Error _) as exn ->
    let msg =
      match exn with
      | Failure m | Shape.Error m -> m
      | _ -> "Syntax error"
    in
    Printf.eprintf
      "%d:%d: %s\n"
      buf.Lexing.lex_curr_p.pos_lnum
      (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol + 1)
      msg;
    Error ()
;;

let execute (types, values) ds =
  let types = List.fold_left (fun env d -> fst (Infer.declaration env d)) types ds in
  let values, result = Eval.run values ds in
  Option.iter (fun v -> print_endline (Eval.display v)) result;
  types, values
;;

let contracted =
  List.exists (function
    | Syntax.Contract _ -> true
    | _ -> false)
;;

type session =
  { scalar : (string * Infer.scheme) list * (string * Eval.value) list
  ; history : Syntax.declaration list
  ; tables : Analysis_program.environment
  ; lexical : Check.env
  ; directory : string
  ; backend : string
  ; source_path : string option
  }

let submit state ds =
  let history = state.history @ ds in
  if contracted history
  then (
    let inputs = if contracted state.history then ds else history in
    let tables, lexical, _ =
      Analysis_program.extend ~directory:state.directory state.tables state.lexical inputs
    in
    { state with history; tables; lexical })
  else { state with scalar = execute state.scalar ds; history }
;;

let rec submission state source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | ds -> Result.value (diagnostic buf (fun () -> submit state ds)) ~default:state
  | exception Parser.Error when Lexing.lexeme buf = "" ->
    print_string "    .. ";
    flush stdout;
    (match terminal_line () with
     | exception End_of_file ->
       prerr_endline "Incomplete expression";
       state
     | ":cancel" -> state
     | ":prefix" ->
       (match Frontend.valid_prefix source with
        | None -> print_endline "No complete prefix"
        | Some (text, ds) ->
          print_endline text;
          ignore
            (diagnostic (Lexing.from_string text) (fun () ->
               if contracted (state.history @ ds)
               then (
                 let bindings, _ =
                   Analysis_program.analyze ~directory:state.directory (state.history @ ds)
                 in
                 match List.rev ds with
                 | Syntax.Let (n, _, _) :: _ ->
                   (match List.assoc_opt n bindings with
                    | Some (Dataset d) -> Analysis_program.schema d
                    | _ -> ())
                 | _ -> ())
               else
                 ignore
                   (List.fold_left
                      (fun env d -> fst (Infer.declaration env d))
                      (fst state.scalar)
                      ds))));
       submission state source
     | more -> submission state (source ^ "\n" ^ more))
  | exception ((Parser.Error | Failure _) as exn) ->
    ignore (diagnostic buf (fun () -> raise exn));
    state
;;

let rec repl state =
  print_string "rupam> ";
  flush stdout;
  match terminal_line () with
  | exception End_of_file -> ()
  | ":quit" -> ()
  | ":backend r" -> repl { state with backend = "r" }
  | ":backend python" -> repl { state with backend = "python" }
  | ":reload" ->
    let result =
      diagnostic (Lexing.from_string "") (fun () ->
        let history =
          match state.source_path with
          | None -> state.history
          | Some p ->
            In_channel.with_open_text p (fun ch ->
              Frontend.parse (Lexing.from_channel ch))
        in
        submit
          { state with history = []; tables = []; lexical = []; scalar = [], [] }
          history)
    in
    repl (Result.value result ~default:state)
  | ":help" ->
    print_endline
      ":type EXPR   :bindings   :schema TABLE   :preview TABLE   :inspect TABLE   \
       :explain TABLE   :contract TABLE   :validate TABLE   :backend r|python   \
       :reload   :code r|python TABLE   :quit";
    repl state
  | ":bindings" ->
    List.iter
      (fun (name, b) ->
         match b with
         | Analysis_program.Dataset _ -> Printf.printf "%s : Table\n" name
         | Dictionary _ -> Printf.printf "%s : Contract\n" name
         | Function _ -> Printf.printf "%s : function\n" name)
      state.tables;
    List.iter
      (fun (name, (_, t)) -> Printf.printf "%s : %s\n" name (Infer.display t))
      (fst state.scalar);
    repl state
  | line
    when List.exists
           (fun prefix -> String.starts_with ~prefix line)
           [ ":schema "
           ; ":preview "
           ; ":code "
           ; ":explain "
           ; ":contract "
           ; ":validate "
           ; ":inspect "
           ] ->
    ignore
      (diagnostic (Lexing.from_string line) (fun () ->
         let split text =
           match String.index_opt text ' ' with
           | Some i ->
             ( String.sub text 0 i
             , String.trim (String.sub text (i + 1) (String.length text - i - 1)) )
           | None -> text, ""
         in
         let command, argument = split (String.trim line) in
         let target, source =
           if command = ":code" then split argument else state.backend, argument
         in
         let d =
           match Frontend.parse (Lexing.from_string source) with
           | [ Syntax.Evaluate x ] ->
             Analysis_program.dataset state.tables state.lexical x
           | _ -> failwith "Expected a table expression"
         in
         match command with
         | ":schema" -> Analysis_program.schema d
         | ":contract" ->
           Printf.printf "%s (%s), values pending\n" d.dictionary.path d.table.name
         | ":validate" -> Analysis_program.validate d
         | ":explain" -> Analysis_program.explain d
         | ":preview" | ":inspect" ->
           let page =
             if state.backend = "r"
             then Tidyverse.preview_page
             else Analysis_program.preview_page
           in
           let d = Analysis_program.public_view d in
           let fields = Table.names d.plan in
           let rec view offset column =
             let ns = List.filteri (fun i _ -> i >= column && i < column + 8) fields in
             let schema = List.map (fun n -> n, List.assoc n d.plan.schema) ns in
             let visible =
               { d with plan = { d.plan with schema; steps = Select ns :: d.plan.steps } }
             in
             page visible ~offset ~limit:20;
             if command = ":inspect"
             then (
               print_string "[n/p] rows  [l/h] columns  [q] quit > ";
               flush stdout;
               match terminal_line () with
               | "n" -> view (offset + 20) column
               | "p" -> view (max 0 (offset - 20)) column
               | "l" -> view offset (min (max 0 (List.length fields - 8)) (column + 8))
               | "h" -> view offset (max 0 (column - 8))
               | _ -> ()
               | exception End_of_file -> ())
           in
           view 0 0
         | ":code" ->
           print_endline
             (match target with
              | "r" -> Tidyverse.table d
              | "python" -> Analysis_program.python_table d
              | _ -> failwith "Expected r or python")
         | _ -> assert false));
    repl state
  | line when String.starts_with ~prefix:":type " line ->
    let buf = Lexing.from_string (String.sub line 6 (String.length line - 6)) in
    ignore
      (diagnostic buf (fun () ->
         match Frontend.parse buf with
         | [ Syntax.Evaluate x ] ->
           (try
              print_endline (Infer.display (Infer.expression (fst state.scalar) 1 x))
            with
            | Infer.Error _ when contracted state.history ->
              Analysis_program.schema
                (Analysis_program.dataset state.tables state.lexical x))
         | _ -> failwith "Expected one expression"));
    repl state
  | line -> repl (submission state line)
;;

let file path action =
  In_channel.with_open_text path (fun ch ->
    let buf = Lexing.from_channel ch in
    Lexing.set_filename buf path;
    if Result.is_error (diagnostic buf (fun () -> action (Frontend.parse buf)))
    then exit 1)
;;

let empty =
  { scalar = [], []
  ; history = []
  ; tables = []
  ; lexical = []
  ; directory = "."
  ; backend = "python"
  ; source_path = None
  }
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "lsp" ] -> Lsp.run ()
  | [ _ ] ->
    Sys.catch_break true;
    repl empty
  | [ _; "repl"; path ] ->
    Sys.catch_break true;
    file path (fun ds ->
      repl
        (submit
           { empty with directory = Filename.dirname path; source_path = Some path }
           ds))
  | [ _; "compile"; target; path ] | [ _; "compile"; target; path; "-o"; _ ] ->
    file path (fun ds ->
      let directory = Filename.dirname path in
      let mapping = Source_map.create () in
      let annotate =
        if Array.length Sys.argv = 4 then Source_map.plain else Source_map.mark mapping
      in
      let code =
        match target with
        | "r" ->
          if contracted ds
          then Tidyverse.compile ~annotate ~directory ds
          else R_scalar.compile ~annotate ds
        | "python" ->
          if contracted ds
          then Analysis_program.python ~annotate ~directory ds
          else Python.compile ~annotate ds
        | _ -> failwith "Expected target r or python"
      in
      if Array.length Sys.argv = 4
      then print_string code
      else (
        let code, locations = Source_map.finish mapping code in
        Artifact.write ~locations ~target ~source:path ~output:Sys.argv.(5) ~code ds))
  | [ _; "fmt"; path ] -> file path (fun ds -> print_endline (Format.program ds))
  | [ _; "schema"; path ] -> Contract.describe (Contract.import path)
  | [ _; path ] ->
    file path (fun ds ->
      if contracted ds
      then
        Analysis_program.run_python
          (Analysis_program.python ~directory:(Filename.dirname path) ds)
      else ignore (execute ([], []) ds))
  | _ ->
    prerr_endline "Usage: rupam [FILE.rp | fmt FILE.rp | compile python FILE.rp]";
    exit 2
;;
