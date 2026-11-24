open Rupam

let diagnostic buf f =
  try Ok (f ()) with
  | Infer.Error ((p, _), msg) | Eval.Error ((p, _), msg) ->
      Printf.eprintf "%s%d:%d: %s\n" (if p.pos_fname = "" then "" else p.pos_fname ^ ":")
        p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg;
      Error ()
  | Parser.Error | Failure _ | Shape.Error _ as exn ->
      let msg = match exn with Failure m | Shape.Error m -> m | _ -> "Syntax error" in
      Printf.eprintf "%d:%d: %s\n" buf.Lexing.lex_curr_p.pos_lnum
        (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol + 1) msg;
      Error ()

let execute (types, values) ds =
  let types = List.fold_left (fun env d -> fst (Infer.declaration env d)) types ds in
  let values, result = Eval.run values ds in
  Option.iter (fun v -> print_endline (Eval.display v)) result;
  types, values

let contracted = List.exists (function Syntax.Contract _ -> true | _ -> false)
type session = { scalar : (string * Infer.scheme) list * (string * Eval.value) list;
                 history : Syntax.declaration list; tables : Analysis_program.environment; directory : string }

let submit state ds =
  let history = state.history @ ds in
  if contracted history then (
    let tables,_ = Analysis_program.analyze ~directory:state.directory history in
    {state with history;tables})
  else {state with scalar=execute state.scalar ds;history}

let rec submission state source =
  let buf = Lexing.from_string source in
  match Frontend.parse buf with
  | ds -> Result.value (diagnostic buf (fun () -> submit state ds)) ~default:state
  | exception Parser.Error when Lexing.lexeme buf = "" ->
      print_string "    .. "; flush stdout;
      (match read_line () with
       | exception End_of_file -> prerr_endline "Incomplete expression"; state
       | ":cancel" -> state
       | more -> submission state (source ^ "\n" ^ more))
  | exception (Parser.Error | Failure _ as exn) ->
      ignore (diagnostic buf (fun () -> raise exn)); state

let rec repl state =
  print_string "rupam> "; flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | ":quit" -> ()
  | ":help" -> print_endline ":type EXPR   :bindings   :schema TABLE   :preview TABLE   :code r|python TABLE   :quit"; repl state
  | ":bindings" ->
      List.iter (fun (name, (_, t)) -> Printf.printf "%s : %s\n" name (Infer.display t)) (fst state.scalar);
      repl state
  | line when String.starts_with ~prefix:":schema " line || String.starts_with ~prefix:":preview " line || String.starts_with ~prefix:":code " line ->
      ignore (diagnostic (Lexing.from_string line) (fun () ->
        let bound = Analysis_program.bound state.tables in
        match String.split_on_char ' ' (String.trim line) |> List.filter ((<>) "") with
        | [":schema";n] -> Analysis_program.schema (bound n)
        | [":preview";n] -> Analysis_program.preview (bound n)
        | [":code";target;n] ->
            let d = bound n in
            let emit = match target with "r" -> Tidyverse.lower | "python" -> Polars.lower
              | _ -> failwith "Expected r or python" in
            print_endline (emit ~fields:d.fields d.plan)
        | _ -> failwith "Expected table name"));
      repl state
  | line when String.starts_with ~prefix:":type " line ->
      let buf = Lexing.from_string (String.sub line 6 (String.length line - 6)) in
      ignore (diagnostic buf (fun () -> match Frontend.parse buf with
        | [Syntax.Evaluate x] -> print_endline (Infer.display (Infer.expression (fst state.scalar) 1 x))
        | _ -> failwith "Expected one expression"));
      repl state
  | line ->
      repl (submission state line)

let file path action =
  In_channel.with_open_text path (fun ch ->
    let buf = Lexing.from_channel ch in
    Lexing.set_filename buf path;
    if Result.is_error (diagnostic buf (fun () -> action (Frontend.parse buf))) then exit 1)

let empty = {scalar=([],[]); history=[];tables=[];directory="."}

let () = match Array.to_list Sys.argv with
  | [_] -> repl empty
  | [_; "repl"; path] -> file path (fun ds -> repl (submit {empty with directory=Filename.dirname path} ds))
  | [_; "compile"; "r"; path] -> file path (fun ds -> print_string (Tidyverse.compile ~directory:(Filename.dirname path) ds))
  | [_; "compile"; "python"; path] -> file path (fun ds -> print_string (if contracted ds then Analysis_program.python ~directory:(Filename.dirname path) ds else Python.compile ds))
  | [_; "fmt"; path] -> file path (fun ds -> print_endline (Format.program ds))
  | [_; "schema"; path] -> Contract.describe (Contract.import path)
  | [_; path] -> file path (fun ds -> if contracted ds then Analysis_program.run_python (Analysis_program.python ~directory:(Filename.dirname path) ds) else ignore (execute ([], []) ds))
  | _ -> prerr_endline "Usage: rupam [FILE.rp | fmt FILE.rp | compile python FILE.rp]"; exit 2






