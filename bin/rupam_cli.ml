open Rupam

let diagnostic buf f =
  try Ok (f ()) with
  | Infer.Error ((p, _), msg) | Eval.Error ((p, _), msg) ->
      Printf.eprintf "%s%d:%d: %s\n" (if p.pos_fname = "" then "" else p.pos_fname ^ ":")
        p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg;
      Error ()
  | Parser.Error | Failure _ as exn ->
      let msg = match exn with Failure m -> m | _ -> "Syntax error" in
      Printf.eprintf "%d:%d: %s\n" buf.Lexing.lex_curr_p.pos_lnum
        (buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol + 1) msg;
      Error ()

let execute (types, values) ds =
  let types = List.fold_left (fun env d -> fst (Infer.declaration env d)) types ds in
  let values, result = Eval.run values ds in
  Option.iter (fun v -> print_endline (Eval.display v)) result;
  types, values

let rec repl state =
  print_string "rupam> "; flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | ":quit" -> ()
  | ":help" -> print_endline ":type EXPR   :bindings   :quit"; repl state
  | ":bindings" ->
      List.iter (fun (name, (_, t)) -> Printf.printf "%s : %s\n" name (Infer.display t)) (fst state);
      repl state
  | line when String.starts_with ~prefix:":type " line ->
      let buf = Lexing.from_string (String.sub line 6 (String.length line - 6)) in
      ignore (diagnostic buf (fun () -> match Frontend.parse buf with
        | [Syntax.Evaluate x] -> print_endline (Infer.display (Infer.expression (fst state) 1 x))
        | _ -> failwith "Expected one expression"));
      repl state
  | line ->
      let buf = Lexing.from_string line in
      let result = diagnostic buf (fun () -> execute state (Frontend.parse buf)) in
      repl (Result.value result ~default:state)

let file path action =
  In_channel.with_open_text path (fun ch ->
    let buf = Lexing.from_channel ch in
    Lexing.set_filename buf path;
    if Result.is_error (diagnostic buf (fun () -> action (Frontend.parse buf))) then exit 1)

let () = match Array.to_list Sys.argv with
  | [_] -> repl ([], [])
  | [_; "compile"; "python"; path] -> file path (fun ds -> print_string (Python.compile ds))
  | [_; "fmt"; path] -> file path (fun ds -> print_endline (Format.program ds))
  | [_; path] -> file path (fun ds -> ignore (execute ([], []) ds))
  | _ -> prerr_endline "Usage: rupam [FILE.rp | fmt FILE.rp | compile python FILE.rp]"; exit 2
