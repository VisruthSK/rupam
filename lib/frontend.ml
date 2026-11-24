open Parser

let parse lexbuf =
  let previous = ref END and depth = ref 0 and pending = ref None in
  let complete = function NAME _ | NUMBER _ | STRING _ | BOOL _ | RB | RC | RP -> true | _ -> false in
  let rec next buf =
    let token = match !pending with
      | Some (t, pos) -> pending := None; buf.Lexing.lex_start_p <- pos; t
      | None -> Lexer.token buf in
    match token with
    | END when !depth > 0 || not (complete !previous) -> next buf
    | END ->
        let rec look () = match Lexer.token buf with END -> look () | t -> t in
        let t = look () in
        pending := Some (t, buf.lex_start_p);
        if t = PIPE || t = BAR || t = ELSE || t = THEN || t = IN then next buf else (previous := END; END)
    | EOF when complete !previous -> previous := END; END
    | t ->
        (match t with LP | LB | LC -> incr depth | RP | RB | RC -> decr depth | _ -> ());
        previous := t; t in
  Parser.program next lexbuf



let valid_prefix source =
  let buf = Lexing.from_string source in
  let rec boundaries depth acc =
    match Lexer.token buf with
    | EOF -> acc
    | LP | LB | LC -> boundaries (depth+1) acc
    | RP | RB | RC -> boundaries (max 0 (depth-1)) acc
    | PIPE | END when depth=0 -> boundaries depth (Lexing.lexeme_start buf::acc)
    | _ -> boundaries depth acc
    | exception Failure _ -> acc in
  List.find_map (fun stop ->
    let text = String.trim (String.sub source 0 stop) in
    try match parse (Lexing.from_string text) with [] -> None | ds -> Some (text,ds)
    with Parser.Error | Failure _ -> None) (boundaries 0 [])
