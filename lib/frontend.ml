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
        if t = PIPE then next buf else (previous := END; END)
    | EOF when complete !previous -> previous := END; END
    | t ->
        (match t with LP | LB | LC -> incr depth | RP | RB | RC -> decr depth | _ -> ());
        previous := t; t in
  Parser.program next lexbuf

