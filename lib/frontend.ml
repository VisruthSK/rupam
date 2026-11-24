open Parser

let parse lexbuf =
  let complete = function
    | NAME _ | INT _ | FLOAT _ | STRING _ | TRUE | FALSE | NONE | RB | RC | RP | QUESTION
      -> true
    | _ -> false
  in
  let read () =
    let t = Lexer.token lexbuf in
    t, lexbuf.lex_start_p, lexbuf.lex_curr_p
  in
  let pending = ref None
  and previous = ref END
  and depth = ref 0 in
  let pop () =
    match !pending with
    | None -> read ()
    | Some token ->
      pending := None;
      token
  in
  let rec next _ =
    let t, start, finish = pop () in
    let deliver t =
      lexbuf.lex_start_p <- start;
      lexbuf.lex_curr_p <- finish;
      previous := t;
      t
    in
    match t with
    | SEMI -> deliver SEMI
    | END when !depth > 0 || not (complete !previous) -> next lexbuf
    | END ->
      let rec look () =
        match read () with
        | END, _, _ -> look ()
        | t -> t
      in
      let ((following, _, _) as token) = look () in
      pending := Some token;
      (match following with
       | PIPE | BAR | ELSE | THEN | IN -> next lexbuf
       | _ -> deliver END)
    | EOF when complete !previous -> deliver END
    | t ->
      (match t with
       | LP | LB | LC -> incr depth
       | RP | RB | RC -> decr depth
       | _ -> ());
      deliver t
  in
  Parser.program next lexbuf
;;

let valid_prefix source =
  let buf = Lexing.from_string source in
  let rec boundaries depth acc =
    match Lexer.token buf with
    | EOF -> acc
    | LP | LB | LC -> boundaries (depth + 1) acc
    | RP | RB | RC -> boundaries (max 0 (depth - 1)) acc
    | (PIPE | END | SEMI) when depth = 0 ->
      boundaries depth (Lexing.lexeme_start buf :: acc)
    | _ -> boundaries depth acc
    | exception Failure _ -> acc
  in
  List.find_map
    (fun stop ->
       let text = String.trim (String.sub source 0 stop) in
       try
         match parse (Lexing.from_string text) with
         | [] -> None
         | ds -> Some (text, ds)
       with
       | Parser.Error | Failure _ -> None)
    (boundaries 0 [])
;;
