open Parser

let hole x =
  match x.Syntax.form with
  | Syntax.Name (Bare, "_") -> true
  | _ -> false
;;

let rec desugar x =
  let x = Syntax.map desugar x in
  match x.form with
  | Pipe (piped, ({ form = Call (f, args); _ } as call))
    when List.exists (fun (_, a) -> hole a) args ->
    let filled = ref false in
    let args =
      List.map
        (fun (n, a) ->
           if hole a && not !filled
           then (
             filled := true;
             n, piped)
           else n, a)
        args
    in
    desugar { x with form = Call (f, args) }
    |> fun result -> { result with span = call.span }
  | Call (f, args) when List.exists (fun (_, a) -> hole a) args ->
    let count = ref 0 in
    let args =
      List.map
        (fun (n, a) ->
           if hole a
           then (
             incr count;
             n, { a with form = Name (Bare, "_" ^ string_of_int !count) })
           else n, a)
        args
    in
    let params = List.init !count (fun i -> "_" ^ string_of_int (i + 1)) in
    { x with form = Lambda (params, { x with form = Call (f, args) }) }
  | _ -> x
;;

let declaration = function
  | Syntax.Let (n, None, x) ->
    (match desugar x with
     | { form = Lambda (ps, body); _ } -> Syntax.Let (n, Some ps, body)
     | x -> Syntax.Let (n, None, x))
  | Syntax.Let (n, ps, x) -> Syntax.Let (n, ps, desugar x)
  | Evaluate x -> Evaluate (desugar x)
  | d -> d
;;

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
  let pending = ref []
  and previous = ref END
  and brackets = ref [] in
  let pop () =
    match !pending with
    | [] -> read ()
    | token :: rest ->
      pending := rest;
      token
  in
  let rec significant () =
    match pop () with
    | END, _, _ -> significant ()
    | token -> token
  in
  let rec next _ =
    let t, start, finish = pop () in
    let deliver t =
      lexbuf.lex_start_p <- start;
      lexbuf.lex_curr_p <- finish;
      previous := t;
      t
    in
    let inside_brackets =
      match !brackets with
      | `Block :: _ | [] -> false
      | `Group :: _ -> true
    in
    match t with
    | SEMI -> deliver SEMI
    | END when inside_brackets || not (complete !previous) -> next lexbuf
    | END ->
      let ((following, _, _) as token) = significant () in
      pending := token :: !pending;
      (match following with
       | PIPE | BAR | ELSE | IN | RP | RB | RC -> next lexbuf
       | _ -> deliver END)
    | EOF when complete !previous -> deliver END
    | LC ->
      let ((following, _, _) as token) = significant () in
      pending := token :: !pending;
      brackets := (if following = LET then `Block else `Group) :: !brackets;
      deliver LC
    | LP | LB ->
      brackets := `Group :: !brackets;
      deliver t
    | RP | RB | RC ->
      (brackets
       := match !brackets with
          | _ :: rest -> rest
          | [] -> []);
      deliver t
    | t -> deliver t
  in
  List.map declaration (Parser.program next lexbuf)
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
