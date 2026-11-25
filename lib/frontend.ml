open Parser

let hole x =
  match x.Syntax.form with
  | Syntax.Name (Bare, "_") -> true
  | _ -> false
;;

let pieces parse (x : Syntax.expr) text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let flush acc =
    if Buffer.length buffer = 0
    then acc
    else (
      let piece = Syntax.Text_piece (Buffer.contents buffer) in
      Buffer.clear buffer;
      piece :: acc)
  in
  let slot start finish =
    let source = String.sub text start (finish - start) in
    let trimmed = String.trim source in
    let echo, source =
      if String.ends_with ~suffix:"=" trimmed
         && not (String.ends_with ~suffix:"==" trimmed)
      then (
        let body = String.sub trimmed 0 (String.length trimmed - 1) in
        Some (body ^ "="), body)
      else None, source
    in
    let format, source =
      match String.rindex_opt source ':' with
      | Some i
        when String.for_all
               (fun c -> String.contains "<>^=+- 0123456789,.dfeg%" c)
               (String.sub source (i + 1) (String.length source - i - 1)) ->
        Some (String.sub source (i + 1) (String.length source - i - 1)), String.sub source 0 i
      | _ -> None, source
    in
    let position = fst x.span in
    let value = parse { position with pos_cnum = position.pos_cnum + start + 1 } source in
    Syntax.Slot { value; format; echo }
  in
  let rec scan i acc =
    if i >= length
    then List.rev (flush acc)
    else (
      match text.[i] with
      | '{' when i + 1 < length && text.[i + 1] = '{' ->
        Buffer.add_char buffer '{';
        scan (i + 2) acc
      | '}' when i + 1 < length && text.[i + 1] = '}' ->
        Buffer.add_char buffer '}';
        scan (i + 2) acc
      | '{' ->
        let rec close j depth =
          if j >= length
          then failwith "Unclosed { in string"
          else (
            match text.[j] with
            | '{' -> close (j + 1) (depth + 1)
            | '}' when depth = 0 -> j
            | '}' -> close (j + 1) (depth - 1)
            | _ -> close (j + 1) depth)
        in
        let finish = close (i + 1) 0 in
        let acc = flush acc in
        scan (finish + 1) (slot (i + 1) finish :: acc)
      | '}' -> failwith "Unmatched } in string; write }} for a literal brace"
      | c ->
        Buffer.add_char buffer c;
        scan (i + 1) acc)
  in
  scan 0 []
;;

let rec desugar parse x =
  let desugar = desugar parse in
  let x = Syntax.map desugar x in
  match x.form with
  | Literal (Text text) when String.contains text '{' || String.contains text '}' ->
    (match pieces parse x text with
     | [ Text_piece plain ] -> { x with form = Literal (Text plain) }
     | [] -> { x with form = Literal (Text "") }
     | pieces -> { x with form = Interpolate pieces })
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

let rec parse lexbuf =
  let expression position source =
    let buffer = Lexing.from_string source in
    Lexing.set_position buffer position;
    match parse buffer with
    | [ Syntax.Evaluate value ] -> value
    | _ -> failwith "A string slot holds one expression"
  in
  let desugar = desugar expression in
  let declaration = function
    | Syntax.Let (n, None, x) ->
      (match desugar x with
       | { form = Lambda (ps, body); _ } -> Syntax.Let (n, Some ps, body)
       | x -> Syntax.Let (n, None, x))
    | Syntax.Let (n, ps, x) -> Syntax.Let (n, ps, desugar x)
    | Evaluate x -> Evaluate (desugar x)
    | d -> d
  in
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
