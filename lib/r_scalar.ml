open Syntax

let compile ?(annotate = Source_map.plain) ds =
  let next = ref 0 in
  let fresh () =
    incr next;
    "v" ^ string_of_int !next
  in
  let q = Polars.quoted in
  let rec emit env x = annotate x.span (render env x)
  and render env x =
    let exp = emit env in
    let call f args = "(" ^ f ^ ")(" ^ String.concat ", " args ^ ")" in
    let args xs =
      List.map
        (function
          | None, v -> exp v
          | _ -> raise (Infer.Error (x.span, "Expected positional arguments")))
        xs
    in
    let lambda ps body =
      let vs = List.map (fun n -> n, fresh ()) ps in
      let names = List.map snd vs in
      "function("
      ^ String.concat ", " names
      ^ ") { "
      ^ String.concat "" (List.map (fun v -> "force(" ^ v ^ "); ") names)
      ^ emit (vs @ env) body
      ^ " }"
    in
    match x.form with
    | Name (Bare, n) -> List.assoc n env
    | Literal Absent -> "NULL"
    | Literal (Bool true) -> "TRUE"
    | Literal (Bool false) -> "FALSE"
    | Literal (Text s) -> q s
    | Literal (Float s) -> s
    | Literal (Int s) -> "rp_int(" ^ q s ^ ")"
    | Annotated (v, _) -> exp v
    | Tuple vs | List vs -> call "list" (List.map exp vs)
    | Record fs ->
      "list(" ^ String.concat ", " (List.map (fun (n, v) -> q n ^ " = " ^ exp v) fs) ^ ")"
    | Field (v, n) -> "(" ^ exp v ^ ")[[" ^ q n ^ "]]"
    | Lambda (ps, v) -> lambda ps v
    | Local (n, v, b) -> call (lambda [ n ] b) [ exp v ]
    | If (c, a, b) -> "(if (" ^ exp c ^ ") " ^ exp a ^ " else " ^ exp b ^ ")"
    | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) ->
      call "rp_variant" [ q "Some"; exp v ]
    | Call (f, vs) -> call (exp f) (args vs)
    | Unary (Not, v) -> "(!" ^ exp v ^ ")"
    | Unary (Negate, v) -> "rp_numeric(-" ^ exp v ^ ")"
    | Pipe (v, f) ->
      let id = fresh () in
      let body =
        match f.form with
        | Call (g, vs) -> call (exp g) (id :: args vs)
        | _ -> call (exp f) [ id ]
      in
      call ("function(" ^ id ^ ") { force(" ^ id ^ "); " ^ body ^ " }") [ exp v ]
    | Binary (op, a, b) ->
      let symbol =
        match op with
        | And -> "&&"
        | Or -> "||"
        | op -> binary_symbol op
      in
      let v = "(" ^ exp a ^ " " ^ symbol ^ " " ^ exp b ^ ")" in
      (match op with
       | Add | Sub | Mul | Power -> "rp_numeric(" ^ v ^ ")"
       | Not_equal -> "(isTRUE(" ^ v ^ ") || is.na(" ^ v ^ "))"
       | op when comparison op -> "isTRUE(" ^ v ^ ")"
       | _ -> v)
    | Match (subject, bs) ->
      let id = fresh () in
      let rec pattern path = function
        | Precord ns -> [], List.map (fun n -> n, path ^ "[[" ^ q n ^ "]]") ns
        | Wildcard -> [], []
        | Bind n -> [], [ n, path ]
        | Pliteral Absent -> [ "is.null(" ^ path ^ ")" ], []
        | Pliteral l ->
          [ "isTRUE(" ^ path ^ " == " ^ exp { x with form = Literal l } ^ ")" ], []
        | Tag (n, ps) ->
          let ts, vs = parts path ps in
          ("identical(attr(" ^ path ^ ", 'tag'), " ^ q n ^ ")") :: ts, vs
        | Ptuple ps -> parts path ps
      and parts path ps =
        let ts, vs =
          List.mapi (fun i p -> pattern (Printf.sprintf "%s[[%d]]" path (i + 1)) p) ps
          |> List.split
        in
        List.concat ts, List.concat vs
      in
      let branch (p, b) rest =
        let tests, vars = pattern id p in
        if tests = []
        then emit (vars @ env) b
        else
          "if ("
          ^ String.concat " && " tests
          ^ ") "
          ^ emit (vars @ env) b
          ^ " else "
          ^ rest
      in
      call
        ("function("
         ^ id
         ^ ") { force("
         ^ id
         ^ "); "
         ^ List.fold_right branch bs "stop('Non-exhaustive match')"
         ^ " }")
        [ exp subject ]
    | _ -> raise (Infer.Error (x.span, "Unsupported R scalar expression"))
  in
  let _, _, lines =
    List.fold_left
      (fun (types, env, lines) d ->
         let types, typ = Infer.declaration types d in
         match d with
         | Alias _ -> types, env, lines
         | Type (_, _, cs) ->
           List.fold_left
             (fun (ts, env, ls) (n, fs) ->
                let vs = List.map (fun _ -> fresh ()) fs in
                let v = "rp_variant(" ^ String.concat ", " (q n :: vs) ^ ")" in
                let v =
                  if vs = [] then v else "function(" ^ String.concat ", " vs ^ ") " ^ v
                in
                let tag = fresh () in
                ts, (n, tag) :: env, (tag ^ " <- " ^ v) :: ls)
             (types, env, lines)
             cs
         | Let (n, ps, x) ->
           let v =
             match ps with
             | None -> x
             | Some ps -> { x with form = Lambda (ps, x) }
           in
           let id = fresh () in
           types, (n, id) :: env, (id ^ " <- " ^ emit env v) :: lines
         | Evaluate x ->
           let shape = q (Yojson.Basic.to_string (Infer.descriptor typ)) in
           ( types
           , env
           , ("cat(rp_show(" ^ emit env x ^ ", " ^ shape ^ "), \"\\n\", sep = \"\")")
             :: lines )
         | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
         | Contract _ -> failwith "Expected scalar program")
      (Infer.empty, [], [])
      ds
  in
  Adapters.r ^ String.concat "\n" (List.rev lines) ^ "\n"
;;
