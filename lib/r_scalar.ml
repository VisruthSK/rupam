open Syntax

let native fresh (builtin : Prelude.builtin) args =
  let number a = "as.numeric(" ^ a ^ ")" in
  match builtin, args with
  | Coalesce, [ a; fallback ] ->
    let t = fresh () in
    "local({ "
    ^ t
    ^ " <- "
    ^ a
    ^ "; if (is.null("
    ^ t
    ^ ")) "
    ^ fallback
    ^ " else "
    ^ t
    ^ " })"
  | Row Abs, [ a ] -> "abs(" ^ a ^ ")"
  | Row Sign, [ a ] -> "bit64::as.integer64(sign(" ^ number a ^ "))"
  | Row Sqrt, [ a ] -> "sqrt(" ^ number a ^ ")"
  | Row Exp, [ a ] -> "exp(" ^ number a ^ ")"
  | Row Log, [ a ] -> "log(" ^ number a ^ ")"
  | Row Log2, [ a ] -> "log2(" ^ number a ^ ")"
  | Row Log10, [ a ] -> "log10(" ^ number a ^ ")"
  | Row Floor, [ a ] -> "floor(" ^ number a ^ ")"
  | Row Ceiling, [ a ] -> "ceiling(" ^ number a ^ ")"
  | Row Trunc, [ a ] -> "trunc(" ^ number a ^ ")"
  | Row Round, [ a ] -> "round(" ^ number a ^ ")"
  | Row Pmin, [ a; b ] -> "min(" ^ a ^ ", " ^ b ^ ")"
  | Row Pmax, [ a; b ] -> "max(" ^ a ^ ", " ^ b ^ ")"
  | Row Present, [ a ] -> "!is.null(" ^ a ^ ")"
  | Row Absent, [ a ] -> "is.null(" ^ a ^ ")"
  | Row Length, [ a ] -> "bit64::as.integer64(nchar(" ^ a ^ ", type = \"chars\"))"
  | Row Lower, [ a ] -> "tolower(" ^ a ^ ")"
  | Row Upper, [ a ] -> "toupper(" ^ a ^ ")"
  | Row Trim, [ a ] -> "trimws(" ^ a ^ ")"
  | Row Starts, [ a; b ] -> "startsWith(" ^ a ^ ", " ^ b ^ ")"
  | Row Ends, [ a; b ] -> "endsWith(" ^ a ^ ", " ^ b ^ ")"
  | Row Detect, [ a; b ] -> "grepl(" ^ b ^ ", " ^ a ^ ", perl = TRUE)"
  | Reduction _, _ -> failwith (Prelude.name builtin ^ " works on table columns")
  | _ -> failwith ("Wrong arguments to " ^ Prelude.name builtin)
;;

let arity builtin =
  match snd (Infer.signature builtin) with
  | Infer.Arrow (ps, _) -> List.length ps
  | _ -> 0
;;

let compile ?(annotate = Source_map.plain) ds =
  let next = ref 0 in
  let fresh () =
    incr next;
    "v" ^ string_of_int !next
  in
  let q = Polars.quoted in
  let rec emit env x =
    let code = render env x in
    annotate x.span (if Infer.converted x then "as.numeric(" ^ code ^ ")" else code)
  and render env x =
    let exp = emit env in
    let call f args = "(" ^ f ^ ")(" ^ String.concat ", " args ^ ")" in
    let callee f args =
      match f.form with
      | Name (Bare, n) when not (List.mem_assoc n env) ->
        (match Prelude.find n with
         | Some b -> native fresh b args
         | None -> call (exp f) args)
      | _ -> call (exp f) args
    in
    let operate operands build =
      match Infer.lifted x with
      | None -> build (List.map exp operands)
      | Some flags ->
        let parts =
          List.map2
            (fun flag operand ->
               if flag
               then (
                 let v = fresh () in
                 Some (v, exp operand), v)
               else None, exp operand)
            flags
            operands
        in
        let bound = List.filter_map fst parts in
        "local({ "
        ^ String.concat "" (List.map (fun (v, e) -> v ^ " <- " ^ e ^ "; ") bound)
        ^ "if ("
        ^ String.concat " || " (List.map (fun (v, _) -> "is.null(" ^ v ^ ")") bound)
        ^ ") NULL else "
        ^ build (List.map snd parts)
        ^ " })"
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
    | Name (Bare, n) ->
      (match List.assoc_opt n env, Prelude.find n with
       | Some id, _ -> id
       | None, Some b ->
         let ps = List.init (arity b) (fun _ -> fresh ()) in
         "function(" ^ String.concat ", " ps ^ ") " ^ native fresh b ps
       | None, None -> raise (Infer.Error (x.span, "Unknown name: " ^ n)))
    | Literal Absent -> "NULL"
    | Literal (Bool true) -> "TRUE"
    | Literal (Bool false) -> "FALSE"
    | Literal (Text s) -> q s
    | Literal (Float s) -> s
    | Literal (Int s) when Infer.promoted x -> R_syntax.number (Shape.exact_float s)
    | Literal (Int s) when Infer.unsettled x -> s
    | Literal (Int s) -> "bit64::as.integer64(" ^ q s ^ ")"
    | Annotated (v, _) -> exp v
    | Tuple vs | List vs -> call "list" (List.map exp vs)
    | Record fs ->
      "list(" ^ String.concat ", " (List.map (fun (n, v) -> q n ^ " = " ^ exp v) fs) ^ ")"
    | Field (v, n) -> "(" ^ exp v ^ ")[[" ^ q n ^ "]]"
    | Lambda (ps, v) -> lambda ps v
    | Local (n, v, b) ->
      let id = fresh () in
      "local({ " ^ id ^ " <- " ^ exp v ^ "; " ^ emit ((n, id) :: env) b ^ " })"
    | If (c, a, b) -> "(if (" ^ exp c ^ ") " ^ exp a ^ " else " ^ exp b ^ ")"
    | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) -> exp v
    | Call (f, vs) -> operate (List.map snd vs) (callee f)
    | Unary (op, v) ->
      operate [ v ] (function
        | [ v ] ->
          (match op with
           | Not -> "(!" ^ v ^ ")"
           | Negate -> "(-" ^ v ^ ")")
        | _ -> assert false)
    | Pipe (v, { form = Call (g, vs); _ }) -> operate (v :: List.map snd vs) (callee g)
    | Pipe (v, f) -> operate [ v ] (callee f)
    | Member (v, set) ->
      operate [ v; set ] (function
        | [ v; set ] ->
          let item = fresh () in
          "any(vapply("
          ^ set
          ^ ", function("
          ^ item
          ^ ") identical("
          ^ item
          ^ ", "
          ^ v
          ^ "), TRUE))"
        | _ -> assert false)
    | Binary (op, a, b) ->
      operate [ a; b ] (function
        | [ a; b ] ->
          let symbol =
            match op with
            | And -> "&&"
            | Or -> "||"
            | Mod -> "%%"
            | op -> binary_symbol op
          in
          let v = "(" ^ a ^ " " ^ symbol ^ " " ^ b ^ ")" in
          v
        | _ -> assert false)
    | Match (subject, bs) ->
      let id = fresh () in
      let rec pattern path = function
        | Precord ns -> [], List.map (fun n -> n, path ^ "[[" ^ q n ^ "]]") ns
        | Wildcard -> [], []
        | Bind n -> [], [ n, path ]
        | Pliteral Absent | Tag ("None", []) -> [ "is.null(" ^ path ^ ")" ], []
        | Tag ("Some", [ p ]) ->
          let ts, vs = pattern path p in
          ("!is.null(" ^ path ^ ")") :: ts, vs
        | Pliteral l ->
          [ "identical(" ^ path ^ ", " ^ exp { x with form = Literal l } ^ ")" ], []
        | Tag (n, ps) ->
          let ts, vs = parts path ps in
          ("identical(attr(" ^ path ^ ", 'tag'), " ^ q n ^ ")") :: ts, vs
        | Ptuple ps -> parts path ps
        | Prange { low; high; inclusive } ->
          ( Option.to_list (Option.map (fun l -> literal_text l ^ " <= " ^ path) low)
            @ Option.to_list
                (Option.map
                   (fun h ->
                      path ^ (if inclusive then " <= " else " < ") ^ literal_text h)
                   high)
          , [] )
        | Pas (n, p) ->
          let ts, vs = pattern path p in
          ts, (n, path) :: vs
        | Palt alternatives ->
          let each = List.map (pattern path) alternatives in
          let vs = snd (List.hd each) in
          if List.exists (fun (ts, _) -> ts = []) each
          then [], vs
          else
            ( [ "("
                ^ String.concat
                    " || "
                    (List.map (fun (ts, _) -> "(" ^ String.concat " && " ts ^ ")") each)
                ^ ")"
              ]
            , vs )
      and parts path ps =
        let ts, vs =
          List.mapi (fun i p -> pattern (Printf.sprintf "%s[[%d]]" path (i + 1)) p) ps
          |> List.split
        in
        List.concat ts, List.concat vs
      in
      let branch arm rest =
        let tests, vars = pattern id arm.pattern in
        let inner = vars @ env in
        let tests = tests @ Option.to_list (Option.map (emit inner) arm.guard) in
        let b = arm.body in
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
           let value = if ps = None then "rp_checked(" ^ emit env v ^ ")" else emit env v in
           types, (n, id) :: env, (id ^ " <- " ^ value) :: lines
         | Evaluate x ->
           let shape = q (Yojson.Basic.to_string (Infer.descriptor typ)) in
           ( types
           , env
           , ("cat(rp_show(rp_checked(" ^ emit env x ^ "), " ^ shape ^ "), \"\\n\", sep = \"\")")
             :: lines )
         | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
         | Contract _ -> failwith "Expected scalar program"
         | Setting _ -> types, env, lines)
      (Infer.empty, [], [])
      ds
  in
  Adapters.r ^ String.concat "\n" (List.rev lines) ^ "\n"
;;
