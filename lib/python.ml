open Syntax

let native fresh (builtin : Prelude.builtin) args =
  match builtin, args with
  | Coalesce, [ a; fallback ] ->
    let t = fresh () in
    "(" ^ fallback ^ " if (" ^ t ^ " := " ^ a ^ ") is None else " ^ t ^ ")"
  | Row Abs, [ a ] -> "abs(" ^ a ^ ")"
  | Row Sign, [ a ] -> "(lambda v: (v > 0) - (v < 0))(" ^ a ^ ")"
  | Row Sqrt, [ a ] -> "math.sqrt(" ^ a ^ ")"
  | Row Exp, [ a ] -> "math.exp(" ^ a ^ ")"
  | Row Log, [ a ] -> "math.log(" ^ a ^ ")"
  | Row Log2, [ a ] -> "math.log2(" ^ a ^ ")"
  | Row Log10, [ a ] -> "math.log10(" ^ a ^ ")"
  | Row Floor, [ a ] -> "float(math.floor(" ^ a ^ "))"
  | Row Ceiling, [ a ] -> "float(math.ceil(" ^ a ^ "))"
  | Row Trunc, [ a ] -> "float(math.trunc(" ^ a ^ "))"
  | Row Round, [ a ] -> "float(round(" ^ a ^ "))"
  | Row Pmin, [ a; b ] -> "min(" ^ a ^ ", " ^ b ^ ")"
  | Row Pmax, [ a; b ] -> "max(" ^ a ^ ", " ^ b ^ ")"
  | Row Present, [ a ] -> "(" ^ a ^ " is not None)"
  | Row Absent, [ a ] -> "(" ^ a ^ " is None)"
  | Row Length, [ a ] -> "len(" ^ a ^ ")"
  | Row Lower, [ a ] -> "(" ^ a ^ ").lower()"
  | Row Upper, [ a ] -> "(" ^ a ^ ").upper()"
  | Row Trim, [ a ] -> "(" ^ a ^ ").strip()"
  | Row Starts, [ a; b ] -> "(" ^ a ^ ").startswith(" ^ b ^ ")"
  | Row Ends, [ a; b ] -> "(" ^ a ^ ").endswith(" ^ b ^ ")"
  | Row Detect, [ a; b ] -> "(re.search(" ^ b ^ ", " ^ a ^ ") is not None)"
  | Reduction _, _ -> failwith (Prelude.name builtin ^ " works on table columns")
  | _ -> failwith ("Wrong arguments to " ^ Prelude.name builtin)
;;

let arity builtin =
  match snd (Infer.signature builtin) with
  | Infer.Arrow (ps, _) -> List.length ps
  | _ -> 0
;;

let compile ?(annotate = Source_map.plain) program =
  let serial = ref 0 in
  let fresh () =
    incr serial;
    "v" ^ string_of_int !serial
  in
  let error x msg = raise (Infer.Error (x.span, msg)) in
  let rec emit env x = annotate x.span (render env x)
  and render env x =
    let expr = emit env in
    let apply f xs = f ^ "(" ^ String.concat ", " xs ^ ")" in
    let callee f args =
      match f.form with
      | Name (Bare, n) when not (List.mem_assoc n env) ->
        (match Prelude.find n with
         | Some b -> native fresh b args
         | None -> apply (expr f) args)
      | _ -> apply (expr f) args
    in
    let operate operands build =
      match Infer.lifted x with
      | None -> build (List.map expr operands)
      | Some flags ->
        let parts =
          List.map2
            (fun flag operand ->
               if flag
               then (
                 let v = fresh () in
                 Some ("(" ^ v ^ " := " ^ expr operand ^ ") is None"), v)
               else None, expr operand)
            flags
            operands
        in
        "(None if "
        ^ String.concat " or " (List.filter_map fst parts)
        ^ " else "
        ^ build (List.map snd parts)
        ^ ")"
    in
    match x.form with
    | Name (Bare, name) ->
      (match List.assoc_opt name env, Prelude.find name with
       | Some id, _ -> id
       | None, Some b ->
         let ps = List.init (arity b) (fun _ -> fresh ()) in
         "(lambda " ^ String.concat ", " ps ^ ": " ^ native fresh b ps ^ ")"
       | None, None -> error x ("Unknown name: " ^ name))
    | Literal Absent -> "None"
    | Literal (Bool true) -> "True"
    | Literal (Bool false) -> "False"
    | Annotated (v, _) -> expr v
    | Tuple xs -> "(" ^ String.concat ", " (List.map expr xs) ^ ")"
    | List xs -> "[" ^ String.concat ", " (List.map expr xs) ^ "]"
    | Record fs ->
      "{"
      ^ String.concat ", " (List.map (fun (n, v) -> Polars.quoted n ^ ": " ^ expr v) fs)
      ^ "}"
    | Field (v, n) -> "(" ^ expr v ^ ")[" ^ Polars.quoted n ^ "]"
    | Local (n, v, body) ->
      let id = fresh () in
      apply ("(lambda " ^ id ^ ": " ^ emit ((n, id) :: env) body ^ ")") [ expr v ]
    | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) -> expr v
    | Match (subject, branches) ->
      let id = fresh () in
      let rec pattern path = function
        | Precord ns -> [], List.map (fun n -> n, path ^ "[" ^ Polars.quoted n ^ "]") ns
        | Wildcard -> [], []
        | Bind n -> [], [ n, path ]
        | Pliteral Absent | Tag ("None", []) -> [ path ^ " is None" ], []
        | Pliteral l -> [ path ^ " == " ^ expr { x with form = Literal l } ], []
        | Tag ("Some", [ p ]) ->
          let tests, bindings = pattern path p in
          (path ^ " is not None") :: tests, bindings
        | Tag (n, ps) ->
          let tests, bindings = children path 1 ps in
          ( ("isinstance(" ^ path ^ ", tuple)")
            :: (path ^ "[0] == " ^ Polars.quoted n)
            :: tests
          , bindings )
        | Ptuple ps -> children path 0 ps
      and children path offset ps =
        let tests, bindings =
          List.mapi (fun i p -> pattern (Printf.sprintf "%s[%d]" path (i + offset)) p) ps
          |> List.split
        in
        List.concat tests, List.concat bindings
      in
      let branch (p, body) rest =
        let tests, bindings = pattern id p in
        let value = emit (bindings @ env) body in
        match tests with
        | [] -> value
        | _ -> "(" ^ value ^ " if " ^ String.concat " and " tests ^ " else " ^ rest ^ ")"
      in
      apply
        ("(lambda " ^ id ^ ": " ^ List.fold_right branch branches "None" ^ ")")
        [ expr subject ]
    | Literal (Text text) -> Polars.quoted text
    | Literal (Float s) -> s
    | Literal (Int s) when Infer.promoted x -> Python_syntax.float (Shape.exact_float s)
    | Literal (Int s) -> s
    | Call (f, xs) -> operate (List.map snd xs) (callee f)
    | Lambda (ps, body) ->
      let names = List.map (fun p -> p, fresh ()) ps in
      "(lambda "
      ^ String.concat ", " (List.map snd names)
      ^ ": "
      ^ emit (names @ env) body
      ^ ")"
    | If (c, a, b) -> "(" ^ expr a ^ " if " ^ expr c ^ " else " ^ expr b ^ ")"
    | Pipe (a, { form = Call (g, xs); _ }) -> operate (a :: List.map snd xs) (callee g)
    | Pipe (a, f) -> operate [ a ] (callee f)
    | Binary (op, a, b) ->
      operate [ a; b ] (function
        | [ a; b ] ->
          let symbol =
            match op with
            | Power -> "**"
            | op -> binary_symbol op
          in
          let v = "(" ^ a ^ " " ^ symbol ^ " " ^ b ^ ")" in
          v
        | _ -> assert false)
    | Unary (op, v) ->
      operate [ v ] (function
        | [ v ] ->
          (match op with
           | Negate -> "(-" ^ v ^ ")"
           | Not -> "(not " ^ v ^ ")")
        | _ -> assert false)
    | _ -> error x "Expression has no supported Python lowering"
  in
  let _, _, lines =
    List.fold_left
      (fun (types, env, lines) d ->
         let types, typ = Infer.declaration types d in
         match d with
         | Let (n, ps, body) ->
           let x =
             match ps with
             | None -> body
             | Some ps -> { body with form = Lambda (ps, body) }
           in
           let id = fresh () in
           types, (n, id) :: env, (id ^ " = " ^ emit env x) :: lines
         | Evaluate x ->
           let shape = Polars.quoted (Yojson.Basic.to_string (Infer.descriptor typ)) in
           types, env, ("print(rp_show(" ^ emit env x ^ ", " ^ shape ^ "))") :: lines
         | Alias _ -> types, env, lines
         | Type (_, _, constructors) ->
           List.fold_left
             (fun (types, env, lines) (n, fs) ->
                let id = fresh ()
                and ps = List.map (fun _ -> fresh ()) fs in
                let tuple = "(" ^ Polars.quoted n ^ ", " ^ String.concat ", " ps ^ ")" in
                let value =
                  if ps = []
                  then tuple
                  else "lambda " ^ String.concat ", " ps ^ ": " ^ tuple
                in
                types, (n, id) :: env, (id ^ " = " ^ value) :: lines)
             (types, env, lines)
             constructors
         | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
         | Contract _ -> failwith "Contract lowering is unavailable")
      (Infer.empty, [], [])
      program
  in
  String.concat "\n" (Adapters.python :: List.rev lines) ^ "\n"
;;
