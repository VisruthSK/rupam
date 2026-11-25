module R = R_syntax
open Syntax

let number a = R.call "as.numeric" [ a ]
let integer a = R.qualified "bit64" "as.integer64" [ R.Positional a ]
let null a = R.call "is.null" [ a ]

let native fresh (builtin : Prelude.builtin) args =
  match builtin, args with
  | Coalesce, [ a; fallback ] ->
    let t = fresh () in
    R.Block ([ t, a ], R.If (null (R.Name t), fallback, R.Name t))
  | Row Abs, [ a ] -> R.call "abs" [ a ]
  | Row Sign, [ a ] -> integer (R.call "sign" [ number a ])
  | Row Sqrt, [ a ] -> R.call "sqrt" [ number a ]
  | Row Exp, [ a ] -> R.call "exp" [ number a ]
  | Row Log, [ a ] -> R.call "log" [ number a ]
  | Row Log2, [ a ] -> R.call "log2" [ number a ]
  | Row Log10, [ a ] -> R.call "log10" [ number a ]
  | Row Floor, [ a ] -> R.call "floor" [ number a ]
  | Row Ceiling, [ a ] -> R.call "ceiling" [ number a ]
  | Row Trunc, [ a ] -> R.call "trunc" [ number a ]
  | Row Round, [ a ] -> R.call "round" [ number a ]
  | Row Pmin, [ a; b ] -> R.call "min" [ a; b ]
  | Row Pmax, [ a; b ] -> R.call "max" [ a; b ]
  | Row Present, [ a ] -> R.Not (null a)
  | Row Absent, [ a ] -> null a
  | Row Length, [ a ] ->
    integer
      (R.Call (R.Name "nchar", [ R.Positional a; R.Named ("type", R.String "chars") ]))
  | Row Lower, [ a ] -> R.call "tolower" [ a ]
  | Row Upper, [ a ] -> R.call "toupper" [ a ]
  | Row Trim, [ a ] -> R.call "trimws" [ a ]
  | Row Starts, [ a; b ] -> R.call "startsWith" [ a; b ]
  | Row Ends, [ a; b ] -> R.call "endsWith" [ a; b ]
  | Row Detect, [ a; b ] ->
    R.Call
      ( R.Name "grepl"
      , [ R.Positional b; R.Positional a; R.Named ("perl", R.Logical true) ] )
  | Reduction _, _ ->
    Diagnostic.fail Wrong_arguments (Prelude.name builtin ^ " works on table columns")
  | _ -> Diagnostic.fail Wrong_arguments ("Wrong arguments to " ^ Prelude.name builtin)
;;

let arity builtin =
  match snd (Infer.signature builtin) with
  | Infer.Arrow (ps, _) -> List.length ps
  | _ -> 0
;;

let operator = function
  | Add -> R.Add
  | Sub -> R.Subtract
  | Mul -> R.Multiply
  | Div -> R.Divide
  | Mod -> R.Modulo
  | Power -> R.Power
  | Less -> R.Less
  | Greater -> R.Greater
  | Less_equal -> R.Less_equal
  | Greater_equal -> R.Greater_equal
  | Equal -> R.Equal
  | Not_equal -> R.Not_equal
  | And -> R.Both_scalar
  | Or -> R.Either_scalar
;;

let literal = function
  | Int s -> R.Number s
  | Float s -> R.Number s
  | Text s -> R.String s
  | Bool b -> R.Logical b
  | Absent -> R.Null
;;

let all = function
  | [] -> None
  | t :: rest -> Some (List.fold_left (fun a b -> R.Binary (R.Both_scalar, a, b)) t rest)
;;

let guarded bound body =
  match bound with
  | [] -> body
  | first :: rest ->
    R.Block
      ( bound
      , R.If
          ( List.fold_left
              (fun a (v, _) -> R.Binary (R.Either_scalar, a, null (R.Name v)))
              (null (R.Name (fst first)))
              rest
          , R.Null
          , body ) )
;;

let statements declarations =
  let next = ref 0 in
  let fresh () =
    incr next;
    "v" ^ string_of_int !next
  in
  let rec emit env x =
    let e = render env x in
    R.Located (x.span, if Infer.converted x then number e else e)
  and render env x =
    let exp = emit env in
    let apply f args = R.Call (f, List.map (fun a -> R.Positional a) args) in
    let callee f args =
      match f.form with
      | Name (Bare, n) when not (List.mem_assoc n env) ->
        (match Prelude.find n with
         | Some b -> native fresh b args
         | None -> apply (exp f) args)
      | _ -> apply (exp f) args
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
                 Some (v, exp operand), R.Name v)
               else None, exp operand)
            flags
            operands
        in
        guarded (List.filter_map fst parts) (build (List.map snd parts))
    in
    let lambda ps body =
      let vs = List.map (fun n -> n, fresh ()) ps in
      R.Function (List.map snd vs, emit (vs @ env) body)
    in
    match x.form with
    | Name (Bare, n) ->
      (match List.assoc_opt n env, Prelude.find n with
       | Some id, _ -> R.Name id
       | None, Some b ->
         let ps = List.init (arity b) (fun _ -> fresh ()) in
         R.Function (ps, native fresh b (List.map (fun p -> R.Name p) ps))
       | None, None -> Diagnostic.fail ~span:x.span Unknown_name ("Unknown name: " ^ n))
    | Literal (Int s) when Infer.promoted x -> R.Number (R.number (Shape.exact_float s))
    | Literal (Int s) when Infer.unsettled x -> R.Number s
    | Literal (Int s) -> integer (R.String s)
    | Literal l -> literal l
    | Annotated (v, _) -> exp v
    | Tuple vs | List vs -> R.call "list" (List.map exp vs)
    | Record fs -> R.Call (R.Name "list", List.map (fun (n, v) -> R.Named (n, exp v)) fs)
    | Field (v, n) -> R.Element (exp v, R.String n)
    | Lambda (ps, v) -> lambda ps v
    | Local (n, v, b) ->
      let id = fresh () in
      R.Block ([ id, exp v ], emit ((n, id) :: env) b)
    | If (c, a, b) -> R.If (exp c, exp a, exp b)
    | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) -> exp v
    | Call (f, vs) -> operate (List.map snd vs) (callee f)
    | Pipe (v, { form = Call (g, vs); _ }) -> operate (v :: List.map snd vs) (callee g)
    | Pipe (v, f) -> operate [ v ] (callee f)
    | Unary (op, v) ->
      operate [ v ] (function
        | [ v ] ->
          (match op with
           | Not -> R.Not v
           | Negate -> R.Negate v)
        | _ -> assert false)
    | Binary (op, a, b) ->
      operate [ a; b ] (function
        | [ a; b ] -> R.Binary (operator op, a, b)
        | _ -> assert false)
    | Member (v, set) ->
      operate [ v; set ] (function
        | [ v; set ] ->
          let item = fresh () in
          R.call
            "any"
            [ R.call
                "vapply"
                [ set
                ; R.Function ([ item ], R.call "identical" [ R.Name item; v ])
                ; R.Logical true
                ]
            ]
        | _ -> assert false)
    | Interpolate pieces ->
      let shown = ref (Infer.shown x) in
      let bound = ref [] in
      let part = function
        | Text_piece s -> [ R.String s ]
        | Slot slot ->
          let t, lifted =
            match !shown with
            | s :: rest ->
              shown := rest;
              s
            | [] -> Infer.Var (ref (Infer.Free (0, Any))), false
          in
          let v =
            if lifted
            then (
              let id = fresh () in
              bound := (id, exp slot.value) :: !bound;
              R.Name id)
            else exp slot.value
          in
          let display =
            match slot.format, t with
            | Some spec, _ -> R.call "rp_format_number" [ v; R.String spec ]
            | None, String -> v
            | None, Int -> R.call "as.character" [ v ]
            | None, Float -> R.call "rp_float" [ v ]
            | None, Bool -> R.If (v, R.String "TRUE", R.String "FALSE")
            | None, _ -> R.call "rp_dynamic" [ v ]
          in
          Option.to_list (Option.map (fun e -> R.String e) slot.echo) @ [ display ]
      in
      guarded (List.rev !bound) (R.call "paste0" (List.concat_map part pieces))
    | Match (subject, arms) ->
      let id = fresh () in
      let rec pattern path = function
        | Precord ns -> [], List.map (fun n -> n, R.Element (path, R.String n)) ns
        | Wildcard -> [], []
        | Bind n -> [], [ n, path ]
        | Pliteral Absent | Tag ("None", []) -> [ null path ], []
        | Tag ("Some", [ p ]) ->
          let ts, vs = pattern path p in
          R.Not (null path) :: ts, vs
        | Pliteral l ->
          [ R.call "identical" [ path; exp { x with form = Literal l } ] ], []
        | Tag (n, ps) ->
          let ts, vs = parts path ps in
          ( R.call "identical" [ R.call "attr" [ path; R.String "tag" ]; R.String n ] :: ts
          , vs )
        | Ptuple ps -> parts path ps
        | Prange { low; high; inclusive } ->
          ( Option.to_list
              (Option.map (fun l -> R.Binary (R.Less_equal, literal l, path)) low)
            @ Option.to_list
                (Option.map
                   (fun h ->
                      R.Binary
                        ((if inclusive then R.Less_equal else R.Less), path, literal h))
                   high)
          , [] )
        | Pas (n, p) ->
          let ts, vs = pattern path p in
          ts, (n, path) :: vs
        | Palt alternatives ->
          let each = List.map (pattern path) alternatives in
          let vs = snd (List.hd each) in
          (match List.map (fun (ts, _) -> all ts) each with
           | tests when List.mem None tests -> [], vs
           | t :: rest ->
             ( [ List.fold_left
                   (fun a b -> R.Binary (R.Either_scalar, a, Option.get b))
                   (Option.get t)
                   rest
               ]
             , vs )
           | [] -> [], vs)
      and parts path ps =
        let ts, vs =
          List.mapi
            (fun i p -> pattern (R.Element (path, R.Number (string_of_int (i + 1)))) p)
            ps
          |> List.split
        in
        List.concat ts, List.concat vs
      in
      let branch (arm : arm) rest =
        let tests, bindings = pattern (R.Name id) arm.pattern in
        let names = List.map (fun (n, path) -> n, fresh (), path) bindings in
        let inner = List.map (fun (n, v, _) -> n, v) names @ env in
        let bind body =
          match names with
          | [] -> body
          | _ -> R.Block (List.map (fun (_, v, path) -> v, path) names, body)
        in
        let tests =
          tests @ Option.to_list (Option.map (fun g -> bind (emit inner g)) arm.guard)
        in
        let value = bind (emit inner arm.body) in
        match all tests with
        | None -> value
        | Some test -> R.If (test, value, rest)
      in
      R.Call
        ( R.Function
            ( [ id ]
            , List.fold_right
                branch
                arms
                (R.call "stop" [ R.String "Non-exhaustive match" ]) )
        , [ R.Positional (exp subject) ] )
    | Range _ | Formula _ | Name ((Column | Lexical), _) ->
      Diagnostic.fail ~span:x.span Unsupported_feature "Expression has no R lowering"
  in
  let _, _, statements =
    List.fold_left
      (fun (types, env, statements) d ->
         let types, typ = Infer.declaration types d in
         match d with
         | Type (_, _, cs) ->
           List.fold_left
             (fun (types, env, statements) (n, fs) ->
                let vs = List.map (fun _ -> fresh ()) fs in
                let value =
                  R.call "rp_variant" (R.String n :: List.map (fun v -> R.Name v) vs)
                in
                let value = if vs = [] then value else R.Function (vs, value) in
                let tag = fresh () in
                types, (n, tag) :: env, R.Assign (tag, value) :: statements)
             (types, env, statements)
             cs
         | Let (n, ps, x) ->
           let v =
             match ps with
             | None -> x
             | Some ps -> { x with form = Lambda (ps, x) }
           in
           let id = fresh () in
           let value =
             if ps = None then R.call "rp_checked" [ emit env v ] else emit env v
           in
           types, (n, id) :: env, R.Assign (id, value) :: statements
         | Evaluate x ->
           let shape = R.String (Yojson.Basic.to_string (Infer.descriptor typ)) in
           ( types
           , env
           , R.Expression
               (R.Call
                  ( R.Name "cat"
                  , [ R.Positional
                        (R.call "rp_show" [ R.call "rp_checked" [ emit env x ]; shape ])
                    ; R.Positional (R.String "\n")
                    ; R.Named ("sep", R.String "")
                    ] ))
             :: statements )
         | Alias _ | Setting _ -> types, env, statements
         | Import (n, _) ->
           Diagnostic.fail
             Unsupported_feature
             ("Module " ^ n ^ " requires a table program")
         | Contract _ ->
           Diagnostic.fail Unsupported_feature "Contracts require a table program")
      (Infer.empty, [], [])
      declarations
  in
  R.Verbatim Adapters.r :: List.rev statements
;;

let render declarations = fst (R.program (statements declarations))
