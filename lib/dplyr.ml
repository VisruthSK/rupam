open Shape
module R = R_syntax

let all_of names = R.call "all_of" [ R.strings names ]

let apply (row : Prelude.row) args =
  let call f = R.call f args in
  match row, args with
  | Abs, _ -> call "abs"
  | Sign, [ x ] ->
    R.qualified "bit64" "as.integer64" [ R.Positional (R.call "sign" [ x ]) ]
  | Sqrt, _ -> call "sqrt"
  | Exp, _ -> call "exp"
  | Log, _ -> call "log"
  | Log2, _ -> call "log2"
  | Log10, _ -> call "log10"
  | Floor, [ x ] -> R.call "floor" [ R.call "as.double" [ x ] ]
  | Ceiling, [ x ] -> R.call "ceiling" [ R.call "as.double" [ x ] ]
  | Trunc, [ x ] -> R.call "trunc" [ R.call "as.double" [ x ] ]
  | Round, [ x ] -> R.call "round" [ R.call "as.double" [ x ] ]
  | Pmin, _ -> call "pmin"
  | Pmax, _ -> call "pmax"
  | Present, [ x ] -> R.Not (R.call "is.na" [ x ])
  | Absent, _ -> call "is.na"
  | Length, [ x ] ->
    R.qualified
      "bit64"
      "as.integer64"
      [ R.Positional
          (R.Call (R.Name "nchar", [ R.Positional x; R.Named ("type", R.String "chars") ]))
      ]
  | Lower, _ -> call "tolower"
  | Upper, _ -> call "toupper"
  | Trim, _ -> call "trimws"
  | Starts, [ x; p ] -> R.call "startsWith" [ x; p ]
  | Ends, [ x; p ] -> R.call "endsWith" [ x; p ]
  | Detect, [ x; p ] ->
    R.qualified
      "dplyr"
      "if_else"
      [ R.Positional (R.call "is.na" [ x ])
      ; R.Positional R.Na
      ; R.Positional
          (R.Call
             ( R.Name "grepl"
             , [ R.Positional p; R.Positional x; R.Named ("perl", R.Logical true) ] ))
      ]
  | _ -> raise (Error ("Wrong arguments to " ^ Prelude.name (Row row)))
;;

let lower ~fields (plan : Table.t) base =
  let names = Array.make plan.next "" in
  List.iteri (fun i n -> names.(i) <- n) fields;
  let next = ref (List.length fields) in
  let bind n =
    names.(!next) <- n;
    incr next
  in
  let rec emit e =
    match e.node with
    | Input i -> R.Dollar (R.Name ".data", names.(i))
    | Broadcast (_, e) -> emit e
    | Constant (`Int i) ->
      R.qualified "bit64" "as.integer64" [ R.Positional (R.String (Int64.to_string i)) ]
    | Constant (`Float f) -> R.Number (R.number f)
    | Constant (`Bool b) -> R.Logical b
    | Constant (`String s) -> R.String s
    | Null ->
      (match e.typ.primitive with
       | Int64 -> R.Namespaced ("bit64", "NA_integer64_")
       | Float64 -> R.Name "NA_real_"
       | Bool -> R.Na
       | String -> R.Name "NA_character_")
    | Choose (c, t, f) ->
      R.qualified
        "dplyr"
        "if_else"
        [ R.Positional (emit c); R.Positional (emit t); R.Positional (emit f) ]
    | Coalesce (a, b) ->
      R.qualified "dplyr" "coalesce" [ R.Positional (emit a); R.Positional (emit b) ]
    | Polars_sqrt _ ->
      raise (Error "R target cannot execute the Python capability polars.sqrt")
    | Apply (row, args) -> apply row (List.map emit args)
    | Convert a -> R.call "as.double" [ emit a ]
    | Count -> R.qualified "bit64" "as.integer64" [ R.Positional (R.qualified "dplyr" "n" []) ]
    | Text parts ->
      let shown =
        List.filter_map
          (function
            | Shape.Shown (x, _) when x.typ.nullable -> Some (emit x)
            | _ -> None)
          parts
      in
      let part = function
        | Shape.Fixed s -> R.String s
        | Shown (x, format) ->
          let e = emit x in
          (match format, x.typ.primitive with
           | Some spec, _ -> R.call "rp_format_number" [ e; R.String spec ]
           | None, String -> e
           | None, Int64 -> R.call "as.character" [ e ]
           | None, Float64 -> R.call "rp_float" [ e ]
           | None, Bool ->
             R.qualified
               "dplyr"
               "if_else"
               [ R.Positional e; R.Positional (R.String "TRUE"); R.Positional (R.String "FALSE") ])
      in
      let joined = R.call "paste0" (List.map part parts) in
      (match shown with
       | [] -> joined
       | first :: rest ->
         R.qualified
           "dplyr"
           "if_else"
           [ R.Positional
               (List.fold_left
                  (fun a b -> R.Binary (R.Either, a, R.call "is.na" [ b ]))
                  (R.call "is.na" [ first ])
                  rest)
           ; R.Positional (R.Name "NA_character_")
           ; R.Positional joined
           ])
    | Member (a, items) ->
      let a = emit a in
      R.qualified
        "dplyr"
        "if_else"
        [ R.Positional (R.call "is.na" [ a ])
        ; R.Positional R.Na
        ; R.Positional (R.Binary (R.In_set, a, R.call "c" (List.map emit items)))
        ]
    | Unary (Not, a) -> R.Not (emit a)
    | Unary (Negate, a) -> R.Negate (emit a)
    | Required a -> R.call "rp_present" [ emit a ]
    | Binary (op, a, b) ->
      R.Binary
        ( (match op with
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
           | And -> R.Both
           | Or -> R.Either)
        , emit a
        , emit b )
    | Reduce (r, policy, a) ->
      let source = emit a in
      let x =
        match policy with
        | Fail -> R.call "rp_present" [ source ]
        | Skip | Propagate -> source
      in
      let guard value =
        match policy with
        | Propagate ->
          R.qualified
            "dplyr"
            "if_else"
            [ R.Positional (R.call "anyNA" [ source ]); R.Positional R.Na; R.Positional value ]
        | Skip | Fail -> value
      in
      guard
      @@
      let present f = R.Call (f, [ R.Positional x; R.Named ("na.rm", R.Logical true) ]) in
      let unless_empty value =
        R.qualified
          "dplyr"
          "if_else"
          [ R.Positional (R.call "all" [ R.call "is.na" [ x ] ])
          ; R.Positional R.Na
          ; R.Positional value
          ]
      in
      (match r with
       | Mean ->
         R.qualified
           "dplyr"
           "na_if"
           [ R.Positional (present (R.Name "mean")); R.Positional (R.Name "NaN") ]
       | Median -> present (R.Namespaced ("stats", "median"))
       | Var -> present (R.Namespaced ("stats", "var"))
       | Sd -> present (R.Namespaced ("stats", "sd"))
       | Sum -> unless_empty (present (R.Name "sum"))
       | Min -> unless_empty (present (R.Name "min"))
       | Max -> unless_empty (present (R.Name "max"))
       | Distinct ->
         R.qualified
           "bit64"
           "as.integer64"
           [ R.Positional
               (R.qualified
                  "dplyr"
                  "n_distinct"
                  [ R.Positional x; R.Named ("na.rm", R.Logical true) ])
           ]
       | Count -> assert false)
  in
  let checked x =
    if Shape.nonfinite x || Shape.overflows x
    then R.call "rp_checked" [ emit x ]
    else emit x
  in
  let stage = function
    | Table.Pivot p ->
      let ids = List.map fst p.ids in
      List.iter bind (ids @ if p.wider then p.labels else [ p.key; p.value ]);
      if p.wider
      then
        R.call
          "rp_wider"
          [ R.strings ids; R.String p.key; R.String p.value; R.strings p.labels ]
      else
        R.qualified
          "tidyr"
          "pivot_longer"
          [ R.Positional (all_of p.labels)
          ; R.Named ("names_to", R.String p.key)
          ; R.Named ("values_to", R.String p.value)
          ]
    | Table.Rename changes ->
      List.iter (fun (_, b, i) -> names.(i) <- b) changes;
      R.Call (R.Name "rename", List.map (fun (a, b, _) -> R.Named (b, R.Name a)) changes)
    | Window (n, x, keys) ->
      let value = checked x in
      bind n;
      R.Call
        ( R.Name "mutate"
        , [ R.Named (n, value); R.Named (".by", all_of (List.map fst keys)) ] )
    | Table.Take n ->
      R.Call (R.Name "slice_head", [ R.Named ("n", R.Number (string_of_int n)) ])
    | Distinct keys ->
      R.Call
        ( R.Name "distinct"
        , List.map (fun k -> R.Positional (emit k)) keys
          @ [ R.Named (".keep_all", R.Logical true) ] )
    | Filter x -> R.call "filter" [ emit x ]
    | Assert_key columns -> R.call "rp_assert_key" [ R.strings columns ]
    | Select ns -> R.call "select" [ all_of ns ]
    | Bind (n, x) ->
      let value = checked x in
      bind n;
      R.Call (R.Name "mutate", [ R.Named (n, value) ])
    | Arrange keys ->
      R.call
        "arrange"
        (List.map (fun (x, d) -> if d then R.call "desc" [ emit x ] else emit x) keys)
    | Summarize (keys, values) ->
      let key_names = List.map fst keys in
      let measures =
        List.map
          (fun (n, x) ->
             R.Named
               ( n
               , match x with
                 | None ->
                   R.qualified "bit64" "as.integer64" [ R.Positional (R.call "n" []) ]
                 | Some x -> checked x ))
          values
      in
      List.iter bind (key_names @ List.map fst values);
      R.Call
        ( R.Name "summarise"
        , measures @ if keys = [] then [] else [ R.Named (".by", all_of key_names) ] )
  in
  List.fold_left
    (fun input step ->
       let e = stage step in
       R.pipe
         input
         (match List.find_opt (fun (s, _) -> s == step) plan.locations with
          | Some (_, span) -> R.Located (span, e)
          | None -> e))
    base
    (List.rev plan.steps)
;;

let rec table d =
  let open Analysis_program in
  let base =
    match d.origin with
    | File source | Contracted { source; _ } ->
      R.call
        "rp_read"
        [ R.String source.path
        ; R.String source.digest
        ; R.String (Data_file.format_name source.format)
        ; R.Call
            ( R.Name "c"
            , List.map (fun (n, t, _) -> R.Named (n, R.String (kind t))) source.columns )
        ]
    | Joined { left; right; key; kind; relationship; covered } ->
      let left =
        if covered
        then
          R.call
            "rp_covered"
            [ table left; table right; R.String key; R.String (join_name kind) ]
        else table left
      in
      R.Call
        ( R.Name (join_name kind ^ "_join")
        , [ R.Positional left
          ; R.Positional (table right)
          ; R.Named ("by", R.String key)
          ; R.Named ("na_matches", R.String "never")
          ]
          @
          match relationship with
          | None -> []
          | Some r ->
            [ R.Named
                ( "relationship"
                , R.String
                    (match r with
                     | One_to_one -> "one-to-one"
                     | Many_to_one -> "many-to-one"
                     | One_to_many -> "one-to-many"
                     | Many_to_many -> "many-to-many") )
            ] )
  in
  lower ~fields:d.fields d.plan base
;;

let compile ?strict ~directory declarations =
  R.Verbatim Adapters.dplyr
  :: List.map
       (fun (d, output) ->
          let t = table (Analysis_program.public_view d) in
          R.Expression
            (match (output : Analysis_program.output) with
             | Write path -> R.pipe t (R.call "rp_write" [ R.String path ])
             | Print -> R.call "print" [ R.call "as_tibble" [ t ] ]))
       (Analysis_program.compile ?strict ~directory declarations)
;;

let preview ~runtime d ~offset ~limit =
  let d = Analysis_program.public_view d in
  let path = Filename.temp_file "rupam" ".parquet" in
  let total =
    Worker.evaluate
      runtime
      Runtime.R
      (fst
         (R.program
            [ R.Verbatim Adapters.dplyr
            ; R.Expression
                (R.call
                   "cat"
                   [ R.call
                       "rp_preview"
                       [ table d
                       ; R.Number (string_of_int offset)
                       ; R.Number (string_of_int limit)
                       ; R.String path
                       ]
                   ])
            ]))
  in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () -> Analysis_program.page d ~path ~total ~offset)
;;

module Values = struct
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
      integer (R.Call (R.Name "nchar", [ R.Positional a; R.Named ("type", R.String "chars") ]))
    | Row Lower, [ a ] -> R.call "tolower" [ a ]
    | Row Upper, [ a ] -> R.call "toupper" [ a ]
    | Row Trim, [ a ] -> R.call "trimws" [ a ]
    | Row Starts, [ a; b ] -> R.call "startsWith" [ a; b ]
    | Row Ends, [ a; b ] -> R.call "endsWith" [ a; b ]
    | Row Detect, [ a; b ] ->
      R.Call (R.Name "grepl", [ R.Positional b; R.Positional a; R.Named ("perl", R.Logical true) ])
    | Reduction _, _ -> failwith (Prelude.name builtin ^ " works on table columns")
    | _ -> failwith ("Wrong arguments to " ^ Prelude.name builtin)
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
         | None, None -> raise (Infer.Error (x.span, "Unknown name: " ^ n)))
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
          | Pliteral l -> [ R.call "identical" [ path; exp { x with form = Literal l } ] ], []
          | Tag (n, ps) ->
            let ts, vs = parts path ps in
            ( R.call "identical" [ R.call "attr" [ path; R.String "tag" ]; R.String n ] :: ts
            , vs )
          | Ptuple ps -> parts path ps
          | Prange { low; high; inclusive } ->
            ( Option.to_list (Option.map (fun l -> R.Binary (R.Less_equal, literal l, path)) low)
              @ Option.to_list
                  (Option.map
                     (fun h ->
                        R.Binary ((if inclusive then R.Less_equal else R.Less), path, literal h))
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
              , List.fold_right branch arms (R.call "stop" [ R.String "Non-exhaustive match" ]) )
          , [ R.Positional (exp subject) ] )
      | Range _ | Formula _ | Name ((Column | Lexical), _) ->
        raise (Infer.Error (x.span, "Expression has no R lowering"))
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
                  let value = R.call "rp_variant" (R.String n :: List.map (fun v -> R.Name v) vs) in
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
                    , [ R.Positional (R.call "rp_show" [ R.call "rp_checked" [ emit env x ]; shape ])
                      ; R.Positional (R.String "\n")
                      ; R.Named ("sep", R.String "")
                      ] ))
               :: statements )
           | Alias _ | Setting _ -> types, env, statements
           | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table program")
           | Contract _ -> failwith "Contracts require a table program")
        (Infer.empty, [], [])
        declarations
    in
    R.Verbatim Adapters.r :: List.rev statements
  ;;
end

let program ~strict ~directory declarations =
  R.program
    (if Analysis_program.tabular declarations
     then compile ~strict ~directory declarations
     else Values.statements declarations)
;;

let value declarations = fst (R.program (Values.statements declarations))
let table d = R.render (table d)
