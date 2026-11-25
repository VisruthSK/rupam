open Shape
module P = Python_syntax

let quoted s = Yojson.Basic.to_string (`String s)
let pl name = P.Attribute (P.Name "pl", name)
let col n = P.Call (pl "col", [ P.Positional (P.Str n) ])
let lit e = P.Call (pl "lit", [ P.Positional e ])
let named n e = P.meth e "alias" [ P.Positional (P.Str n) ]
let positional xs = List.map (fun x -> P.Positional x) xs

let choose condition yes no =
  P.meth
    (P.meth (P.Call (pl "when", [ P.Positional condition ])) "then" [ P.Positional yes ])
    "otherwise"
    [ P.Positional no ]
;;

let dtype (t : Shape.typ) =
  pl
    (match t.primitive with
     | Int64 -> "Int64"
     | Float64 -> "Float64"
     | Bool -> "Boolean"
     | String -> "String")
;;

let apply (row : Prelude.row) args =
  let on x name = P.meth x name [] in
  let text x name args = P.meth (P.Attribute (x, "str")) name (positional args) in
  match row, args with
  | Abs, [ x ] -> on x "abs"
  | Sign, [ x ] -> P.meth (on x "sign") "cast" [ P.Positional (pl "Int64") ]
  | Sqrt, [ x ] -> on x "sqrt"
  | Exp, [ x ] -> on x "exp"
  | Log, [ x ] -> on x "log"
  | Log2, [ x ] -> P.meth x "log" [ P.Positional (P.Int "2") ]
  | Log10, [ x ] -> on x "log10"
  | Floor, [ x ] -> on (P.meth x "cast" [ P.Positional (pl "Float64") ]) "floor"
  | Ceiling, [ x ] -> on (P.meth x "cast" [ P.Positional (pl "Float64") ]) "ceil"
  | Trunc, [ x ] ->
    let x = P.meth x "cast" [ P.Positional (pl "Float64") ] in
    choose (P.Binary (P.Greater_equal, x, P.Int "0")) (on x "floor") (on x "ceil")
  | Round, [ x ] ->
    P.meth
      (P.meth x "cast" [ P.Positional (pl "Float64") ])
      "round"
      [ P.Positional (P.Int "0"); P.Keyword ("mode", P.Str "half_to_even") ]
  | Pmin, [ a; b ] ->
    P.meth
      (P.meth
         (P.meth
            (P.Call (pl "when", positional [ P.Binary (P.Less_equal, a, b) ]))
            "then"
            [ P.Positional a ])
         "when"
         [ P.Positional (P.Binary (P.Greater, a, b)) ])
      "then"
      [ P.Positional b ]
  | Pmax, [ a; b ] ->
    P.meth
      (P.meth
         (P.meth
            (P.Call (pl "when", positional [ P.Binary (P.Greater_equal, a, b) ]))
            "then"
            [ P.Positional a ])
         "when"
         [ P.Positional (P.Binary (P.Less, a, b)) ])
      "then"
      [ P.Positional b ]
  | Present, [ x ] -> on x "is_not_null"
  | Absent, [ x ] -> on x "is_null"
  | Length, [ x ] -> P.meth (text x "len_chars" []) "cast" [ P.Positional (pl "Int64") ]
  | Lower, [ x ] -> text x "to_lowercase" []
  | Upper, [ x ] -> text x "to_uppercase" []
  | Trim, [ x ] -> text x "strip_chars" []
  | Starts, [ x; p ] -> text x "starts_with" [ p ]
  | Ends, [ x; p ] -> text x "ends_with" [ p ]
  | Detect, [ x; p ] -> text x "contains" [ p ]
  | _ -> Diagnostic.fail Wrong_arguments ("Wrong arguments to " ^ Prelude.name (Row row))
;;

let wide e = P.meth e "cast" [ P.Positional (pl "Int128") ]

let narrow e =
  P.meth e "cast" [ P.Positional (pl "Int64"); P.Keyword ("strict", P.Bool true) ]
;;

let finite name =
  P.meth
    (P.Call (pl "col", [ P.Positional (P.Str name) ]))
    "map_batches"
    [ P.Positional (P.Name "rp_finite"); P.Keyword ("return_dtype", pl "Float64") ]
;;

let emit_value (x : Shape.expr) =
  match x.node with
  | Constant (`Int i) -> P.Int (Int64.to_string i)
  | Constant (`Float f) -> P.Float (P.float f)
  | Constant (`String s) -> P.Str s
  | Constant (`Bool b) -> P.Bool b
  | Null -> P.Null
  | _ -> Diagnostic.fail Wrong_arguments "%in% needs a list of values"
;;

let lower ~fields (table : Table.t) =
  let symbols = Array.make table.next "" in
  List.iteri (fun i n -> symbols.(i) <- n) fields;
  let next = ref (List.length fields) in
  let bind n =
    symbols.(!next) <- n;
    incr next
  in
  let rec emit x =
    match x.node with
    | Input i -> col symbols.(i)
    | Broadcast (_, x) -> emit x
    | Constant (`Int i) ->
      P.Call
        ( pl "lit"
        , [ P.Positional (P.Int (Int64.to_string i)); P.Keyword ("dtype", pl "Int64") ] )
    | Constant (`Float f) -> lit (P.Float (P.float f))
    | Constant (`Bool b) -> lit (P.Bool b)
    | Constant (`String s) -> lit (P.Str s)
    | Null -> P.Call (pl "lit", [ P.Positional P.Null; P.Keyword ("dtype", dtype x.typ) ])
    | Choose (c, t, f) -> choose (emit c) (emit t) (emit f)
    | Coalesce (a, b) -> P.Call (pl "coalesce", positional [ emit a; emit b ])
    | Polars_sqrt a -> P.meth (emit a) "sqrt" []
    | Apply (row, args) -> apply row (List.map emit args)
    | Convert a -> P.meth (emit a) "cast" [ P.Positional (pl "Float64") ]
    | Count -> P.meth (P.Call (pl "len", [])) "cast" [ P.Positional (pl "Int64") ]
    | Text parts ->
      let part = function
        | Shape.Fixed s -> lit (P.Str s)
        | Shown (x, format) ->
          let e = emit x in
          let adapter f =
            P.meth
              e
              "map_batches"
              [ P.Positional f; P.Keyword ("return_dtype", pl "String") ]
          in
          (match format, x.typ.primitive with
           | Some spec, _ -> adapter (P.call "rp_formatter" [ P.Str spec ])
           | None, String -> e
           | None, Int64 -> P.meth e "cast" [ P.Positional (pl "String") ]
           | None, Float64 -> adapter (P.Name "rp_show_float")
           | None, Bool ->
             P.meth
               e
               "replace_strict"
               [ P.Positional
                   (P.Dict [ P.Bool true, P.Str "TRUE"; P.Bool false, P.Str "FALSE" ])
               ; P.Keyword ("return_dtype", pl "String")
               ])
      in
      P.Call
        ( pl "concat_str"
        , [ P.Positional (P.List (List.map part parts))
          ; P.Keyword ("separator", P.Str "")
          ] )
    | Member (a, items) ->
      P.meth (emit a) "is_in" [ P.Positional (P.List (List.map emit_value items)) ]
    | Unary (Not, a) -> P.Unary (P.Invert, emit a)
    | Unary (Negate, a) when x.typ.primitive = Int64 ->
      narrow (P.Unary (P.Negate, wide (emit a)))
    | Unary (Negate, a) -> P.Unary (P.Negate, emit a)
    | Binary (((Add | Sub | Mul) as op), a, b) when x.typ.primitive = Int64 ->
      narrow
        (P.Binary
           ( (match op with
              | Add -> P.Add
              | Sub -> P.Subtract
              | _ -> P.Multiply)
           , wide (emit a)
           , emit b ))
    | Required a ->
      P.meth
        (emit a)
        "map_batches"
        [ P.Positional (P.Name "rp_present"); P.Keyword ("return_dtype", pl "Boolean") ]
    | Binary (op, a, b) ->
      P.Binary
        ( (match op with
           | Add -> P.Add
           | Sub -> P.Subtract
           | Mul -> P.Multiply
           | Div -> P.Divide
           | Mod -> P.Modulo
           | Power -> P.Power
           | Less -> P.Less
           | Greater -> P.Greater
           | Less_equal -> P.Less_equal
           | Greater_equal -> P.Greater_equal
           | Equal -> P.Equal
           | Not_equal -> P.Not_equal
           | And -> P.Both
           | Or -> P.Either)
        , emit a
        , emit b )
    | Reduce (reduction, policy, a) ->
      let source = emit a in
      let a =
        match policy with
        | Fail ->
          P.meth
            source
            "map_batches"
            [ P.Positional (P.Name "rp_present")
            ; P.Keyword ("return_dtype", dtype x.typ)
            ]
        | Skip | Propagate -> source
      in
      let ddof = [ P.Keyword ("ddof", P.Int "1") ] in
      let guard value =
        match policy with
        | Propagate ->
          choose
            (P.Binary (P.Equal, P.meth source "null_count" [], P.Int "0"))
            value
            P.Null
        | Skip | Fail -> value
      in
      guard
      @@
        (match reduction with
        | Sum ->
          let total =
            if x.typ.primitive = Int64
            then narrow (P.meth (wide a) "sum" [])
            else P.meth a "sum" []
          in
          choose (P.Binary (P.Greater, P.meth a "count" [], P.Int "0")) total P.Null
        | Mean -> P.meth a "mean" []
        | Median -> P.meth a "median" []
        | Var -> P.meth a "var" ddof
        | Sd -> P.meth a "std" ddof
        | Min -> P.meth a "min" []
        | Max -> P.meth a "max" []
        | Count -> P.meth (P.Call (pl "len", [])) "cast" [ P.Positional (pl "Int64") ]
        | Distinct ->
          P.meth
            (P.meth (P.meth a "drop_nulls" []) "n_unique" [])
            "cast"
            [ P.Positional (pl "Int64") ])
  in
  let rec stage = function
    | Table.Pivot p ->
      let ids = List.map fst p.ids in
      List.iter bind (ids @ if p.wider then p.labels else [ p.key; p.value ]);
      if p.wider
      then
        [ P.link
            "pipe"
            (positional
               [ P.Name "rp_wider"
               ; P.names ids
               ; P.Str p.key
               ; P.Str p.value
               ; P.names p.labels
               ])
        ]
      else
        [ P.link
            "unpivot"
            [ P.Positional (P.names p.labels)
            ; P.Keyword ("index", P.names ids)
            ; P.Keyword ("variable_name", P.Str p.key)
            ; P.Keyword ("value_name", P.Str p.value)
            ]
        ]
    | Table.Rename changes ->
      List.iter (fun (_, b, i) -> symbols.(i) <- b) changes;
      [ P.link
          "rename"
          [ P.Positional (P.Dict (List.map (fun (a, b, _) -> P.Str a, P.Str b) changes)) ]
      ]
    | Window (n, x, keys) ->
      let value = emit x in
      bind n;
      P.link
        "with_columns"
        [ P.Positional
            (named n (P.meth value "over" [ P.Positional (P.names (List.map fst keys)) ]))
        ]
      :: checks [ n, x ]
    | Table.Take n -> [ P.link "head" [ P.Positional (P.Int (string_of_int n)) ] ]
    | Distinct keys ->
      [ P.link "unique" [ P.Keyword ("subset", P.List (List.map emit keys)) ] ]
    | Table.Arrange keys ->
      [ P.link
          "sort"
          [ P.Positional (P.List (List.map (fun (x, _) -> emit x) keys))
          ; P.Keyword ("descending", P.List (List.map (fun (_, d) -> P.Bool d) keys))
          ; P.Keyword ("nulls_last", P.Bool true)
          ]
      ]
    | Summarize (keys, measures) ->
      let measure (n, x) =
        named
          n
          (match x with
           | None -> P.meth (P.Call (pl "len", [])) "cast" [ P.Positional (pl "Int64") ]
           | Some x -> emit x)
      in
      let aggregates = P.List (List.map measure measures) in
      List.iter bind (List.map fst keys @ List.map fst measures);
      let computed =
        List.filter_map (fun (n, x) -> Option.map (fun x -> n, x) x) measures
      in
      (if keys = []
       then [ P.link "select" [ P.Positional aggregates ] ]
       else
         [ P.link "group_by" [ P.Positional (P.names (List.map fst keys)) ]
         ; P.link "agg" [ P.Positional aggregates ]
         ])
      @ checks computed
    | Table.Select fields -> [ P.link "select" [ P.Positional (P.names fields) ] ]
    | Filter x -> [ P.link "filter" [ P.Positional (emit x) ] ]
    | Bind (name, x) ->
      let value = emit x in
      bind name;
      P.link "with_columns" [ P.Positional (named name value) ] :: checks [ name, x ]
    | Assert_key columns ->
      [ P.link
          "pipe"
          [ P.Positional (P.Name "rp_assert_key"); P.Positional (P.names columns) ]
      ]
  and checks columns =
    match List.filter (fun (_, x) -> Shape.nonfinite x) columns with
    | [] -> []
    | risky ->
      [ P.link "with_columns" (List.map (fun (n, _) -> P.Positional (finite n)) risky) ]
  in
  List.concat_map
    (fun step ->
       match stage step, List.find_opt (fun (s, _) -> s == step) table.locations with
       | first :: rest, Some (_, span) -> { first with P.span = Some span } :: rest
       | links, _ -> links)
    (List.rev table.steps)
;;

open Analysis_program

let rec python_stages d =
  let base =
    match d.origin with
    | File source | Contracted { source; _ } ->
      P.call
        "read_locked"
        [ P.Str source.path
        ; P.Str source.digest
        ; P.Str (Data_file.format_name source.format)
        ; P.Dict (List.map (fun (n, t, _) -> P.Str n, P.Str (kind t)) source.columns)
        ]
    | Joined { left; right; key; kind; relationship; covered } ->
      P.Chain
        ( (if covered
           then
             P.call
               "rp_covered"
               [ python_table left
               ; python_table right
               ; P.Str key
               ; P.Str (join_name kind)
               ]
           else python_table left)
        , [ P.link
              "join"
              ([ P.Positional (python_table right)
               ; P.Keyword ("on", P.Str key)
               ; P.Keyword ("how", P.Str (join_name kind))
               ]
               @ (match relationship with
                  | None -> []
                  | Some r ->
                    [ P.Keyword
                        ( "validate"
                        , P.Str
                            (match r with
                             | One_to_one -> "1:1"
                             | Many_to_one -> "m:1"
                             | One_to_many -> "1:m"
                             | Many_to_many -> "m:m") )
                    ])
               @ [ P.Keyword ("nulls_equal", P.Bool false)
                 ; P.Keyword ("coalesce", P.Bool true)
                 ])
          ; P.link "select" [ P.Positional (P.names d.fields) ]
          ] )
  in
  base, lower ~fields:d.fields d.plan

and python_table d =
  let base, links = python_stages d in
  P.Chain (base, links)
;;

let python ?strict ~directory declarations =
  P.Verbatim Adapters.polars
  :: List.map
       (fun (d, output) ->
          let base, links = python_stages (public_view d) in
          let collected = P.Chain (base, links @ [ P.link "collect" [] ]) in
          P.Expression
            (match output with
             | Write path ->
               P.Chain
                 ( base
                 , links
                   @ [ P.link "collect" []
                     ; P.link "write_parquet" [ P.Positional (P.Str path) ]
                     ] )
             | Print -> P.call "print" [ collected ]))
       (compile ?strict ~directory declarations)
;;

let preview ~runtime d ~offset ~limit =
  let d = public_view d in
  let path = Filename.temp_file "rupam" ".parquet" in
  let total =
    Worker.evaluate
      runtime
      Runtime.Python
      (fst
         (P.program
            [ P.Verbatim Adapters.polars
            ; P.Expression
                (P.call
                   "print"
                   [ P.call
                       "rp_preview"
                       [ python_table d
                       ; P.Int (string_of_int offset)
                       ; P.Int (string_of_int limit)
                       ; P.Str path
                       ]
                   ])
            ]))
  in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> page d ~path ~total ~offset)
;;

module Values = struct
  open Syntax

  let math name a = P.Call (P.Attribute (P.Name "math", name), [ P.Positional a ])
  let as_float e = P.call "float" [ e ]
  let missing e = P.Binary (P.Is, e, P.Null)

  let native fresh (builtin : Prelude.builtin) args =
    match builtin, args with
    | Coalesce, [ a; fallback ] ->
      let t = fresh () in
      P.If (missing (P.Walrus (t, a)), fallback, P.Name t)
    | Row Abs, [ a ] -> P.call "abs" [ a ]
    | Row Sign, [ a ] ->
      let v = fresh () in
      P.Call
        ( P.Lambda
            ( [ v ]
            , P.Binary
                ( P.Subtract
                , P.Binary (P.Greater, P.Name v, P.Int "0")
                , P.Binary (P.Less, P.Name v, P.Int "0") ) )
        , [ P.Positional a ] )
    | Row Sqrt, [ a ] -> math "sqrt" a
    | Row Exp, [ a ] -> math "exp" a
    | Row Log, [ a ] -> math "log" a
    | Row Log2, [ a ] -> math "log2" a
    | Row Log10, [ a ] -> math "log10" a
    | Row Floor, [ a ] -> as_float (math "floor" a)
    | Row Ceiling, [ a ] -> as_float (math "ceil" a)
    | Row Trunc, [ a ] -> as_float (math "trunc" a)
    | Row Round, [ a ] -> as_float (P.call "round" [ a ])
    | Row Pmin, [ a; b ] -> P.call "min" [ a; b ]
    | Row Pmax, [ a; b ] -> P.call "max" [ a; b ]
    | Row Present, [ a ] -> P.Binary (P.Is_not, a, P.Null)
    | Row Absent, [ a ] -> missing a
    | Row Length, [ a ] -> P.call "len" [ a ]
    | Row Lower, [ a ] -> P.meth a "lower" []
    | Row Upper, [ a ] -> P.meth a "upper" []
    | Row Trim, [ a ] -> P.meth a "strip" []
    | Row Starts, [ a; b ] -> P.meth a "startswith" [ P.Positional b ]
    | Row Ends, [ a; b ] -> P.meth a "endswith" [ P.Positional b ]
    | Row Detect, [ a; b ] ->
      P.Binary
        ( P.Is_not
        , P.Call (P.Attribute (P.Name "re", "search"), [ P.Positional b; P.Positional a ])
        , P.Null )
    | Reduction _, _ ->
      Diagnostic.fail Wrong_arguments (Prelude.name builtin ^ " works on table columns")
    | _ -> Diagnostic.fail Wrong_arguments ("Wrong arguments to " ^ Prelude.name builtin)
  ;;

  let arity builtin =
    match snd (Infer.signature builtin) with
    | Infer.Arrow (ps, _) -> List.length ps
    | _ -> 0
  ;;

  let operator : Syntax.binary -> P.operator = function
    | Add -> P.Add
    | Sub -> P.Subtract
    | Mul -> P.Multiply
    | Div -> P.Divide
    | Mod -> P.Modulo
    | Power -> P.Power
    | Less -> P.Less
    | Greater -> P.Greater
    | Less_equal -> P.Less_equal
    | Greater_equal -> P.Greater_equal
    | Equal -> P.Equal
    | Not_equal -> P.Not_equal
    | And -> P.And
    | Or -> P.Or
  ;;

  let literal : Syntax.literal -> P.expr = function
    | Int s -> P.Int s
    | Float s -> P.Float s
    | Text s -> P.Str s
    | Bool b -> P.Bool b
    | Absent -> P.Null
  ;;

  let values declarations =
    let serial = ref 0 in
    let fresh () =
      incr serial;
      "v" ^ string_of_int !serial
    in
    let error (x : Syntax.expr) code message =
      Diagnostic.fail ~span:x.span code message
    in
    let rec emit env (x : Syntax.expr) =
      let e = render env x in
      P.Located (x.span, if Infer.converted x then as_float e else e)
    and render env (x : Syntax.expr) =
      let expr = emit env in
      let callee (f : Syntax.expr) args =
        match f.form with
        | Name (Bare, n) when not (List.mem_assoc n env) ->
          (match Prelude.find n with
           | Some b -> native fresh b args
           | None -> P.Call (expr f, List.map (fun a -> P.Positional a) args))
        | _ -> P.Call (expr f, List.map (fun a -> P.Positional a) args)
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
                   Some (missing (P.Walrus (v, expr operand))), P.Name v)
                 else None, expr operand)
              flags
              operands
          in
          let tests = List.filter_map fst parts in
          P.If
            ( List.fold_left
                (fun a b -> P.Binary (P.Or, a, b))
                (List.hd tests)
                (List.tl tests)
            , P.Null
            , build (List.map snd parts) )
      in
      match x.form with
      | Name (Bare, name) ->
        (match List.assoc_opt name env, Prelude.find name with
         | Some id, _ -> P.Name id
         | None, Some b ->
           let ps = List.init (arity b) (fun _ -> fresh ()) in
           P.Lambda (ps, native fresh b (List.map (fun p -> P.Name p) ps))
         | None, None -> error x Unknown_name ("Unknown name: " ^ name))
      | Literal (Int s) when Infer.promoted x -> P.Float (P.float (Shape.exact_float s))
      | Literal l -> literal l
      | Annotated (v, _) -> expr v
      | Tuple xs -> P.Tuple (List.map expr xs)
      | List xs -> P.List (List.map expr xs)
      | Record fs -> P.Dict (List.map (fun (n, v) -> P.Str n, expr v) fs)
      | Field (v, n) -> P.Subscript (expr v, P.Str n)
      | Local (n, v, body) ->
        let id = fresh () in
        P.Call (P.Lambda ([ id ], emit ((n, id) :: env) body), [ P.Positional (expr v) ])
      | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) -> expr v
      | Call (f, xs) -> operate (List.map snd xs) (callee f)
      | Pipe (a, { form = Call (g, xs); _ }) -> operate (a :: List.map snd xs) (callee g)
      | Pipe (a, f) -> operate [ a ] (callee f)
      | Lambda (ps, body) ->
        let names = List.map (fun p -> p, fresh ()) ps in
        P.Lambda (List.map snd names, emit (names @ env) body)
      | If (c, a, b) -> P.If (expr c, expr a, expr b)
      | Member (a, set) ->
        operate [ a; set ] (function
          | [ a; set ] -> P.Binary (P.In, a, set)
          | _ -> assert false)
      | Binary (op, a, b) ->
        operate [ a; b ] (function
          | [ a; b ] ->
            let v = P.Binary (operator op, a, b) in
            (match op, Infer.operand x with
             | Power, _ -> as_float v
             | (Add | Sub | Mul), Some Int -> P.call "rp_int" [ v ]
             | (Add | Sub | Mul), Some Float -> P.call "rp_real" [ v ]
             | _ -> v)
          | _ -> assert false)
      | Unary (op, v) ->
        operate [ v ] (function
          | [ v ] ->
            (match op with
             | Negate when Infer.operand x = Some Int ->
               P.call "rp_int" [ P.Unary (P.Negate, v) ]
             | Negate -> P.Unary (P.Negate, v)
             | Not -> P.Unary (P.Not, v))
          | _ -> assert false)
      | Interpolate pieces ->
        let shown = ref (Infer.shown x) in
        let guards = ref [] in
        let part = function
          | Syntax.Text_piece s -> [ P.Str s ]
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
                guards := missing (P.Walrus (id, expr slot.value)) :: !guards;
                P.Name id)
              else expr slot.value
            in
            let display =
              match slot.format, t with
              | Some spec, _ -> P.call "format" [ v; P.Str spec ]
              | None, String -> v
              | None, Int -> P.call "str" [ v ]
              | None, Float -> P.call "repr" [ v ]
              | None, Bool -> P.If (v, P.Str "TRUE", P.Str "FALSE")
              | None, _ -> P.call "rp_dynamic" [ v ]
            in
            Option.to_list (Option.map (fun e -> P.Str e) slot.echo) @ [ display ]
        in
        let joined =
          match List.concat_map part pieces with
          | [] -> P.Str ""
          | first :: rest -> List.fold_left (fun a b -> P.Binary (P.Add, a, b)) first rest
        in
        (match List.rev !guards with
         | [] -> joined
         | first :: rest ->
           P.If
             (List.fold_left (fun a b -> P.Binary (P.Or, a, b)) first rest, P.Null, joined))
      | Match (subject, arms) ->
        let id = fresh () in
        let all = function
          | [] -> None
          | t :: rest -> Some (List.fold_left (fun a b -> P.Binary (P.And, a, b)) t rest)
        in
        let rec pattern path = function
          | Syntax.Precord ns -> [], List.map (fun n -> n, P.Subscript (path, P.Str n)) ns
          | Wildcard -> [], []
          | Bind n -> [], [ n, path ]
          | Pliteral Absent | Tag ("None", []) -> [ missing path ], []
          | Pliteral l -> [ P.Binary (P.Equal, path, literal l) ], []
          | Tag ("Some", [ p ]) ->
            let tests, bindings = pattern path p in
            P.Binary (P.Is_not, path, P.Null) :: tests, bindings
          | Tag (n, ps) ->
            let tests, bindings = children path 1 ps in
            ( P.call "isinstance" [ path; P.Name "tuple" ]
              :: P.Binary (P.Equal, P.Subscript (path, P.Int "0"), P.Str n)
              :: tests
            , bindings )
          | Ptuple ps -> children path 0 ps
          | Prange { low; high; inclusive } ->
            ( Option.to_list
                (Option.map (fun l -> P.Binary (P.Less_equal, literal l, path)) low)
              @ Option.to_list
                  (Option.map
                     (fun h ->
                        P.Binary
                          ((if inclusive then P.Less_equal else P.Less), path, literal h))
                     high)
            , [] )
          | Pas (n, p) ->
            let tests, bindings = pattern path p in
            tests, (n, path) :: bindings
          | Palt alternatives ->
            let each = List.map (pattern path) alternatives in
            let bindings = snd (List.hd each) in
            (match List.map (fun (tests, _) -> all tests) each with
             | tests when List.mem None tests -> [], bindings
             | t :: rest ->
               ( [ List.fold_left
                     (fun a b -> P.Binary (P.Or, a, Option.get b))
                     (Option.get t)
                     rest
                 ]
               , bindings )
             | [] -> [], bindings)
        and children path offset ps =
          let tests, bindings =
            List.mapi
              (fun i p ->
                 pattern (P.Subscript (path, P.Int (string_of_int (i + offset)))) p)
              ps
            |> List.split
          in
          List.concat tests, List.concat bindings
        in
        let branch (arm : Syntax.arm) rest =
          let tests, bindings = pattern (P.Name id) arm.pattern in
          let names = List.map (fun (n, _) -> n, fresh ()) bindings in
          let inner = names @ env in
          let bind body =
            match bindings with
            | [] -> body
            | _ ->
              P.Call
                ( P.Lambda (List.map snd names, body)
                , List.map (fun (_, path) -> P.Positional path) bindings )
          in
          let tests =
            tests @ Option.to_list (Option.map (fun g -> bind (emit inner g)) arm.guard)
          in
          let value = bind (emit inner arm.body) in
          match all tests with
          | None -> value
          | Some test -> P.If (test, value, rest)
        in
        P.Call
          ( P.Lambda ([ id ], List.fold_right branch arms P.Null)
          , [ P.Positional (expr subject) ] )
      | Range _ | Formula _ | Name ((Column | Lexical), _) ->
        error x Unsupported_feature "Expression has no Python lowering"
    in
    let _, _, statements =
      List.fold_left
        (fun (types, env, statements) (d : Syntax.declaration) ->
           let types, typ = Infer.declaration types d in
           match d with
           | Let (n, ps, body) ->
             let x =
               match ps with
               | None -> body
               | Some ps -> { body with form = Lambda (ps, body) }
             in
             let id = fresh () in
             types, (n, id) :: env, P.Assign (id, emit env x) :: statements
           | Evaluate x ->
             let shape = P.Str (Yojson.Basic.to_string (Infer.descriptor typ)) in
             ( types
             , env
             , P.Expression (P.call "print" [ P.call "rp_show" [ emit env x; shape ] ])
               :: statements )
           | Type (_, _, constructors) ->
             List.fold_left
               (fun (types, env, statements) (n, fs) ->
                  let id = fresh ()
                  and ps = List.map (fun _ -> fresh ()) fs in
                  let tuple = P.Tuple (P.Str n :: List.map (fun p -> P.Name p) ps) in
                  let value = if ps = [] then tuple else P.Lambda (ps, tuple) in
                  types, (n, id) :: env, P.Assign (id, value) :: statements)
               (types, env, statements)
               constructors
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
    P.Verbatim Adapters.python :: List.rev statements
  ;;
end

let program ~strict ~directory declarations =
  P.program
    (if tabular declarations
     then python ~strict ~directory declarations
     else Values.values declarations)
;;

let value declarations = fst (P.program (Values.values declarations))
let table d = P.render (python_table d)
