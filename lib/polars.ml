module C = TransformIR
module T = Types
module P = Python_syntax

let pl name = P.Attribute (P.Name "pl", name)
let col n = P.Call (pl "col", [ P.Positional (P.Str n) ])
let positional xs = List.map (fun x -> P.Positional x) xs
let cast x dtype = P.meth x "cast" [ P.Positional (pl dtype) ]
let alias n e = P.meth e "alias" [ P.Positional (P.Str n) ]
let wide e = cast e "Int128"

let narrow e =
  P.meth e "cast" [ P.Positional (pl "Int64"); P.Keyword ("strict", P.Bool true) ]
;;

let length = cast (P.Call (pl "len", [])) "Int64"

let choose condition yes no =
  P.meth
    (P.meth (P.Call (pl "when", [ P.Positional condition ])) "then" [ P.Positional yes ])
    "otherwise"
    [ P.Positional no ]
;;

let dtype t =
  match T.repr (T.present t) with
  | Int -> "Int64"
  | Float -> "Float64"
  | Bool -> "Boolean"
  | _ -> "String"
;;

let batches e f dtype' =
  P.meth e "map_batches" [ P.Positional f; P.Keyword ("return_dtype", pl dtype') ]
;;

let builtin (row : Prelude.row) args =
  let on x name = P.meth x name [] in
  let text x name args = P.meth (P.Attribute (x, "str")) name (positional args) in
  match row, args with
  | Abs, [ x ] -> on x "abs"
  | Sign, [ x ] -> cast (on x "sign") "Int64"
  | Sqrt, [ x ] -> on x "sqrt"
  | Exp, [ x ] -> on x "exp"
  | Log, [ x ] -> on x "log"
  | Log2, [ x ] -> P.meth x "log" [ P.Positional (P.Int "2") ]
  | Log10, [ x ] -> on x "log10"
  | Floor, [ x ] -> on x "floor"
  | Ceiling, [ x ] -> on x "ceil"
  | Trunc, [ x ] ->
    choose (P.Binary (Greater_equal, x, P.Int "0")) (on x "floor") (on x "ceil")
  | Round, [ x ] ->
    P.meth
      x
      "round"
      [ P.Positional (P.Int "0"); P.Keyword ("mode", P.Str "half_to_even") ]
  | Pmin, [ a; b ] -> P.Call (pl "min_horizontal", positional [ a; b ])
  | Pmax, [ a; b ] -> P.Call (pl "max_horizontal", positional [ a; b ])
  | Present, [ x ] -> on x "is_not_null"
  | Absent, [ x ] -> on x "is_null"
  | Length, [ x ] -> cast (text x "len_chars" []) "Int64"
  | Lower, [ x ] -> text x "to_lowercase" []
  | Upper, [ x ] -> text x "to_uppercase" []
  | Trim, [ x ] -> text x "strip_chars" []
  | Starts, [ x; p ] -> text x "starts_with" [ p ]
  | Ends, [ x; p ] -> text x "ends_with" [ p ]
  | Detect, [ x; p ] -> text x "contains" [ p ]
  | _ -> invalid_arg "Wrong arguments to a builtin"
;;

let operator : Syntax.binary -> P.operator = function
  | Add -> Add
  | Sub -> Subtract
  | Mul -> Multiply
  | Div -> Divide
  | Mod -> Modulo
  | Power -> Power
  | Less -> Less
  | Greater -> Greater
  | Less_equal -> Less_equal
  | Greater_equal -> Greater_equal
  | Equal -> Equal
  | Not_equal -> Not_equal
  | And -> Both
  | Or -> Either
;;

let columnar =
  C.exists (fun e ->
    match e.node with
    | Column _ | Reduce _ | Count -> true
    | _ -> false)
;;

let rec expression ~scalar (e : C.expr) : P.expr =
  let go = expression ~scalar in
  let integer = T.repr (T.present e.typ) = Int in
  if not (columnar e)
  then
    P.Call (pl "lit", [ P.Positional (scalar e); P.Keyword ("dtype", pl (dtype e.typ)) ])
  else (
    match e.node with
    | Column n -> col n
    | If (c, a, b) -> choose (go c) (go a) (go b)
    | Coalesce (a, b) -> P.Call (pl "coalesce", positional [ go a; go b ])
    | Builtin (row, args) -> builtin row (List.map go args)
    | Foreign ("polars", "sqrt", [ x ]) -> P.meth (go x) "sqrt" []
    | Convert x -> cast (go x) "Float64"
    | Count -> length
    | Unary (Not, x) -> P.Unary (Invert, go x)
    | Unary (Negate, x) when integer -> narrow (P.Unary (Negate, wide (go x)))
    | Unary (Negate, x) -> P.Unary (Negate, go x)
    | Binary (((Add | Sub | Mul) as op), a, b) when integer ->
      narrow (P.Binary (operator op, wide (go a), go b))
    | Binary (op, a, b) -> P.Binary (operator op, go a, go b)
    | Require x -> batches (go x) (P.Name "rp_present") "Boolean"
    | Member (x, { node = List items; _ }) ->
      P.meth (go x) "is_in" [ P.Positional (P.List (List.map scalar items)) ]
    | Text pieces ->
      let part = function
        | C.Fixed s -> P.Call (pl "lit", [ P.Positional (P.Str s) ])
        | Shown (x, format) ->
          let v = go x in
          (match format, T.repr (T.present x.typ) with
           | Some spec, _ -> batches v (P.call "rp_formatter" [ P.Str spec ]) "String"
           | None, String -> v
           | None, Int -> cast v "String"
           | None, Float -> batches v (P.Name "rp_show_float") "String"
           | None, Bool ->
             P.meth
               v
               "replace_strict"
               [ P.Positional
                   (P.Dict [ P.Bool true, P.Str "TRUE"; P.Bool false, P.Str "FALSE" ])
               ; P.Keyword ("return_dtype", pl "String")
               ]
           | None, _ -> cast v "String")
      in
      P.Call
        ( pl "concat_str"
        , [ P.Positional (P.List (List.map part pieces))
          ; P.Keyword ("separator", P.Str "")
          ] )
    | Reduce (r, policy, x) -> reduce ~scalar e r policy x
    | Let (id, v, body) -> go (C.substitute id v body)
    | Lift (bound, body) ->
      go (List.fold_left (fun body (id, value) -> C.substitute id value body) body bound)
    | Foreign (m, f, _) ->
      Diagnostic.fail
        ~span:e.span
        Dialect_capability
        (m ^ "." ^ f ^ " is not available in Polars")
    | _ ->
      Diagnostic.fail
        ~span:e.span
        Unsupported_feature
        "This expression has no Polars lowering")

and reduce ~scalar (e : C.expr) r policy x =
  let source = expression ~scalar x in
  let a =
    if policy = C.Fail then batches source (P.Name "rp_present") (dtype x.typ) else source
  in
  let ddof = [ P.Keyword ("ddof", P.Int "1") ] in
  let value =
    match (r : Prelude.reduction) with
    | Sum ->
      let total =
        if T.repr (T.present x.typ) = Int
        then narrow (P.meth (wide a) "sum" [])
        else P.meth a "sum" []
      in
      choose (P.Binary (Greater, P.meth a "count" [], P.Int "0")) total P.Null
    | Mean -> P.meth a "mean" []
    | Median -> P.meth a "median" []
    | Var -> P.meth a "var" ddof
    | Sd -> P.meth a "std" ddof
    | Min -> P.meth a "min" []
    | Max -> P.meth a "max" []
    | Count -> length
    | Distinct when policy = C.Keep -> cast (P.meth a "n_unique" []) "Int64"
    | Distinct -> cast (P.meth (P.meth a "drop_nulls" []) "n_unique" []) "Int64"
    | First | Last ->
      let present = if policy = C.Skip then P.meth a "drop_nulls" [] else a in
      P.meth present (if r = First then "first" else "last") []
  in
  ignore e;
  if policy = C.Keep && not (Prelude.missing_is_value r)
  then choose (P.Binary (Equal, P.meth source "null_count" [], P.Int "0")) value P.Null
  else value
;;

let finite n = batches (col n) (P.Name "rp_finite") "Float64"

let checks columns =
  match List.filter (fun (_, x) -> Host.risky x && Host.float x) columns with
  | [] -> []
  | risky ->
    [ P.link "with_columns" (List.map (fun (n, _) -> P.Positional (finite n)) risky) ]
;;

let step ~scalar span : C.step -> P.link list =
  let go = expression ~scalar in
  let at name args = P.link ~span name args in
  function
  | Filter x -> [ at "filter" [ P.Positional (go x) ] ]
  | Mutate { name; value; by } ->
    let v = go value in
    let v = if by = [] then v else P.meth v "over" [ P.Positional (P.names by) ] in
    at "with_columns" [ P.Positional (alias name v) ] :: checks [ name, value ]
  | Select columns -> [ at "select" [ P.Positional (P.names columns) ] ]
  | Rename changes ->
    [ at
        "rename"
        [ P.Positional
            (P.Dict (List.map (fun (fresh, old) -> P.Str old, P.Str fresh) changes))
        ]
    ]
  | Summarize { by; measures } ->
    let aggregates = P.List (List.map (fun (n, x) -> alias n (go x)) measures) in
    (if by = []
     then [ at "select" [ P.Positional aggregates ] ]
     else
       [ at
           "group_by"
           [ P.Positional (P.names by); P.Keyword ("maintain_order", P.Bool true) ]
       ; P.link "agg" [ P.Positional aggregates ]
       ])
    @ checks measures
  | Arrange keys ->
    [ at
        "sort"
        [ P.Positional (P.List (List.map (fun (x, _) -> go x) keys))
        ; P.Keyword
            ( "descending"
            , P.List (List.map (fun (_, d) -> P.Bool (d = C.Descending)) keys) )
        ; P.Keyword ("nulls_last", P.Bool true)
        ]
    ]
  | Take n -> [ at "head" [ P.Positional (P.Int (string_of_int n)) ] ]
  | Distinct _ -> [ at "unique" [ P.Keyword ("maintain_order", P.Bool true) ] ]
  | Assert_key columns ->
    [ at "pipe" [ P.Positional (P.Name "rp_assert_key"); P.Positional (P.names columns) ]
    ]
  | Pivot_longer { ids; columns; names_to; values_to } ->
    [ at
        "unpivot"
        [ P.Positional (P.names columns)
        ; P.Keyword ("index", P.names ids)
        ; P.Keyword ("variable_name", P.Str names_to)
        ; P.Keyword ("value_name", P.Str values_to)
        ]
    ]
  | Pivot_wider { ids; names_from; values_from; levels } ->
    [ at
        "pipe"
        (positional
           [ P.Name "rp_wider"
           ; P.names ids
           ; P.Str names_from
           ; P.Str values_from
           ; P.names levels
           ])
    ]
;;

let file (f : C.file) =
  P.call
    "read_locked"
    [ P.Str f.path
    ; P.Str f.digest
    ; P.Str (Data_file.format_name f.format)
    ; P.Dict
        (List.map
           (fun (n, t) ->
              ( P.Str n
              , P.Str
                  (match T.repr (T.present t) with
                   | Int -> "Int64"
                   | Float -> "Float64"
                   | Bool -> "Bool"
                   | _ -> "String") ))
           f.columns)
    ]
;;

let rec table ~scalar ~name (t : C.table) =
  let base =
    match t.source with
    | Read f | Load { file = f; _ } -> file f
    | Bound id -> P.Name (name id)
    | Join { left; right; key; kind; relationship; covered } ->
      let l = table ~scalar ~name left
      and r = table ~scalar ~name right in
      let l =
        if covered
        then P.call "rp_covered" [ l; r; P.Str key; P.Str (C.join_name kind) ]
        else l
      in
      let fields =
        C.names left
        @
        match kind with
        | Semi | Anti -> []
        | _ -> List.filter (( <> ) key) (C.names right)
      in
      P.Chain
        ( l
        , [ P.link
              "join"
              ([ P.Positional r
               ; P.Keyword ("on", P.Str key)
               ; P.Keyword ("how", P.Str (C.join_name kind))
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
          ; P.link "select" [ P.Positional (P.names fields) ]
          ] )
  in
  match List.concat_map (fun (s : C.stage) -> step ~scalar s.at s.step) t.stages with
  | [] -> base
  | links -> P.Chain (base, links)
;;
