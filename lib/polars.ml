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
  | _ -> raise (Error ("Wrong arguments to " ^ Prelude.name (Row row)))
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
    | Unary (Not, a) -> P.Unary (P.Invert, emit a)
    | Unary (Negate, a) -> P.Unary (P.Negate, emit a)
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
    | Reduce (reduction, a) ->
      let a = emit a in
      let ddof = [ P.Keyword ("ddof", P.Int "1") ] in
      (match reduction with
       | Sum ->
         choose
           (P.Binary (P.Greater, P.meth a "count" [], P.Int "0"))
           (P.meth a "sum" [])
           P.Null
       | Mean -> P.meth a "mean" []
       | Median -> P.meth a "median" []
       | Var -> P.meth a "var" ddof
       | Sd -> P.meth a "std" ddof
       | Min -> P.meth a "min" []
       | Max -> P.meth a "max" []
       | Distinct ->
         P.meth
           (P.meth (P.meth a "drop_nulls" []) "n_unique" [])
           "cast"
           [ P.Positional (pl "Int64") ])
  in
  let stage = function
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
      [ P.link
          "with_columns"
          [ P.Positional
              (named
                 n
                 (P.meth value "over" [ P.Positional (P.names (List.map fst keys)) ]))
          ]
      ]
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
      if keys = []
      then [ P.link "select" [ P.Positional aggregates ] ]
      else
        [ P.link "group_by" [ P.Positional (P.names (List.map fst keys)) ]
        ; P.link "agg" [ P.Positional aggregates ]
        ]
    | Table.Select fields -> [ P.link "select" [ P.Positional (P.names fields) ] ]
    | Filter x -> [ P.link "filter" [ P.Positional (emit x) ] ]
    | Bind (name, x) ->
      let value = emit x in
      bind name;
      [ P.link "with_columns" [ P.Positional (named name value) ] ]
  in
  List.concat_map
    (fun step ->
       match stage step, List.find_opt (fun (s, _) -> s == step) table.locations with
       | first :: rest, Some (_, span) -> { first with P.span = Some span } :: rest
       | links, _ -> links)
    (List.rev table.steps)
;;
