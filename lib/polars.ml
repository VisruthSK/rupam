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
    | Choose (c, t, f) -> choose (emit c) (emit t) (emit f)
    | Coalesce (a, b) -> P.Call (pl "coalesce", positional [ emit a; emit b ])
    | Polars_sqrt a -> P.meth (emit a) "sqrt" []
    | Abs a -> P.meth (emit a) "abs" []
    | Binary (op, a, b) ->
      P.Binary
        ( (match op with
           | Add -> P.Add
           | Sub -> P.Subtract
           | Mul -> P.Multiply
           | Div -> P.Divide
           | Greater -> P.Greater)
        , emit a
        , emit b )
    | Reduce (Mean, a) -> P.meth (emit a) "mean" []
    | Reduce (Sd, a) -> P.meth (emit a) "std" [ P.Keyword ("ddof", P.Int "1") ]
    | Reduce (Sum, a) ->
      let a = emit a in
      choose
        (P.Binary (P.Greater, P.meth a "count" [], P.Int "0"))
        (P.meth a "sum" [])
        P.Null
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
