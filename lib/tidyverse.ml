open Shape
module R = R_syntax

let all_of names = R.call "all_of" [ R.strings names ]

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
    | Choose (c, t, f) ->
      R.qualified
        "dplyr"
        "if_else"
        [ R.Positional (emit c); R.Positional (emit t); R.Positional (emit f) ]
    | Coalesce (a, b) -> R.call "rp_coalesce" [ emit a; emit b ]
    | Polars_sqrt _ ->
      raise (Error "R target cannot execute the Python capability polars.sqrt")
    | Abs a -> R.call "abs" [ emit a ]
    | Binary (op, a, b) ->
      R.Binary
        ( (match op with
           | Add -> R.Add
           | Sub -> R.Subtract
           | Mul -> R.Multiply
           | Div -> R.Divide
           | Greater -> R.Greater)
        , emit a
        , emit b )
    | Reduce (r, a) ->
      R.call
        "rp_fold"
        [ emit a
        ; R.Name
            (match r with
             | Mean -> "mean"
             | Sd -> "sd"
             | Sum -> "sum")
        ]
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
      let value = emit x in
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
    | Select ns -> R.call "select" [ all_of ns ]
    | Bind (n, x) ->
      let value = emit x in
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
                 | Some x -> emit x ))
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
    match d.join with
    | None ->
      R.call
        "rp_source"
        [ R.String d.dictionary.path
        ; R.String (Yojson.Basic.to_string d.dictionary.metadata)
        ; R.String d.table.name
        ; R.String (Yojson.Basic.to_string d.physical_types)
        ]
    | Some { left; right; key; kind } ->
      R.Call
        ( R.Name (join_name kind ^ "_join")
        , [ R.Positional (table left)
          ; R.Positional (table right)
          ; R.Named ("by", R.String key)
          ; R.Named ("na_matches", R.String "never")
          ] )
  in
  lower ~fields:d.fields d.plan base
;;

let compile ?strict ~directory declarations =
  R.Verbatim Adapters.dplyr
  :: List.map
       (fun (d, path) ->
          R.Expression
            (R.pipe
               (table (Analysis_program.public_view d))
               (R.qualified
                  "nanoparquet"
                  "write_parquet"
                  [ R.Positional (R.String path) ])))
       (Analysis_program.compile ?strict ~directory declarations)
;;

let preview_page ~runtime d ~offset ~limit =
  let d = Analysis_program.public_view d in
  let rows =
    R.Call
      ( R.Name "seq.int"
      , [ R.Positional (R.Number (string_of_int (offset + 1)))
        ; R.Named ("length.out", R.Number (string_of_int limit))
        ] )
  in
  let page = R.pipe (R.call "as_tibble" [ table d ]) (R.call "slice" [ rows ]) in
  print_endline (if d.plan.ordered then "Ordered preview" else "Unordered preview");
  Worker.run
    runtime
    Runtime.R
    (fst
       (R.program
          [ R.Verbatim Adapters.dplyr
          ; R.Expression
              (R.Call
                 ( R.Name "print"
                 , [ R.Positional page; R.Named ("n", R.Number (string_of_int limit)) ] ))
          ]))
;;
