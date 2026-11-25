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
    | Reduce (Distinct, a) ->
      R.qualified
        "dplyr"
        "n_distinct"
        [ R.Positional (emit a); R.Named ("na.rm", R.Logical true) ]
      |> fun n -> R.qualified "bit64" "as.integer64" [ R.Positional n ]
    | Reduce (r, a) ->
      let x = emit a in
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
       | Distinct -> assert false)
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
    match d.origin with
    | File { path; format; columns } ->
      R.call
        "rp_read"
        [ R.String path
        ; R.String (Data_file.format_name format)
        ; R.Call
            ( R.Name "c"
            , List.map (fun (n, t) -> R.Named (n, R.String (polars_name t))) columns )
        ]
    | Contracted { dictionary; table; physical_types } ->
      R.call
        "rp_source"
        [ R.String dictionary.path
        ; R.String (Yojson.Basic.to_string dictionary.metadata)
        ; R.String table.name
        ; R.String (Yojson.Basic.to_string physical_types)
        ]
    | Joined { left; right; key; kind } ->
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
       (fun (d, output) ->
          let t = table (Analysis_program.public_view d) in
          R.Expression
            (match (output : Analysis_program.output) with
             | Write path -> R.pipe t (R.call "rp_write" [ R.String path ])
             | Print -> R.call "print" [ R.call "as_tibble" [ t ] ]))
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
