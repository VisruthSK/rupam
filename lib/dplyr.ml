module C = TransformIR
module T = Types
module R = R_syntax

let integer64 e = R.qualified "bit64" "as.integer64" [ R.Positional e ]

let if_else c a b =
  R.qualified "dplyr" "if_else" [ R.Positional c; R.Positional a; R.Positional b ]
;;

let all_of names = R.call "all_of" [ R.strings names ]

let missing typ =
  match T.repr (T.present typ) with
  | Int -> R.Namespaced ("bit64", "NA_integer64_")
  | Float -> R.Name "NA_real_"
  | String -> R.Name "NA_character_"
  | _ -> R.Na
;;

let operator : Syntax.binary -> R.operator = function
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

let builtin (row : Prelude.row) args =
  match row, args with
  | Abs, [ x ] -> R.call "abs" [ x ]
  | Sign, [ x ] -> integer64 (R.call "sign" [ x ])
  | Sqrt, [ x ] -> R.call "sqrt" [ x ]
  | Exp, [ x ] -> R.call "exp" [ x ]
  | Log, [ x ] -> R.call "log" [ x ]
  | Log2, [ x ] -> R.call "log2" [ x ]
  | Log10, [ x ] -> R.call "log10" [ x ]
  | Floor, [ x ] -> R.call "floor" [ x ]
  | Ceiling, [ x ] -> R.call "ceiling" [ x ]
  | Trunc, [ x ] -> R.call "trunc" [ x ]
  | Round, [ x ] -> R.call "round" [ x ]
  | Pmin, [ a; b ] -> R.call "pmin" [ a; b ]
  | Pmax, [ a; b ] -> R.call "pmax" [ a; b ]
  | Present, [ x ] -> R.Not (R.call "is.na" [ x ])
  | Absent, [ x ] -> R.call "is.na" [ x ]
  | Length, [ x ] ->
    integer64
      (R.Call (R.Name "nchar", [ R.Positional x; R.Named ("type", R.String "chars") ]))
  | Lower, [ x ] -> R.call "tolower" [ x ]
  | Upper, [ x ] -> R.call "toupper" [ x ]
  | Trim, [ x ] -> R.call "trimws" [ x ]
  | Starts, [ x; p ] -> R.call "startsWith" [ x; p ]
  | Ends, [ x; p ] -> R.call "endsWith" [ x; p ]
  | Detect, [ x; p ] ->
    if_else
      (R.call "is.na" [ x ])
      R.Na
      (R.Call
         ( R.Name "grepl"
         , [ R.Positional p; R.Positional x; R.Named ("perl", R.Logical true) ] ))
  | _ -> invalid_arg "Wrong arguments to a builtin"
;;

let rec expression names (e : C.expr) : R.expr =
  let go = expression names in
  match e.node with
  | Column n -> R.Name n
  | Variable { stamp = 0; name } -> R.Name name
  | Variable id -> R.Name (Host.name names id)
  | Constant (Int i) -> integer64 (R.String (Int64.to_string i))
  | Constant (Float f) -> R.Number (R.number f)
  | Constant (Number s) -> R.Number s
  | Constant (Bool b) -> R.Logical b
  | Constant (Text s) -> R.String s
  | Absent -> missing e.typ
  | If (c, a, b) -> if_else (go c) (go a) (go b)
  | Coalesce (a, b) ->
    R.qualified "dplyr" "coalesce" [ R.Positional (go a); R.Positional (go b) ]
  | Builtin (row, args) -> builtin row (List.map go args)
  | Convert x -> R.call "as.double" [ go x ]
  | Unary (Not, x) -> R.Not (go x)
  | Unary (Negate, x) -> R.Negate (go x)
  | Binary (op, a, b) -> R.Binary (operator op, go a, go b)
  | Require x -> R.call "rp_present" [ go x ]
  | Count -> integer64 (R.qualified "dplyr" "n" [])
  | Reduce (r, policy, x) -> reduce names r policy x
  | Let (id, v, body) -> R.Block ([ Host.name names id, go v ], go body)
  | Lift (bound, body) ->
    let substituted =
      List.fold_left
        (fun body ((id : C.id), (value : C.expr)) -> C.substitute id value body)
        body
        bound
    in
    go substituted
  | Member (x, { node = List items; _ }) ->
    let x = go x in
    if_else
      (R.call "is.na" [ x ])
      R.Na
      (R.Binary (In_set, x, R.call "c" (List.map go items)))
  | Text pieces -> text names pieces
  | Apply (f, args) -> R.Call (go f, List.map (fun a -> R.Positional (go a)) args)
  | Foreign (m, f, _) ->
    Diagnostic.fail
      ~span:e.span
      Dialect_capability
      ("R target cannot execute the Python capability " ^ m ^ "." ^ f)
  | Member _
  | Match _
  | Lambda _
  | Construct _
  | Tuple _
  | List _
  | Record _
  | Field _
  | Table _ ->
    Diagnostic.fail
      ~span:e.span
      Unsupported_feature
      "This expression has no dplyr lowering"

and reduce names r policy x =
  let source = expression names x in
  let x = if policy = C.Fail then R.call "rp_present" [ source ] else source in
  let present f = R.Call (f, [ R.Positional x; R.Named ("na.rm", R.Logical true) ]) in
  let unless_empty value = if_else (R.call "all" [ R.call "is.na" [ x ] ]) R.Na value in
  let value =
    match (r : Prelude.reduction) with
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
      integer64
        (R.qualified
           "dplyr"
           "n_distinct"
           [ R.Positional x; R.Named ("na.rm", R.Logical (policy <> C.Keep)) ])
    | First | Last ->
      R.qualified
        "dplyr"
        (if r = First then "first" else "last")
        [ R.Positional x; R.Named ("na_rm", R.Logical (policy = C.Skip)) ]
    | Count -> integer64 (R.qualified "dplyr" "n" [])
  in
  if policy = C.Keep && not (Prelude.missing_is_value r)
  then if_else (R.call "anyNA" [ source ]) R.Na value
  else value

and text names pieces =
  let shown =
    List.filter_map
      (function
        | C.Shown (x, _) when T.is_optional x.typ -> Some (expression names x)
        | _ -> None)
      pieces
  in
  let part = function
    | C.Fixed s -> R.String s
    | Shown (x, format) ->
      let v = expression names x in
      (match format, T.repr (T.present x.typ) with
       | Some spec, _ -> R.call "rp_format_number" [ v; R.String spec ]
       | None, String -> v
       | None, Int -> R.call "as.character" [ v ]
       | None, Float -> R.call "rp_float" [ v ]
       | None, Bool -> if_else v (R.String "TRUE") (R.String "FALSE")
       | None, _ -> R.call "as.character" [ v ])
  in
  let joined = R.call "paste0" (List.map part pieces) in
  match shown with
  | [] -> joined
  | first :: rest ->
    if_else
      (List.fold_left
         (fun a b -> R.Binary (Either, a, R.call "is.na" [ b ]))
         (R.call "is.na" [ first ])
         rest)
      (R.Name "NA_character_")
      joined
;;

let checked names e =
  let v = expression names e in
  if Host.risky e then R.call "rp_checked" [ v ] else v
;;

let step names : C.step -> R.expr = function
  | Filter x -> R.call "filter" [ expression names x ]
  | Mutate { name; value; by } ->
    R.Call
      ( R.Name "mutate"
      , R.Named (name, checked names value)
        :: (if by = [] then [] else [ R.Named (".by", all_of by) ]) )
  | Select columns -> R.call "select" [ all_of columns ]
  | Rename changes ->
    R.Call
      (R.Name "rename", List.map (fun (fresh, old) -> R.Named (fresh, R.Name old)) changes)
  | Summarize { by; measures } ->
    R.Call
      ( R.Name "summarise"
      , List.map (fun (n, x) -> R.Named (n, checked names x)) measures
        @ if by = [] then [] else [ R.Named (".by", all_of by) ] )
  | Arrange keys ->
    R.call
      "arrange"
      (List.map
         (fun (x, d) ->
            let x = expression names x in
            if d = C.Descending then R.call "desc" [ x ] else x)
         keys)
  | Take n -> R.Call (R.Name "slice_head", [ R.Named ("n", R.Number (string_of_int n)) ])
  | Distinct _ -> R.call "distinct" []
  | Assert_key columns -> R.call "rp_assert_key" [ R.strings columns ]
  | Pivot_longer { columns; names_to; values_to; _ } ->
    R.qualified
      "tidyr"
      "pivot_longer"
      [ R.Positional (all_of columns)
      ; R.Named ("names_to", R.String names_to)
      ; R.Named ("values_to", R.String values_to)
      ]
  | Pivot_wider { ids; names_from; values_from; levels } ->
    R.call
      "rp_wider"
      [ R.strings ids; R.String names_from; R.String values_from; R.strings levels ]
;;

let file (f : C.file) =
  R.call
    "rp_read"
    [ R.String f.path
    ; R.String f.digest
    ; R.String (Data_file.format_name f.format)
    ; R.Call
        ( R.Name "c"
        , List.map
            (fun (n, t) ->
               R.Named
                 ( n
                 , R.String
                     (match T.repr (T.present t) with
                      | Int -> "Int64"
                      | Float -> "Float64"
                      | Bool -> "Bool"
                      | _ -> "String") ))
            f.columns )
    ]
;;

let rec table names (t : C.table) =
  let base =
    match t.source with
    | Read f | Load { file = f; _ } -> file f
    | Bound id -> R.Name (Host.name names id)
    | Join { left; right; key; kind; relationship; covered } ->
      let left_table = table names left
      and right_table = table names right in
      let left_table =
        if covered
        then
          R.call
            "rp_covered"
            [ left_table; right_table; R.String key; R.String (C.join_name kind) ]
        else left_table
      in
      R.Call
        ( R.Name (C.join_name kind ^ "_join")
        , [ R.Positional left_table
          ; R.Positional right_table
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
                    (String.map
                       (function
                         | '_' -> '-'
                         | c -> c)
                       (C.relationship_name r)) )
            ] )
  in
  List.fold_left
    (fun input (s : C.stage) -> R.pipe input (R.Located (s.at, step names s.step)))
    base
    t.stages
;;
