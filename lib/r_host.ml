module C = TransformIR
module T = Types
module R = R_syntax

let reserved =
  [ "if"
  ; "else"
  ; "repeat"
  ; "while"
  ; "function"
  ; "for"
  ; "next"
  ; "break"
  ; "TRUE"
  ; "FALSE"
  ; "NULL"
  ; "Inf"
  ; "NaN"
  ; "NA"
  ; "in"
  ; "c"
  ; "t"
  ; "q"
  ; "T"
  ; "F"
  ; "data"
  ; "list"
  ; "paste0"
  ]
;;

let names () = Host.names reserved
let number a = R.call "as.numeric" [ a ]
let integer64 a = R.qualified "bit64" "as.integer64" [ R.Positional a ]
let is_null a = R.call "is.null" [ a ]

let conjunction = function
  | [] -> None
  | first :: rest ->
    Some (List.fold_left (fun a b -> R.Binary (R.Both_scalar, a, b)) first rest)
;;

let builtin (row : Prelude.row) args =
  match Dplyr.dates row args with
  | Some e -> e
  | None ->
  match row, args with
  | Abs, [ a ] -> R.call "abs" [ a ]
  | Sign, [ a ] -> integer64 (R.call "sign" [ number a ])
  | Sqrt, [ a ] -> R.call "sqrt" [ a ]
  | Exp, [ a ] -> R.call "exp" [ a ]
  | Log, [ a ] -> R.call "log" [ a ]
  | Log2, [ a ] -> R.call "log2" [ a ]
  | Log10, [ a ] -> R.call "log10" [ a ]
  | Floor, [ a ] -> R.call "floor" [ a ]
  | Ceiling, [ a ] -> R.call "ceiling" [ a ]
  | Trunc, [ a ] -> R.call "trunc" [ a ]
  | Round, [ a ] -> R.call "round" [ a ]
  | Round_digits, [ a; d ] -> R.call "round" [ a; R.call "as.integer" [ d ] ]
  | Pmin, [ a; b ] -> R.call "min" [ a; b ]
  | Pmax, [ a; b ] -> R.call "max" [ a; b ]
  | Present, [ a ] -> R.Not (is_null a)
  | Absent, [ a ] -> is_null a
  | Length, [ a ] ->
    integer64
      (R.Call (R.Name "nchar", [ R.Positional a; R.Named ("type", R.String "chars") ]))
  | Lower, [ a ] -> R.call "tolower" [ a ]
  | Upper, [ a ] -> R.call "toupper" [ a ]
  | Trim, [ a ] -> R.call "trimws" [ a ]
  | Starts, [ a; b ] -> R.call "startsWith" [ a; b ]
  | Ends, [ a; b ] -> R.call "endsWith" [ a; b ]
  | Detect, [ a; b ] ->
    R.Call
      ( R.Name "grepl"
      , [ R.Positional b; R.Positional a; R.Named ("perl", R.Logical true) ] )
  | Size, [ a ] -> integer64 (R.call "length" [ a ])
  | Reverse, [ a ] -> R.call "rev" [ a ]
  | Sorted, [ a ] -> R.call "rp_sort" [ a ]
  | Deduplicate, [ a ] -> R.call "rp_unique" [ a ]
  | Map_each, [ a; f ] -> R.call "lapply" [ a; f ]
  | Retain, [ a; f ] -> R.call "Filter" [ f; a ]
  | Fold, [ a; init; f ] -> R.call "Reduce" [ f; a; init ]
  | Sequence, [ a; b; s ] -> R.call "rp_seq" [ a; b; s ]
  | _ -> invalid_arg "Wrong arguments to a builtin"
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
  | Approx -> invalid_arg "~= lowers to rp_approx"
  | And -> Both_scalar
  | Or -> Either_scalar
;;

let checked names v = if Host.checked names then R.call "rp_checked" [ v ] else v

let constant : C.constant -> R.expr = function
  | Int i -> integer64 (R.String (Int64.to_string i))
  | Float f -> R.Number (R.number f)
  | Number s -> R.Number s
  | Text s -> R.String s
  | Bool b -> R.Logical b
  | Date d -> R.call "as.Date" [ R.String (Calendar.to_iso d) ]
;;

let rec value names (e : C.expr) = R.Located (e.span, node names e)

and node names (e : C.expr) : R.expr =
  let go = value names in
  let name id = R.Name (Host.name names id) in
  match e.node with
  | Constant c -> constant c
  | Absent -> R.Null
  | Variable { stamp = 0; name } ->
    (match Prelude.find name, T.repr e.typ with
     | Some (Row row), Arrow (params, _) ->
       let ps = List.mapi (fun i _ -> "x" ^ string_of_int (i + 1)) params in
       R.Function (ps, builtin row (List.map (fun p -> R.Name p) ps))
     | _ -> R.Name name)
  | Variable id -> name id
  | Builtin (row, args) -> builtin row (List.map go args)
  | Unary (Not, x) -> R.Not (go x)
  | Unary (Negate, x) -> R.Negate (go x)
  | Binary (Approx, a, b) -> R.call "rp_approx" [ go a; go b ]
  | Binary (((And | Or) as op), a, b) when T.is_optional a.typ || T.is_optional b.typ ->
    R.call (if op = And then "rp_and" else "rp_or") [ go a; go b ]
  | Binary (op, a, b) ->
    Option.value
      (Dplyr.date_arithmetic op a.typ b.typ (go a) (go b))
      ~default:(R.Binary (operator op, go a, go b))
  | Convert x when Host.checked names -> R.call "rp_exact_float" [ go x ]
  | Convert x -> number (go x)
  | Lift (bound, body) ->
    let tests = List.map (fun (id, _) -> is_null (name id)) bound in
    let test =
      List.fold_left
        (fun a b -> R.Binary (R.Either_scalar, a, b))
        (List.hd tests)
        (List.tl tests)
    in
    R.Block
      ( List.map (fun (id, x) -> Host.name names id, go x) bound
      , R.If (test, R.Null, go body) )
  | If (c, a, b) -> R.If (go c, go a, go b)
  | Coalesce (a, b) -> R.call "rp_coalesce" [ go a; go b ]
  | Require x -> go x
  | Match (subject, arms) -> match_ names subject arms
  | Lambda (ids, body) -> R.Function (List.map (Host.name names) ids, go body)
  | Apply (f, args) -> R.Call (go f, List.map (fun a -> R.Positional (go a)) args)
  | Construct (tag, args) -> R.call "rp_variant" (R.String tag :: List.map go args)
  | Let (id, v, body) -> R.Block ([ Host.name names id, go v ], go body)
  | Tuple xs | List xs -> R.call "list" (List.map go xs)
  | Sequence (a, b) -> R.call "rp_seq" [ go a; go b ]
  | Element (x, i) -> R.call "rp_element" [ go x; Option.fold ~none:R.Null ~some:go i ]
  | Slice (x, a, b) ->
    R.call
      "rp_slice"
      [ go x; Option.fold ~none:R.Null ~some:go a; Option.fold ~none:R.Null ~some:go b ]
  | Record fields ->
    R.Call (R.Name "list", List.map (fun (n, x) -> R.Named (n, go x)) fields)
  | Field (x, n) -> R.Element (go x, R.String n)
  | Member (x, set) ->
    R.call
      "any"
      [ R.call
          "vapply"
          [ go set
          ; R.Function ([ "item" ], R.call "identical" [ R.Name "item"; go x ])
          ; R.Logical true
          ]
      ]
  | Text pieces ->
    R.call
      "paste0"
      (List.map
         (function
           | C.Fixed s -> R.String s
           | Shown (x, Some spec) -> R.call "rp_format_number" [ go x; R.String spec ]
           | Shown (x, None) ->
             (match T.repr (T.present x.typ) with
              | String -> go x
              | Int -> R.call "as.character" [ go x ]
              | Float -> R.call "rp_float" [ go x ]
              | Bool -> R.If (go x, R.String "TRUE", R.String "FALSE")
              | _ -> R.call "rp_dynamic" [ go x ]))
         pieces)
  | Table t -> Dplyr.table names t
  | Foreign (m, f, _) ->
    Diagnostic.fail
      ~span:e.span
      Dialect_capability
      ("R target cannot execute the Python capability " ^ m ^ "." ^ f)
  | Reduce (r, policy, x) when x.shape = Scalar ->
    R.call
      "rp_vreduce"
      [ R.String (Prelude.name (Reduction r))
      ; R.String
          (Host.policy_name names policy)
      ; go x
      ]
  | Column _ | Reduce _ | Count ->
    Diagnostic.fail ~span:e.span Column_required "A column expression needs a table verb"

and match_ names subject arms =
  let go = value names in
  let rec pattern path : C.pattern -> R.expr list * (C.id * R.expr) list = function
    | Wildcard -> [], []
    | Bind id -> [], [ id, path ]
    | Named (id, p) ->
      let tests, bound = pattern path p in
      tests, (id, path) :: bound
    | Missing -> [ is_null path ], []
    | Present p ->
      let tests, bound = pattern path p in
      R.Not (is_null path) :: tests, bound
    | Equals c -> [ R.call "identical" [ path; constant c ] ], []
    | Between (low, high) ->
      ( Option.to_list
          (Option.map
             (fun (l, closed) ->
                R.Binary ((if closed then Less_equal else Less), constant l, path))
             low)
        @ Option.to_list
            (Option.map
               (fun (h, closed) ->
                  R.Binary ((if closed then Less_equal else Less), path, constant h))
               high)
      , [] )
    | Tag (tag, ps) ->
      let tests, bound = elements path ps in
      ( R.call "identical" [ R.call "attr" [ path; R.String "tag" ]; R.String tag ]
        :: tests
      , bound )
    | Elements ps -> elements path ps
    | Fields fields ->
      [], List.map (fun (n, id) -> id, R.Element (path, R.String n)) fields
    | Either alternatives ->
      let each = List.map (pattern path) alternatives in
      let bound = snd (List.hd each) in
      (match List.map (fun (tests, _) -> conjunction tests) each with
       | tests when List.mem None tests -> [], bound
       | first :: rest ->
         ( [ List.fold_left
               (fun a b -> R.Binary (Either_scalar, a, Option.get b))
               (Option.get first)
               rest
           ]
         , bound )
       | [] -> [], bound)
  and elements path ps =
    let tests, bound =
      List.mapi
        (fun i p -> pattern (R.Element (path, R.Number (string_of_int (i + 1)))) p)
        ps
      |> List.split
    in
    List.concat tests, List.concat bound
  in
  let subject_name = "subject" in
  let branch (arm : C.arm) rest =
    let tests, bound = pattern (R.Name subject_name) arm.pattern in
    let bind body =
      match bound with
      | [] -> body
      | _ -> R.Block (List.map (fun (id, path) -> Host.name names id, path) bound, body)
    in
    let tests = tests @ Option.to_list (Option.map (fun g -> bind (go g)) arm.guard) in
    let body = bind (go arm.body) in
    match conjunction tests with
    | None -> body
    | Some test -> R.If (test, body, rest)
  in
  R.Call
    ( R.Function
        ( [ subject_name ]
        , List.fold_right branch arms (R.call "stop" [ R.String "Non-exhaustive match" ])
        )
    , [ R.Positional (go subject) ] )
;;

let show names (e : C.expr) =
  match e.node with
  | Table t -> R.call "print" [ R.call "as_tibble" [ Dplyr.table names t ] ]
  | _ ->
    R.Call
      ( R.Name "cat"
      , [ R.Positional
            (R.call
               "rp_show"
               [ checked names (value names e)
               ; R.String (Yojson.Basic.to_string (Host.descriptor e.typ))
               ])
        ; R.Positional (R.String "\n")
        ; R.Named ("sep", R.String "")
        ] )
;;

let statement names : C.item -> R.statement option = function
  | Define (id, e) ->
    let v =
      match e.node with
      | Lambda _ | Table _ -> value names e
      | _ -> checked names (value names e)
    in
    Some (R.Assign (Host.name names id, v))
  | Variant _ -> None
  | Show e -> Some (R.Expression (show names e))
  | Write (e, path) ->
    (match e.node with
     | Table t ->
       Some
         (R.Expression
            (R.pipe (Dplyr.table names t) (R.call "rp_write" [ R.String path ])))
     | _ -> None)
;;

let statements names items = List.filter_map (statement names) items

let script items =
  let body = statements (names ()) items in
  let adapters = Adapters.r ^ if C.tabular items then Adapters.dplyr else "" in
  let code = fst (R.program body) in
  R.program (R.Verbatim (Preamble.used adapters code) :: statements (names ()) items)
;;

let result names (t : C.table) ~path =
  R.Expression
    (R.call "cat" [ R.call "rp_result" [ Dplyr.table names t; R.String path ] ])
;;
