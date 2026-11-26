module C = TransformIR
module T = Types
module P = Python_syntax

let reserved =
  [ "and"
  ; "as"
  ; "assert"
  ; "break"
  ; "class"
  ; "continue"
  ; "def"
  ; "del"
  ; "elif"
  ; "else"
  ; "except"
  ; "False"
  ; "finally"
  ; "for"
  ; "from"
  ; "global"
  ; "if"
  ; "import"
  ; "in"
  ; "is"
  ; "lambda"
  ; "None"
  ; "nonlocal"
  ; "not"
  ; "or"
  ; "pass"
  ; "raise"
  ; "return"
  ; "True"
  ; "try"
  ; "while"
  ; "with"
  ; "yield"
  ; "pl"
  ; "math"
  ; "re"
  ; "print"
  ; "len"
  ; "str"
  ; "list"
  ]
;;

let names () = Host.names reserved
let math name a = P.Call (P.Attribute (P.Name "math", name), [ P.Positional a ])
let as_float e = P.call "float" [ e ]
let is_none e = P.Binary (Is, e, P.Null)

let builtin (row : Prelude.row) args =
  match row, args with
  | Abs, [ a ] -> P.call "abs" [ a ]
  | Sign, [ a ] ->
    P.Call
      ( P.Lambda
          ( [ "x" ]
          , P.Binary
              ( Subtract
              , P.Binary (Greater, P.Name "x", P.Int "0")
              , P.Binary (Less, P.Name "x", P.Int "0") ) )
      , [ P.Positional a ] )
  | Sqrt, [ a ] -> math "sqrt" a
  | Exp, [ a ] -> math "exp" a
  | Log, [ a ] -> math "log" a
  | Log2, [ a ] -> math "log2" a
  | Log10, [ a ] -> math "log10" a
  | Floor, [ a ] -> as_float (math "floor" a)
  | Ceiling, [ a ] -> as_float (math "ceil" a)
  | Trunc, [ a ] -> as_float (math "trunc" a)
  | Round, [ a ] -> as_float (P.call "round" [ a ])
  | Pmin, [ a; b ] -> P.call "min" [ a; b ]
  | Pmax, [ a; b ] -> P.call "max" [ a; b ]
  | Present, [ a ] -> P.Binary (Is_not, a, P.Null)
  | Absent, [ a ] -> is_none a
  | Length, [ a ] -> P.call "len" [ a ]
  | Lower, [ a ] -> P.meth a "lower" []
  | Upper, [ a ] -> P.meth a "upper" []
  | Trim, [ a ] -> P.meth a "strip" []
  | Starts, [ a; b ] -> P.meth a "startswith" [ P.Positional b ]
  | Ends, [ a; b ] -> P.meth a "endswith" [ P.Positional b ]
  | Detect, [ a; b ] ->
    P.Binary
      ( Is_not
      , P.Call (P.Attribute (P.Name "re", "search"), [ P.Positional b; P.Positional a ])
      , P.Null )
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
  | And -> And
  | Or -> Or
;;

let constant : C.constant -> P.expr = function
  | Int i -> P.Int (Int64.to_string i)
  | Float f -> P.Float (P.float f)
  | Number s -> P.Int s
  | Text s -> P.Str s
  | Bool b -> P.Bool b
;;

let conjunction = function
  | [] -> None
  | t :: rest -> Some (List.fold_left (fun a b -> P.Binary (And, a, b)) t rest)
;;

let rec value names (e : C.expr) = P.Located (e.span, node names e)

and node names (e : C.expr) : P.expr =
  let go = value names in
  let name id = P.Name (Host.name names id) in
  let integer = T.repr (T.present e.typ) = Int in
  match e.node with
  | Constant c -> constant c
  | Absent -> P.Null
  | Variable { stamp = 0; name } ->
    (match Prelude.find name, T.repr e.typ with
     | Some (Row row), Arrow (params, _) ->
       let ps = List.mapi (fun i _ -> "x" ^ string_of_int (i + 1)) params in
       P.Lambda (ps, builtin row (List.map (fun p -> P.Name p) ps))
     | _ -> P.Name name)
  | Variable id -> name id
  | Builtin (row, args) -> builtin row (List.map go args)
  | Unary (Not, x) -> P.Unary (Not, go x)
  | Unary (Negate, x) when integer -> P.call "rp_int" [ P.Unary (Negate, go x) ]
  | Unary (Negate, x) -> P.Unary (Negate, go x)
  | Binary (((Add | Sub | Mul) as op), a, b) ->
    let v = P.Binary (operator op, go a, go b) in
    if integer then P.call "rp_int" [ v ] else P.call "rp_real" [ v ]
  | Binary (Power, a, b) -> as_float (P.Binary (Power, go a, go b))
  | Binary (op, a, b) -> P.Binary (operator op, go a, go b)
  | Convert x -> as_float (go x)
  | Lift (bound, body) ->
    let tests =
      List.map (fun (id, x) -> is_none (P.Walrus (Host.name names id, go x))) bound
    in
    P.If
      ( List.fold_left (fun a b -> P.Binary (Or, a, b)) (List.hd tests) (List.tl tests)
      , P.Null
      , go body )
  | If (c, a, b) -> P.If (go c, go a, go b)
  | Coalesce (a, b) ->
    P.If (is_none (P.Walrus ("fallback_value", go a)), go b, P.Name "fallback_value")
  | Require x -> go x
  | Match (subject, arms) -> match_ names subject arms
  | Lambda (ids, body) -> P.Lambda (List.map (Host.name names) ids, go body)
  | Apply (f, args) -> P.Call (go f, List.map (fun a -> P.Positional (go a)) args)
  | Construct (tag, args) -> P.Tuple (P.Str tag :: List.map go args)
  | Let (id, v, body) ->
    P.Call (P.Lambda ([ Host.name names id ], go body), [ P.Positional (go v) ])
  | Tuple xs -> P.Tuple (List.map go xs)
  | List xs -> P.List (List.map go xs)
  | Sequence (a, b) -> P.call "rp_seq" [ go a; go b ]
  | Element (x, i) -> P.call "rp_element" [ go x; Option.fold ~none:P.Null ~some:go i ]
  | Slice (x, a, b) ->
    P.call
      "rp_slice"
      [ go x; Option.fold ~none:P.Null ~some:go a; Option.fold ~none:P.Null ~some:go b ]
  | Record fields -> P.Dict (List.map (fun (n, x) -> P.Str n, go x) fields)
  | Field (x, n) -> P.Subscript (go x, P.Str n)
  | Member (x, set) -> P.Binary (In, go x, go set)
  | Text pieces ->
    let part = function
      | C.Fixed s -> P.Str s
      | Shown (x, Some spec) -> P.call "format" [ go x; P.Str spec ]
      | Shown (x, None) ->
        (match T.repr (T.present x.typ) with
         | String -> go x
         | Int -> P.call "str" [ go x ]
         | Float -> P.call "repr" [ go x ]
         | Bool -> P.If (go x, P.Str "TRUE", P.Str "FALSE")
         | _ -> P.call "rp_dynamic" [ go x ])
    in
    (match List.map part pieces with
     | [] -> P.Str ""
     | first :: rest -> List.fold_left (fun a b -> P.Binary (Add, a, b)) first rest)
  | Table t -> table names t
  | Foreign (m, f, _) ->
    Diagnostic.fail
      ~span:e.span
      Dialect_capability
      (m ^ "." ^ f ^ " needs the Polars table context")
  | Column _ | Reduce _ | Count ->
    Diagnostic.fail ~span:e.span Column_required "A column expression needs a table verb"

and table names t = Polars.table ~scalar:(value names) ~name:(Host.name names) t

and match_ names subject arms =
  let go = value names in
  let rec pattern path : C.pattern -> P.expr list * (C.id * P.expr) list = function
    | Wildcard -> [], []
    | Bind id -> [], [ id, path ]
    | Named (id, p) ->
      let tests, bound = pattern path p in
      tests, (id, path) :: bound
    | Missing -> [ is_none path ], []
    | Present p ->
      let tests, bound = pattern path p in
      P.Binary (Is_not, path, P.Null) :: tests, bound
    | Equals c -> [ P.Binary (Equal, path, constant c) ], []
    | Between (low, high) ->
      ( Option.to_list
          (Option.map
             (fun (l, closed) ->
                P.Binary ((if closed then Less_equal else Less), constant l, path))
             low)
        @ Option.to_list
            (Option.map
               (fun (h, closed) ->
                  P.Binary ((if closed then Less_equal else Less), path, constant h))
               high)
      , [] )
    | Tag (tag, ps) ->
      let tests, bound = elements path 1 ps in
      ( P.call "isinstance" [ path; P.Name "tuple" ]
        :: P.Binary (Equal, P.Subscript (path, P.Int "0"), P.Str tag)
        :: tests
      , bound )
    | Elements ps -> elements path 0 ps
    | Fields fields ->
      [], List.map (fun (n, id) -> id, P.Subscript (path, P.Str n)) fields
    | Either alternatives ->
      let each = List.map (pattern path) alternatives in
      let bound = snd (List.hd each) in
      (match List.map (fun (tests, _) -> conjunction tests) each with
       | tests when List.mem None tests -> [], bound
       | first :: rest ->
         ( [ List.fold_left
               (fun a b -> P.Binary (Or, a, Option.get b))
               (Option.get first)
               rest
           ]
         , bound )
       | [] -> [], bound)
  and elements path offset ps =
    let tests, bound =
      List.mapi
        (fun i p -> pattern (P.Subscript (path, P.Int (string_of_int (i + offset)))) p)
        ps
      |> List.split
    in
    List.concat tests, List.concat bound
  in
  let branch (arm : C.arm) rest =
    let tests, bound = pattern (P.Name "subject") arm.pattern in
    let bind body =
      match bound with
      | [] -> body
      | _ ->
        P.Call
          ( P.Lambda (List.map (fun (id, _) -> Host.name names id) bound, body)
          , List.map (fun (_, path) -> P.Positional path) bound )
    in
    let tests = tests @ Option.to_list (Option.map (fun g -> bind (go g)) arm.guard) in
    let body = bind (go arm.body) in
    match conjunction tests with
    | None -> body
    | Some test -> P.If (test, body, rest)
  in
  P.Call
    ( P.Lambda ([ "subject" ], List.fold_right branch arms P.Null)
    , [ P.Positional (go subject) ] )
;;

let collected names (t : C.table) =
  match table names t with
  | P.Chain (base, links) -> P.Chain (base, links @ [ P.link "collect" [] ])
  | e -> P.Chain (e, [ P.link "collect" [] ])
;;

let statement names : C.item -> P.statement option = function
  | Define (id, e) -> Some (P.Assign (Host.name names id, value names e))
  | Variant _ -> None
  | Show { node = Table t; _ } ->
    Some (P.Expression (P.call "print" [ collected names t ]))
  | Show e ->
    Some
      (P.Expression
         (P.call
            "print"
            [ P.call
                "rp_show"
                [ value names e; P.Str (Yojson.Basic.to_string (Host.descriptor e.typ)) ]
            ]))
  | Write ({ node = Table t; _ }, path) ->
    Some
      (P.Expression
         (match collected names t with
          | P.Chain (base, links) ->
            P.Chain
              (base, links @ [ P.link "write_parquet" [ P.Positional (P.Str path) ] ])
          | e -> e))
  | Write _ -> None
;;

let statements names items = List.filter_map (statement names) items

let preamble items =
  P.Verbatim Adapters.python
  :: (if C.tabular items then [ P.Verbatim Adapters.polars ] else [])
;;

let script items =
  let names = names () in
  P.program (preamble items @ statements names items)
;;

let result names (t : C.table) ~path =
  P.Expression (P.call "print" [ P.call "rp_result" [ table names t; P.Str path ] ])
;;
