open Syntax
module T = Types
module C = Core

type template =
  { params : string list
  ; body : Syntax.expr
  ; closure : env
  }

and binding =
  | Value of
      { id : C.id
      ; scheme : T.scheme
      }
  | Table_value of
      { id : C.id
      ; table : C.table
      }
  | Function of
      { id : C.id option
      ; scheme : T.scheme option
      ; template : template
      }
  | Constructor of
      { scheme : T.scheme
      ; arity : int
      }
  | Inline of C.expr
  | Contract of Contract.t
  | Module of string

and env =
  { names : (string * binding) list
  ; types : (string * T.scheme) list
  ; variants : (string * (string * T.scheme) list) list
  ; missing : C.policy option
  ; group_missing : C.policy option
  ; directory : string
  ; strict : bool
  }

type ctx =
  { env : env
  ; level : int
  ; columns : T.row option
  ; missing : C.policy option
  }

let empty ~directory ~strict =
  { names = []
  ; types = []
  ; variants = []
  ; missing = None
  ; group_missing = None
  ; directory
  ; strict
  }
;;

let stamp = ref 0

let fresh_id name =
  incr stamp;
  { C.name; stamp = !stamp }
;;

let fail span code message = Diagnostic.fail ~span code message
let bind env name binding = { env with names = (name, binding) :: env.names }

let expect span expected actual =
  try T.unify expected actual with
  | T.Mismatch ->
    fail
      span
      Type_mismatch
      (Printf.sprintf "Expected %s, found %s" (T.display expected) (T.display actual))
;;

let make span shape typ node = { C.node; typ; shape; span }

let align span a b =
  match a, b with
  | C.Scalar, s | s, C.Scalar -> s
  | Columnar, Columnar ->
    ignore span;
    C.Columnar
;;

let shape_of span xs =
  List.fold_left (fun s (x : C.expr) -> align span s x.shape) C.Scalar xs
;;

let closest name candidates =
  List.map (fun n -> Distance.between n name, n) candidates
  |> List.sort compare
  |> function
  | (d, n) :: _ when d <= 2 -> Some n
  | _ -> None
;;

let did_you_mean = function
  | Some n -> ". Did you mean " ^ n ^ "?"
  | None -> ""
;;

let column level span row name =
  match T.field level row name with
  | Some t -> t
  | None ->
    fail
      span
      Unknown_column
      ("No column named "
       ^ name
       ^ did_you_mean (closest name (List.map fst (T.known row))))
;;

let exact_float span s =
  match Int64.of_string_opt s with
  | Some i when Int64.to_float i < 2. ** 63. && Int64.of_float (Int64.to_float i) = i ->
    Int64.to_float i
  | _ -> fail span Inexact_float (s ^ " is not exactly representable as Float64")
;;

let constant span = function
  | Syntax.Int s ->
    (match Int64.of_string_opt s with
     | Some _ -> C.Number s
     | None -> fail span Int64_range "Integer literal is outside the Int64 range")
  | Float s -> Float (float_of_string s)
  | Text s -> Text s
  | Bool b -> Bool b
  | Absent -> assert false
;;

let to_float (x : C.expr) =
  let typ = if T.is_optional x.typ then T.Option T.Float64 else T.Float64 in
  match T.repr (T.present x.typ), x.node with
  | Float64, _ -> x
  | Var _, Constant (Number _) ->
    expect x.span T.Float64 (T.present x.typ);
    x
  | Int64, Constant (Int i) ->
    { x with node = Constant (Float (exact_float x.span (Int64.to_string i))); typ }
  | (Int64 | Var _), _ ->
    expect x.span (T.fresh ~kind:Number max_int) (T.present x.typ);
    { x with node = Convert x; typ }
  | _ ->
    expect x.span T.Float64 (T.present x.typ);
    x
;;

let common span (a : C.expr) (b : C.expr) =
  let ea = T.present a.typ
  and eb = T.present b.typ in
  match T.repr ea, T.repr eb with
  | Int64, Float64 -> to_float a, b, T.Float64
  | Float64, Int64 -> a, to_float b, T.Float64
  | _ ->
    expect span ea eb;
    a, b, ea
;;

let lift span shape operands build =
  let optional = List.exists (fun (x : C.expr) -> T.is_optional x.typ) operands in
  if (not optional) || shape = C.Columnar
  then (
    let body : C.expr = build operands in
    if optional then { body with typ = T.optional body.typ } else body)
  else (
    let bound, values =
      List.map
        (fun (x : C.expr) ->
           if T.is_optional x.typ
           then (
             let id = fresh_id "v" in
             Some (id, x), make x.span x.shape (T.present x.typ) (C.Variable id))
           else None, x)
        operands
      |> List.split
    in
    let body : C.expr = build values in
    make span shape (T.optional body.typ) (Lift (List.filter_map Fun.id bound, body)))
;;

let row_signature level (row : Prelude.row) =
  let number () = T.fresh ~kind:Number level in
  match row with
  | Abs ->
    let a = number () in
    [ a ], a
  | Sign -> [ number () ], T.Int64
  | Sqrt | Exp | Log | Log2 | Log10 | Floor | Ceiling | Trunc | Round ->
    [ T.Float64 ], T.Float64
  | Pmin | Pmax ->
    let a = number () in
    [ a; a ], a
  | Present | Absent -> [ T.Option (T.fresh level) ], T.Bool
  | Length -> [ T.String ], T.Int64
  | Lower | Upper | Trim -> [ T.String ], T.String
  | Starts | Ends | Detect -> [ T.String; T.String ], T.Bool
;;

let builtin_type level = function
  | Prelude.Row row ->
    let args, result = row_signature level row in
    T.Arrow (args, result)
  | Reduction (Sum | Min | Max) ->
    let a = T.fresh ~kind:Number level in
    T.Arrow ([ T.Option a ], T.Option a)
  | Reduction (Mean | Median | Var | Sd) ->
    T.Arrow ([ T.Option (T.fresh ~kind:Number level) ], T.Option T.Float64)
  | Reduction Distinct -> T.Arrow ([ T.Option (T.fresh level) ], T.Int64)
  | Reduction Count -> T.Arrow ([], T.Int64)
  | Coalesce ->
    let a = T.fresh level in
    T.Arrow ([ T.Option a; a ], a)
;;

let rec annotation env params level = function
  | Arow (fields, rest) ->
    T.Record
      { fields = List.map (fun (n, a) -> n, annotation env params level a) fields
      ; rest =
          Option.map
            (fun n -> Option.value (List.assoc_opt n params) ~default:(T.fresh level))
            rest
      }
  | Atuple ts -> T.Tuple (List.map (annotation env params level) ts)
  | Aarrow (a, b) ->
    T.Arrow ([ annotation env params level a ], annotation env params level b)
  | Optional a -> T.optional (annotation env params level a)
  | Named (n, args) ->
    let args = List.map (annotation env params level) args in
    (match n, args with
     | "Int64", [] -> T.Int64
     | "Float64", [] -> T.Float64
     | "String", [] -> T.String
     | "Bool", [] -> T.Bool
     | "List", [ t ] -> T.List t
     | "Option", [ t ] -> T.optional t
     | "Table", [ T.Record row ] -> T.Table row
     | n, [] when List.mem_assoc n params -> List.assoc n params
     | n, args ->
       (match List.assoc_opt n env.types with
        | Some scheme ->
          (match T.instantiate level scheme with
           | T.Arrow (ps, t) when List.length ps = List.length args ->
             List.iter2 T.unify ps args;
             t
           | t when args = [] -> t
           | _ -> Diagnostic.fail Unknown_type ("Wrong number of type arguments to " ^ n))
        | None -> Diagnostic.fail Unknown_type ("Unknown type " ^ n)))
;;

let uncovered env level subject (arms : Syntax.arm list) =
  let heads t =
    match T.repr t with
    | Bool -> [ `Constant (Syntax.Bool true), []; `Constant (Syntax.Bool false), [] ]
    | Option inner -> [ `Constant Syntax.Absent, []; `Constructor "Some", [ inner ] ]
    | Tuple fields -> [ `Product, fields ]
    | Variant (name, _) ->
      List.assoc name env.variants
      |> List.map (fun (tag, scheme) ->
        let fields, result =
          match T.instantiate level scheme with
          | Arrow (fields, result) -> fields, result
          | result -> [], result
        in
        T.unify t result;
        `Constructor tag, fields)
    | _ -> []
  in
  let rec catchall = function
    | Wildcard | Bind _ | Precord _ -> true
    | Pas (_, p) -> catchall p
    | Palt ps -> List.exists catchall ps
    | Tag _ | Ptuple _ | Pliteral _ | Prange _ -> false
  in
  let rec expand rows =
    List.concat_map
      (function
        | Pas (_, p) :: ps -> expand [ p :: ps ]
        | Palt alts :: ps -> expand (List.map (fun p -> p :: ps) alts)
        | row -> [ row ])
      rows
  in
  let covers t rows =
    let integral = T.repr t = Int64 in
    let number = function
      | Syntax.Int s | Float s -> float_of_string s
      | _ -> nan
    in
    let interval = function
      | Prange { low; high; inclusive } ->
        let low = Option.fold ~none:neg_infinity ~some:number low in
        let high, closed =
          match high with
          | None -> infinity, false
          | Some h when integral && inclusive -> number h +. 1., false
          | Some h -> number h, inclusive
        in
        Some (low, high, closed)
      | Pliteral ((Syntax.Int _ | Float _) as l) ->
        let x = number l in
        Some (if integral then x, x +. 1., false else x, x, true)
      | _ -> None
    in
    let reach, _ =
      List.filter_map
        (function
          | p :: rest when List.for_all catchall rest -> interval p
          | _ -> None)
        rows
      |> List.sort compare
      |> List.fold_left
           (fun (reach, closed) (low, high, high_closed) ->
              if low <= reach
              then
                if high > reach
                then high, high_closed
                else reach, closed || (high = reach && high_closed)
              else reach, closed)
           (neg_infinity, false)
    in
    reach = infinity
  in
  let specialize head arity = function
    | p :: ps when catchall p -> Some (List.init arity (fun _ -> Wildcard) @ ps)
    | p :: ps ->
      (match head, p with
       | `Constructor n, Tag (m, vs) when n = m -> Some (vs @ ps)
       | `Constant l, Pliteral m when l = m -> Some ps
       | `Product, Ptuple vs -> Some (vs @ ps)
       | _ -> None)
    | [] -> None
  in
  let rebuild head arity witness =
    let args = List.filteri (fun i _ -> i < arity) witness
    and rest = List.filteri (fun i _ -> i >= arity) witness in
    (match head with
     | `Constructor n -> Tag (n, args)
     | `Constant l -> Pliteral l
     | `Product -> Ptuple args)
    :: rest
  in
  let rec missing types rows =
    let rows = expand rows in
    match types with
    | [] -> if rows = [] then Some [] else None
    | t :: rest ->
      let wildcard () =
        List.filter_map
          (function
            | p :: ps when catchall p -> Some ps
            | _ -> None)
          rows
        |> missing rest
        |> Option.map (List.cons Wildcard)
      in
      let all_catchall =
        List.for_all
          (function
            | p :: _ -> catchall p
            | [] -> false)
          rows
      in
      (match heads t with
       | [] when (not all_catchall) && covers t rows ->
         missing rest [ List.map (fun _ -> Wildcard) rest ]
       | [] -> wildcard ()
       | _ when all_catchall -> wildcard ()
       | cases ->
         List.find_map
           (fun (head, fields) ->
              let arity = List.length fields in
              missing (fields @ rest) (List.filter_map (specialize head arity) rows)
              |> Option.map (rebuild head arity))
           cases)
  in
  let unguarded = List.filter (fun (arm : Syntax.arm) -> arm.guard = None) arms in
  match
    missing [ subject ] (List.map (fun (arm : Syntax.arm) -> [ arm.pattern ]) unguarded)
  with
  | Some [ p ] -> Some p
  | _ -> None
;;

let policy_of (x : Syntax.expr) =
  match x.form with
  | Name (_, n) when List.mem_assoc n C.policies -> List.assoc n C.policies
  | _ -> fail x.span Invalid_policy "A missing policy is skip, propagate or error"
;;

let rec expr ctx (x : Syntax.expr) : C.expr =
  let span = x.span in
  let make = make span in
  let here = expr ctx in
  match x.form with
  | Literal Absent -> make Scalar (T.Option (T.fresh ctx.level)) Absent
  | Literal (Int _ as l) ->
    make Scalar (T.fresh ~kind:Number ctx.level) (Constant (constant span l))
  | Literal l ->
    let c = constant span l in
    make
      Scalar
      (match c with
       | Float _ -> T.Float64
       | Text _ -> T.String
       | Bool _ -> T.Bool
       | Int _ | Number _ -> T.Int64)
      (Constant c)
  | Name (reference, n) -> name ctx span reference n
  | Annotated (inner, a) ->
    let v = here inner in
    let t = annotation ctx.env [] ctx.level a in
    (match T.repr t, T.repr v.typ with
     | Option _, (Option _ | Var _) -> expect span t v.typ
     | Option inner, _ -> expect span inner v.typ
     | _ -> expect span t v.typ);
    v
  | Call ({ form = Name (Bare, "Some"); _ }, [ (None, v) ]) ->
    let v = here v in
    { v with typ = T.optional v.typ }
  | Call ({ form = Field ({ form = Name (Bare, m); _ }, f); _ }, args)
    when match List.assoc_opt m ctx.env.names with
         | Some (Module _) -> true
         | _ -> false -> foreign ctx span m f args
  | Call (f, args) -> call ctx span f args
  | Pipe (v, { form = Call (f, args); _ }) -> call ctx span f ((None, v) :: args)
  | Pipe (v, f) -> call ctx span f [ None, v ]
  | Unary (op, v) ->
    let v = here v in
    (match op with
     | Not -> expect span T.Bool (T.present v.typ)
     | Negate -> expect span (T.fresh ~kind:Number ctx.level) (T.present v.typ));
    lift span v.shape [ v ] (function
      | [ v ] -> make v.shape (T.present v.typ) (Unary (op, v))
      | _ -> assert false)
  | Binary (op, a, b) -> binary ctx span op (here a) (here b)
  | If (c, a, b) ->
    let c = here c in
    if T.is_optional c.typ
    then
      fail
        c.span
        Optional_condition
        "This condition is missing where its inputs are missing. Wrap it in \
         coalesce(condition, FALSE) or coalesce(condition, TRUE)";
    expect c.span T.Bool c.typ;
    let a, b, t = common span (here a) (here b) in
    let optional = T.is_optional a.typ || T.is_optional b.typ in
    make (shape_of span [ c; a; b ]) (if optional then T.optional t else t) (If (c, a, b))
  | Member (v, set) ->
    let v = here v
    and set = here set in
    expect span (T.List (T.present v.typ)) set.typ;
    if ctx.columns <> None
    then (
      match set.node with
      | List _ -> ()
      | _ -> fail set.span Wrong_arguments "%in% needs a list of values");
    lift span v.shape [ v ] (function
      | [ v ] -> make v.shape T.Bool (Member (v, set))
      | _ -> assert false)
  | Interpolate pieces -> interpolate ctx span pieces
  | List xs ->
    let t = T.fresh ctx.level in
    let xs =
      List.map
        (fun x ->
           let x = here x in
           expect x.span t x.typ;
           x)
        xs
    in
    make Scalar (T.List t) (List xs)
  | Tuple xs ->
    let xs = List.map here xs in
    make Scalar (T.Tuple (List.map (fun (x : C.expr) -> x.typ) xs)) (Tuple xs)
  | Record fields ->
    let names = List.map fst fields in
    if List.length names <> List.length (List.sort_uniq compare names)
    then fail span Duplicate_name "A record repeats a field name";
    let fields = List.map (fun (n, v) -> n, here v) fields in
    make
      Scalar
      (T.Record (T.closed (List.map (fun (n, (v : C.expr)) -> n, v.typ) fields)))
      (Record fields)
  | Field (v, n) ->
    let v = here v in
    let t = T.fresh ctx.level in
    expect span (T.Record { fields = [ n, t ]; rest = Some (T.fresh ctx.level) }) v.typ;
    make v.shape t (Field (v, n))
  | Local (n, v, body) ->
    (match ctx.columns with
     | Some _ ->
       let v = here v in
       expr { ctx with env = bind ctx.env n (Inline v) } body
     | None ->
       let v = expr { ctx with level = ctx.level + 1 } v in
       let id = fresh_id n in
       let body =
         expr
           { ctx with
             env = bind ctx.env n (Value { id; scheme = T.generalize ctx.level v.typ })
           }
           body
       in
       make body.shape body.typ (Let (id, v, body)))
  | Lambda (params, body) ->
    let ids = List.map fresh_id params in
    let types = List.map (fun _ -> T.fresh ctx.level) params in
    let env =
      List.fold_left2
        (fun env (p, id) t -> bind env p (Value { id; scheme = T.mono t }))
        ctx.env
        (List.combine params ids)
        types
    in
    let body = expr { ctx with env } body in
    make Scalar (T.Arrow (types, body.typ)) (Lambda (ids, body))
  | Match (subject, arms) -> match_ ctx span (here subject) arms
  | Range _ -> fail span R_range "`a:b` selects columns. For integers write `a..=b`"
  | Formula _ ->
    fail
      span
      Unsupported_feature
      "Formulas (y ~ x) are reserved for the model and graphics libraries"

and name ctx span reference n =
  let make = make span in
  let as_column t = make Columnar t (C.Column n) in
  let lexical () =
    match List.assoc_opt n ctx.env.names with
    | Some (Value { id; scheme }) ->
      Some (make Scalar (T.instantiate ctx.level scheme) (Variable id))
    | Some (Inline v) -> Some v
    | Some (Table_value { id; table }) ->
      Some
        (make
           Scalar
           (T.Table (T.closed table.schema))
           (Table { table with source = Bound id; stages = [] }))
    | Some (Function { id = Some id; scheme = Some scheme; _ }) ->
      Some (make Scalar (T.instantiate ctx.level scheme) (Variable id))
    | Some (Constructor { scheme; arity = 0 }) ->
      Some (make Scalar (T.instantiate ctx.level scheme) (Construct (n, [])))
    | Some (Function _ | Constructor _ | Contract _ | Module _) | None -> None
  in
  let known = Option.fold ~none:[] ~some:T.known ctx.columns in
  match reference, ctx.columns with
  | Column, Some row -> as_column (column ctx.level span row n)
  | Column, None -> fail span Unknown_column ("No table is in scope for $" ^ n)
  | Bare, Some _ when List.mem_assoc n known ->
    (match List.assoc_opt n ctx.env.names with
     | Some (Value _ | Inline _) ->
       fail
         span
         Ambiguous_name
         (n
          ^ " is both a column and a binding. Write $"
          ^ n
          ^ " for the column or ^"
          ^ n
          ^ " for the binding")
     | _ -> as_column (List.assoc n known))
  | (Bare | Lexical), _ ->
    (match lexical (), ctx.columns with
     | Some v, _ -> v
     | None, Some row
       when reference = Bare
            && Option.is_none (Prelude.find n)
            && Option.is_some (T.field ctx.level row n) ->
       as_column (Option.get (T.field ctx.level row n))
     | None, _ ->
       (match Prelude.find n with
        | Some b ->
          make Scalar (builtin_type ctx.level b) (Variable { name = n; stamp = 0 })
        | None ->
          let columns = List.map fst known in
          (match closest n (columns @ List.map fst ctx.env.names) with
           | Some m when List.mem m columns ->
             fail span Unknown_column ("No column named " ^ n ^ did_you_mean (Some m))
           | hint -> fail span Unknown_name ("Unknown name: " ^ n ^ did_you_mean hint))))

and binary ctx span op a b =
  let make = make span in
  let number t = expect span (T.fresh ~kind:Number ctx.level) t in
  let a, b, result =
    match op with
    | Div | Power ->
      number (T.present a.typ);
      number (T.present b.typ);
      to_float a, to_float b, T.Float64
    | _ ->
      let a, b, t = common span a b in
      ( a
      , b
      , (match op with
         | And | Or ->
           expect span T.Bool t;
           T.Bool
         | Equal | Not_equal -> T.Bool
         | Less | Greater | Less_equal | Greater_equal ->
           (match T.repr t with
            | String -> ()
            | _ -> number t);
           T.Bool
         | Add | Sub | Mul | Mod ->
           number t;
           t
         | Div | Power -> assert false) )
  in
  let shape = shape_of span [ a; b ] in
  lift span shape [ a; b ] (function
    | [ a; b ] -> make shape result (Binary (op, a, b))
    | _ -> assert false)

and arguments ctx span callee args =
  let missing =
    match List.assoc_opt (Some "missing") args with
    | Some p -> Some (policy_of p)
    | None -> ctx.missing
  in
  ( missing
  , List.filter_map
      (function
        | Some "missing", _ -> None
        | None, a -> Some a
        | Some label, _ ->
          fail span Unknown_option (callee ^ " takes no argument named " ^ label))
      args )

and call ctx span (f : Syntax.expr) args =
  let make = make span in
  match f.form with
  | Name (Bare, n) when List.mem_assoc n Core.join_kinds || Verbs.mem n ->
    (match args with
     | (None, input) :: rest ->
       make Scalar (T.fresh ctx.level) (verb ctx span n input rest)
     | _ -> fail span Wrong_arguments (n ^ " needs a table"))
    |> fun e -> retype_table e
  | Name (Bare, ("read_csv" | "read_tsv" | "read_parquet" | "load")) ->
    retype_table (make Scalar (T.fresh ctx.level) (Table (source ctx span f args)))
  | Name (Bare, n)
    when (not (List.mem_assoc n ctx.env.names)) && Option.is_some (Prelude.find n) ->
    builtin ctx span n (Option.get (Prelude.find n)) args
  | Name (Bare, n) ->
    (match List.assoc_opt n ctx.env.names with
     | Some (Function { template; id; scheme }) ->
       let _, values = arguments ctx span n args in
       let values = List.map (expr ctx) values in
       if List.length values <> List.length template.params
       then
         fail
           span
           Wrong_arguments
           (Printf.sprintf
              "%s takes %d argument%s"
              n
              (List.length template.params)
              (if List.length template.params = 1 then "" else "s"));
       let inline =
         ctx.columns <> None
         || scheme = None
         || List.exists
              (fun (v : C.expr) ->
                 v.shape = Columnar
                 ||
                 match T.repr v.typ with
                 | Table _ -> true
                 | _ -> false)
              values
       in
       (match inline, id, scheme with
        | false, Some id, Some scheme ->
          apply
            ctx
            span
            (make Scalar (T.instantiate ctx.level scheme) (Variable id))
            values
        | _ ->
          let closure = template.closure in
          let env =
            List.fold_left2
              (fun env p v -> bind env p (Inline v))
              closure
              template.params
              values
          in
          let missing =
            match closure.missing with
            | Some p -> Some p
            | None ->
              (match List.assoc_opt (Some "missing") args with
               | Some p -> Some (policy_of p)
               | None -> ctx.missing)
          in
          expr
            { ctx with
              env
            ; missing
            ; columns = Option.map (fun _ -> T.closed []) ctx.columns
            }
            template.body)
     | Some (Constructor { scheme; arity }) ->
       let values = List.map (fun (_, a) -> expr ctx a) args in
       if List.length values <> arity
       then fail span Wrong_arguments (Printf.sprintf "%s takes %d fields" n arity);
       (match T.instantiate ctx.level scheme with
        | Arrow (params, result) ->
          List.iter2 (fun p (v : C.expr) -> expect v.span p v.typ) params values;
          make (shape_of span values) result (Construct (n, values))
        | t -> make Scalar t (Construct (n, [])))
     | _ -> apply ctx span (expr ctx f) (List.map (fun (_, a) -> expr ctx a) args))
  | Lambda (params, body)
    when List.length params = List.length args && ctx.columns <> None ->
    let env =
      List.fold_left2
        (fun env p (_, a) -> bind env p (Inline (expr ctx a)))
        ctx.env
        params
        args
    in
    expr { ctx with env } body
  | _ -> apply ctx span (expr ctx f) (List.map (fun (_, a) -> expr ctx a) args)

and apply ctx span (f : C.expr) values =
  let make = make span in
  match T.repr f.typ with
  | Arrow (params, result) when List.length params = List.length values ->
    let values = List.map2 (coerce ctx) params values in
    let shape = shape_of span values in
    let lifted =
      List.map2
        (fun p (v : C.expr) -> T.is_optional v.typ && not (T.is_optional p))
        params
        values
    in
    if List.mem true lifted && shape = Scalar
    then (
      let bound, args =
        List.map2
          (fun l (v : C.expr) ->
             if l
             then (
               let id = fresh_id "v" in
               Some (id, v), { v with node = Variable id; typ = T.present v.typ })
             else None, v)
          lifted
          values
        |> List.split
      in
      make
        Scalar
        (T.optional result)
        (Lift (List.filter_map Fun.id bound, make Scalar result (Apply (f, args)))))
    else
      make
        shape
        (if List.mem true lifted then T.optional result else result)
        (Apply (f, values))
  | Arrow (params, _) ->
    fail
      span
      Wrong_arguments
      (Printf.sprintf
         "Expected %d argument%s, got %d"
         (List.length params)
         (if List.length params = 1 then "" else "s")
         (List.length values))
  | _ ->
    let result = T.fresh ctx.level in
    expect span (T.Arrow (List.map (fun (v : C.expr) -> v.typ) values, result)) f.typ;
    make (shape_of span values) result (Apply (f, values))

and coerce _ctx param (v : C.expr) =
  let element = T.present v.typ in
  match T.repr param, T.repr element with
  | (Float64 | Option Float64), Int64 -> to_float v
  | Option inner, _ when not (T.is_optional v.typ) ->
    expect v.span inner element;
    v
  | Option _, _ ->
    expect v.span param v.typ;
    v
  | _ ->
    expect v.span param element;
    v

and builtin ctx span n b args =
  let make = make span in
  let missing, values = arguments ctx span n args in
  let values = List.map (expr ctx) values in
  match b, values with
  | Prelude.Coalesce, [ v; fallback ] ->
    let v, fallback, t = common span v fallback in
    make
      (shape_of span [ v; fallback ])
      (if T.is_optional fallback.typ then T.optional t else t)
      (Coalesce (v, fallback))
  | Row row, values ->
    let params, result = row_signature ctx.level row in
    if List.length params <> List.length values
    then fail span Wrong_arguments ("Wrong arguments to " ^ n);
    let values = List.map2 (coerce ctx) params values in
    let values =
      match row, values with
      | (Pmin | Pmax), [ a; b ] ->
        let a, b, _ = common span a b in
        [ a; b ]
      | _ -> values
    in
    let shape = shape_of span values in
    (match row with
     | Present | Absent -> make shape T.Bool (Builtin (row, values))
     | _ ->
       lift span shape values (fun values ->
         make shape (T.resolve result) (Builtin (row, values))))
  | Reduction Count, [] ->
    if ctx.columns = None then fail span Column_required "n() counts the rows of a table";
    make Scalar T.Int64 Count
  | Reduction r, [ v ] ->
    if v.shape <> Columnar
    then fail span Column_required (n ^ " needs a column, not a single value");
    let policy =
      match missing, T.is_optional v.typ with
      | Some p, true -> p
      | _, false -> C.Skip
      | None, true ->
        fail
          span
          Missing_policy_required
          (n
           ^ " reads a column that can be missing. Say what to do with missing values: \
              missing = skip ignores them, missing = propagate makes the result missing, \
              missing = error stops the run")
    in
    let element = T.present v.typ in
    let v, result =
      match r with
      | Sum | Min | Max ->
        expect span (T.fresh ~kind:Number ctx.level) element;
        v, T.Option element
      | Mean | Median | Var | Sd -> to_float v, T.Option T.Float64
      | Distinct -> v, if policy = Propagate then T.Option T.Int64 else T.Int64
      | Count -> v, T.Int64
    in
    make Scalar result (Reduce (r, policy, v))
  | _ -> fail span Wrong_arguments ("Wrong arguments to " ^ n)

and foreign ctx span m f args =
  let values = List.map (fun (_, a) -> expr ctx a) args in
  match m, f, values with
  | "polars", "sqrt", [ v ] ->
    let v = to_float v in
    make span v.shape v.typ (Foreign ("polars", "sqrt", [ v ]))
  | _ -> fail span Unknown_name (Printf.sprintf "%s has no function %s" m f)

and interpolate ctx span pieces =
  let pieces =
    List.concat_map
      (function
        | Text_piece s -> [ `Fixed s ]
        | Slot { value; format; echo } ->
          let v = expr ctx value in
          let element = T.present v.typ in
          (match T.repr element, format with
           | (Int64 | Float64), _ | Var _, None | (Bool | String), None -> ()
           | Var _, Some _ -> expect span (T.fresh ~kind:Number ctx.level) element
           | String, Some f
             when not (String.exists (fun c -> String.contains "dfeg%" c) f) -> ()
           | _, Some _ ->
             fail v.span Type_mismatch "A format spec like :.2f applies to numbers"
           | t, None ->
             fail
               v.span
               Type_mismatch
               ("Interpolation shows numbers, text and Bool, not " ^ T.display t));
          Option.to_list (Option.map (fun e -> `Fixed e) echo) @ [ `Shown (v, format) ])
      pieces
  in
  let shown =
    List.filter_map
      (function
        | `Shown (v, _) -> Some v
        | `Fixed _ -> None)
      pieces
  in
  let shape = shape_of span shown in
  lift span shape shown (fun values ->
    let rec rebuild values = function
      | [] -> []
      | `Fixed s :: rest -> C.Fixed s :: rebuild values rest
      | `Shown (_, format) :: rest ->
        (match values with
         | v :: values -> C.Shown (v, format) :: rebuild values rest
         | [] -> assert false)
    in
    make span shape T.String (Text (rebuild values pieces)))

and match_ ctx span (subject : C.expr) syntax_arms =
  let result = T.fresh ctx.level in
  let rec pattern t = function
    | Syntax.Wildcard -> C.Wildcard, []
    | Bind n ->
      let id = fresh_id n in
      C.Bind id, [ n, id, t ]
    | Pas (n, p) ->
      let id = fresh_id n in
      let p, vars = pattern t p in
      C.Named (id, p), (n, id, t) :: vars
    | Pliteral Absent ->
      expect span (T.Option (T.fresh ctx.level)) t;
      C.Missing, []
    | Pliteral l ->
      let c = constant span l in
      (match c with
       | Number _ -> expect span (T.fresh ~kind:Number ctx.level) (T.present t)
       | Float _ -> expect span T.Float64 (T.present t)
       | Text _ -> expect span T.String (T.present t)
       | Bool _ -> expect span T.Bool (T.present t)
       | Int _ -> ());
      C.Equals c, []
    | Prange { low; high; inclusive } ->
      expect span (T.fresh ~kind:Number ctx.level) (T.present t);
      List.iter
        (function
          | Syntax.Float _ -> expect span T.Float64 (T.present t)
          | _ -> ())
        (Option.to_list low @ Option.to_list high);
      let bound = Option.map (constant span) in
      C.Between (bound low, bound high, inclusive), []
    | Precord names ->
      let fields = List.map (fun n -> n, T.fresh ctx.level) names in
      expect span (T.Record { fields; rest = Some (T.fresh ctx.level) }) t;
      let ids = List.map (fun (n, ft) -> n, fresh_id n, ft) fields in
      C.Fields (List.map (fun (n, id, _) -> n, id) ids), ids
    | Ptuple ps ->
      let ts = List.map (fun _ -> T.fresh ctx.level) ps in
      expect span (T.Tuple ts) t;
      let ps, vars = List.split (List.map2 pattern ts ps) in
      C.Elements ps, List.concat vars
    | Palt [] -> C.Either [], []
    | Palt (first :: rest) ->
      let p, vars = pattern t first in
      let others =
        List.map
          (fun alt ->
             let q, more = pattern t alt in
             if
               List.sort compare (List.map (fun (n, _, _) -> n) more)
               <> List.sort compare (List.map (fun (n, _, _) -> n) vars)
             then fail span Pattern_bindings "Every alternative must bind the same names";
             q)
          rest
      in
      C.Either (p :: others), vars
    | Tag ("Some", [ p ]) ->
      let inner = T.fresh ctx.level in
      expect span (T.Option inner) t;
      let p, vars = pattern inner p in
      C.Present p, vars
    | Tag ("None", []) ->
      expect span (T.Option (T.fresh ctx.level)) t;
      C.Missing, []
    | Tag (tag, ps) ->
      let scheme =
        match List.assoc_opt tag ctx.env.names with
        | Some (Constructor { scheme; _ }) -> scheme
        | _ -> fail span Unknown_constructor ("Unknown constructor " ^ tag)
      in
      let fields, variant =
        match T.instantiate ctx.level scheme with
        | Arrow (fields, variant) -> fields, variant
        | variant -> [], variant
      in
      if List.length fields <> List.length ps
      then fail span Wrong_arguments (tag ^ " has a different number of fields");
      expect span variant t;
      let ps, vars = List.split (List.map2 pattern fields ps) in
      C.Tag (tag, ps), List.concat vars
  in
  let column = subject.shape = Columnar || ctx.columns <> None in
  let arms =
    List.map
      (fun (arm : Syntax.arm) ->
         let p, vars = pattern subject.typ arm.pattern in
         let names = List.map (fun (n, _, _) -> n) vars in
         if List.length names <> List.length (List.sort_uniq compare names)
         then fail span Pattern_bindings "A pattern binds a name twice";
         let env =
           List.fold_left
             (fun env (n, id, t) ->
                bind
                  env
                  n
                  (if column
                   then Inline { subject with typ = t }
                   else Value { id; scheme = T.mono t }))
             ctx.env
             vars
         in
         let inner = { ctx with env } in
         let guard =
           Option.map
             (fun g ->
                let g = expr inner g in
                expect g.span T.Bool g.typ;
                g)
             arm.guard
         in
         let body = expr inner arm.body in
         expect body.span result (T.present body.typ);
         { C.pattern = p; guard; body })
      syntax_arms
  in
  (match uncovered ctx.env ctx.level subject.typ syntax_arms with
   | Some p ->
     fail
       span
       Nonexhaustive_match
       ("Non-exhaustive match. Missing case " ^ Format.pattern p)
   | None -> ());
  let optional = List.exists (fun (a : C.arm) -> T.is_optional a.body.typ) arms in
  let typ = if optional then T.optional result else result in
  let shape = shape_of span (subject :: List.map (fun (a : C.arm) -> a.body) arms) in
  if column
  then decide ctx span subject arms typ shape
  else make span shape typ (Match (subject, arms))

and decide _ctx span subject arms typ shape =
  let known (e : C.expr) =
    make
      e.span
      e.shape
      T.Bool
      (Coalesce (e, make e.span Scalar T.Bool (Constant (Bool false))))
  in
  let both a b =
    match a, b with
    | None, t | t, None -> t
    | Some (a : C.expr), Some (b : C.expr) ->
      Some (make span (shape_of span [ a; b ]) T.Bool (Binary (And, a, b)))
  in
  let compare op (c : C.constant) =
    let literal =
      make
        span
        Scalar
        (match c with
         | Text _ -> T.String
         | Bool _ -> T.Bool
         | _ -> T.present subject.typ)
        (Constant c)
    in
    known (make span subject.shape T.Bool (Binary (op, subject, literal)))
  in
  let rec test = function
    | C.Wildcard | Bind _ | Fields _ -> None
    | Named (_, p) -> test p
    | Missing -> Some (make span subject.shape T.Bool (Builtin (Absent, [ subject ])))
    | Present p ->
      both
        (Some (make span subject.shape T.Bool (Builtin (Present, [ subject ]))))
        (test p)
    | Equals c -> Some (compare Equal c)
    | Between (low, high, inclusive) ->
      both
        (Option.map (compare Greater_equal) low)
        (Option.map (compare (if inclusive then Less_equal else Less)) high)
    | Either ps ->
      let tests = List.map test ps in
      if List.mem None tests
      then None
      else (
        match List.filter_map Fun.id tests with
        | t :: rest ->
          Some
            (List.fold_left
               (fun (a : C.expr) b ->
                  make span (shape_of span [ a; b ]) T.Bool (Binary (Or, a, b)))
               t
               rest)
        | [] -> None)
    | Tag _ | Elements _ ->
      fail
        span
        Unsupported_feature
        "This pattern is not available inside a table verb yet"
  in
  match List.rev arms with
  | [] -> fail span Wrong_arguments "match needs at least one branch"
  | (last : C.arm) :: earlier ->
    List.fold_left
      (fun (otherwise : C.expr) (arm : C.arm) ->
         match both (test arm.pattern) (Option.map known arm.guard) with
         | Some t -> make span shape typ (If (t, arm.body, otherwise))
         | None -> arm.body)
      last.body
      earlier

and retype_table (e : C.expr) =
  match e.node with
  | Table t -> { e with typ = T.Table (T.closed t.schema) }
  | _ -> e

and table ctx (x : Syntax.expr) : C.table =
  let e = expr { ctx with columns = None } x in
  match e.node with
  | Table t -> t
  | Variable id ->
    let rest = T.fresh ctx.level in
    (try T.unify e.typ (T.Table { fields = []; rest = Some rest }) with
     | T.Mismatch -> fail x.span Not_a_table ("Expected a table, found " ^ T.display e.typ));
    let row =
      match T.repr e.typ with
      | Table row -> T.resolve_row row
      | _ -> T.closed []
    in
    { source = Bound id
    ; stages = []
    ; schema = row.fields
    ; rest = row.rest
    ; keys = []
    ; order = None
    ; restricted = []
    }
  | _ -> fail x.span Not_a_table ("Expected a table, found " ^ T.display e.typ)

and source ctx span (f : Syntax.expr) args : C.table =
  let directory = ctx.env.directory in
  let plain path =
    if Filename.is_relative path then Filename.concat directory path else path
  in
  match f.form, args with
  | ( Name (_, (("read_csv" | "read_tsv" | "read_parquet") as reader))
    , [ (None, { form = Literal (Text path); _ }) ] ) ->
    let path = plain path in
    let format = Data_file.format path in
    let expected : Data_file.format =
      match reader with
      | "read_csv" -> Csv
      | "read_tsv" -> Tsv
      | _ -> Parquet
    in
    if format <> expected
    then fail span Unreadable_input (reader ^ " cannot read " ^ Filename.basename path);
    let columns =
      List.map
        (fun (c : Data_file.column) ->
           match c.typ with
           | Supported t -> c.name, if c.nullable then T.Option t else t
           | Unsupported other ->
             fail
               span
               Unsupported_column_type
               (Printf.sprintf "Column %s has unsupported type %s" c.name other))
        (Diagnostic.locate span (fun () -> Data_file.columns path))
    in
    { source = Read { path; digest = Data_file.digest path; format; columns }
    ; stages = []
    ; schema = columns
    ; rest = None
    ; keys = []
    ; order = None
    ; restricted = []
    }
  | Name (_, "load"), [ (None, { form = Field ({ form = Name (Bare, d); _ }, n); _ }) ] ->
    let contract =
      match List.assoc_opt d ctx.env.names with
      | Some (Contract c) -> c
      | _ -> fail span Unknown_name (d ^ " is not a contract")
    in
    let table =
      match List.find_opt (fun (t : Contract.table) -> t.name = n) contract.tables with
      | Some t -> t
      | None -> fail span Unknown_table ("Unknown contracted table " ^ n)
    in
    let path =
      match table.parquet with
      | Some p -> p
      | None ->
        fail span Wrong_arguments ("Contracted table " ^ n ^ " has no Parquet source")
    in
    let physical =
      List.map (fun (c : Data_file.column) -> c.name, c.typ) (Data_file.columns path)
    in
    let columns =
      List.map
        (fun (c : Contract.column) ->
           let t =
             match List.assoc_opt c.name physical with
             | Some (Supported t) -> t
             | Some (Unsupported other) ->
               fail
                 span
                 Unsupported_column_type
                 (Printf.sprintf "Column %s has unsupported type %s" c.name other)
             | None ->
               fail
                 span
                 Unknown_column
                 ("Contract column " ^ c.name ^ " is missing from the file")
           in
           c.name, if c.required then t else T.Option t)
        table.columns
    in
    let flagged f =
      List.filter_map
        (fun (c : Contract.column) -> if f c then Some c.name else None)
        table.columns
    in
    let primary = flagged (fun c -> c.primary) in
    { source =
        Load
          { contract
          ; table
          ; file = { path; digest = Data_file.digest path; format = Parquet; columns }
          }
    ; stages = []
    ; schema = columns
    ; rest = None
    ; keys =
        (if primary = [] then [] else [ primary ])
        @ List.map (fun n -> [ n ]) (flagged (fun c -> c.unique))
    ; order = None
    ; restricted = flagged (fun c -> c.restricted)
    }
  | _ -> fail span Wrong_arguments "A reader takes one file path"

and verb ctx span name input options : C.node =
  let t = table ctx input in
  Table (Verbs.apply (stage_context ctx) span name t options)

and stage_context ctx : Verbs.context =
  { expression =
      (fun ?missing row x ->
        let missing =
          match missing with
          | Some p -> Some p
          | None -> ctx.env.missing
        in
        expr { ctx with columns = Some row; missing } x)
  ; table = (fun x -> table ctx x)
  ; missing = ctx.env.missing
  ; group_missing = ctx.env.group_missing
  }
;;

let rec settle (e : C.expr) : C.expr =
  let typ = T.resolve e.typ in
  let node : C.node =
    match e.node with
    | Constant (Number s) ->
      (match T.repr (T.present typ) with
       | Int64 -> Constant (Int (Int64.of_string s))
       | Float64 -> Constant (Float (exact_float e.span s))
       | _ -> Constant (Number s))
    | (Constant _ | Absent | Variable _ | Column _ | Count) as node -> node
    | Builtin (row, xs) -> Builtin (row, List.map settle xs)
    | Reduce (r, p, x) -> Reduce (r, p, settle x)
    | Unary (op, x) -> Unary (op, settle x)
    | Binary (op, a, b) -> Binary (op, settle a, settle b)
    | Convert x -> Convert (settle x)
    | Lift (bound, body) ->
      Lift (List.map (fun (id, x) -> id, settle x) bound, settle body)
    | If (c, a, b) -> If (settle c, settle a, settle b)
    | Coalesce (a, b) -> Coalesce (settle a, settle b)
    | Require x -> Require (settle x)
    | Match (x, arms) ->
      Match
        ( settle x
        , List.map
            (fun (a : C.arm) ->
               { a with guard = Option.map settle a.guard; body = settle a.body })
            arms )
    | Lambda (ids, body) -> Lambda (ids, settle body)
    | Apply (f, xs) -> Apply (settle f, List.map settle xs)
    | Construct (n, xs) -> Construct (n, List.map settle xs)
    | Let (id, v, body) -> Let (id, settle v, settle body)
    | Tuple xs -> Tuple (List.map settle xs)
    | List xs -> List (List.map settle xs)
    | Record fs -> Record (List.map (fun (n, x) -> n, settle x) fs)
    | Field (x, n) -> Field (settle x, n)
    | Member (x, set) -> Member (settle x, settle set)
    | Foreign (m, f, xs) -> Foreign (m, f, List.map settle xs)
    | Text pieces ->
      Text
        (List.map
           (function
             | C.Fixed s -> C.Fixed s
             | Shown (x, f) -> Shown (settle x, f))
           pieces)
    | Table t -> Table (settle_table t)
  in
  { e with node; typ }

and settle_table (t : C.table) =
  let step : C.step -> C.step = function
    | Filter x -> Filter (settle x)
    | Mutate m -> Mutate { m with value = settle m.value }
    | Summarize s ->
      Summarize { s with measures = List.map (fun (n, x) -> n, settle x) s.measures }
    | Arrange keys -> Arrange (List.map (fun (x, d) -> settle x, d) keys)
    | ( Select _
      | Rename _
      | Take _
      | Distinct _
      | Pivot_longer _
      | Pivot_wider _
      | Assert_key _ ) as s -> s
  in
  let source : C.source =
    match t.source with
    | Join j -> Join { j with left = settle_table j.left; right = settle_table j.right }
    | s -> s
  in
  { t with
    source
  ; stages = List.map (fun (s : C.stage) -> { s with step = step s.step }) t.stages
  ; schema = List.map (fun (n, typ) -> n, T.resolve typ) t.schema
  }
;;

let finish ~default (e : C.expr) =
  if default
  then
    ignore
      (C.exists
         (fun x ->
            T.default_numbers x.typ;
            false)
         e);
  settle e
;;

let function_ env name params body =
  let template = { params; body; closure = env } in
  let ctx = { env; level = 1; columns = None; missing = env.missing } in
  let host () =
    let lambda = expr ctx { body with form = Lambda (params, body) } in
    let id = fresh_id name in
    ( bind
        env
        name
        (Function { id = Some id; scheme = Some (T.generalize 0 lambda.typ); template })
    , [ C.Define (id, finish ~default:false lambda) ] )
  in
  let contextual () =
    let types = List.map (fun _ -> T.fresh 1) params in
    let inner =
      List.fold_left2
        (fun inner p t ->
           bind inner p (Inline (make body.span Columnar t (Variable (fresh_id p)))))
        env
        params
        types
    in
    let result = expr { ctx with env = inner } body in
    let scheme = T.generalize 0 (T.Arrow (types, result.typ)) in
    bind env name (Function { id = None; scheme = Some scheme; template }), []
  in
  try host () with
  | Diagnostic.Error { code = Column_required | Missing_policy_required; _ } ->
    contextual ()
;;

let printed (x : Syntax.expr) =
  match x.form with
  | Call ({ form = Name (Bare, "print"); _ }, [ (None, t) ])
  | Pipe (t, { form = Call ({ form = Name (Bare, "print"); _ }, []); _ }) -> Some t
  | _ -> None
;;

let written (x : Syntax.expr) =
  match x.form with
  | Call
      ( { form = Name (Bare, "write_parquet"); _ }
      , [ (None, t); (None, { form = Literal (Text path); _ }) ] )
  | Pipe
      ( t
      , { form =
            Call
              ( { form = Name (Bare, "write_parquet"); _ }
              , [ (None, { form = Literal (Text path); _ }) ] )
        ; _
        } ) -> Some (t, path)
  | _ -> None
;;

let public span (t : C.table) =
  if t.restricted = []
  then t
  else (
    let visible = List.filter (fun n -> not (List.mem n t.restricted)) (C.names t) in
    if visible = []
    then
      fail
        span
        Restricted_output
        "Every column is restricted, so there is nothing to show";
    { t with
      stages = t.stages @ [ { step = Select visible; at = span } ]
    ; schema = List.filter (fun (n, _) -> List.mem n visible) t.schema
    })
;;

let table_expr span (t : C.table) =
  let t = public span t in
  make span C.Scalar (T.Table (T.closed t.schema)) (C.Table t)
;;

let item env (d : Syntax.declaration) =
  let ctx = { env; level = 1; columns = None; missing = env.missing } in
  match d with
  | Let (name, Some params, body) | Let (name, None, { form = Lambda (params, body); _ })
    -> function_ env name params body
  | Let (name, None, x) ->
    let e = finish ~default:true (expr ctx x) in
    let id = fresh_id name in
    (match e.node with
     | Table t -> bind env name (Table_value { id; table = t }), [ C.Define (id, e) ]
     | _ ->
       bind env name (Value { id; scheme = T.generalize 0 e.typ }), [ C.Define (id, e) ])
  | Evaluate x ->
    (match written x, printed x with
     | Some (t, path), _ ->
       env, [ C.Write (finish ~default:true (table_expr x.span (table ctx t)), path) ]
     | None, Some t ->
       env, [ C.Show (finish ~default:true (table_expr x.span (table ctx t))) ]
     | None, None ->
       let e = expr ctx x in
       (match e.node with
        | Table t -> env, [ C.Show (finish ~default:true (table_expr x.span t)) ]
        | _ -> env, [ C.Show (finish ~default:true e) ]))
  | Alias (name, params, body) ->
    if List.mem_assoc name env.types
    then Diagnostic.fail Duplicate_name ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, T.fresh 1) params in
    let t = annotation env ps 1 body in
    ( { env with
        types = (name, T.generalize 0 (T.Arrow (List.map snd ps, t))) :: env.types
      }
    , [] )
  | Type (name, params, constructors) ->
    if List.mem_assoc name env.types
    then Diagnostic.fail Duplicate_name ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, T.fresh 1) params in
    let t = T.Variant (name, List.map snd ps) in
    let env =
      { env with
        types =
          (name, T.generalize 0 (if ps = [] then t else T.Arrow (List.map snd ps, t)))
          :: env.types
      }
    in
    let tags =
      List.fold_left
        (fun tags (tag, fields) ->
           if
             List.exists (fun (_, cs) -> List.mem_assoc tag cs) env.variants
             || List.mem_assoc tag tags
           then Diagnostic.fail Duplicate_name ("Duplicate constructor " ^ tag);
           let args = List.map (fun (_, a) -> annotation env ps 1 a) fields in
           (tag, T.generalize 0 (if args = [] then t else T.Arrow (args, t))) :: tags)
        []
        constructors
      |> List.rev
    in
    let env =
      List.fold_left
        (fun env (tag, (scheme : T.scheme)) ->
           let arity =
             match scheme.body with
             | Arrow (args, _) -> List.length args
             | _ -> 0
           in
           bind env tag (Constructor { scheme; arity }))
        { env with variants = (name, tags) :: env.variants }
        tags
    in
    ( env
    , [ C.Variant
          (name, List.map (fun (tag, fields) -> tag, List.length fields) constructors)
      ] )
  | Import ("polars", alias) ->
    if env.strict
    then
      Diagnostic.fail
        Dialect_capability
        "import polars is target-specific. Set strict = false under [check] in \
         rupam.toml, or pass --no-strict";
    bind env alias (Module "polars"), []
  | Import (n, _) -> Diagnostic.fail Unknown_module ("Unknown module " ^ n)
  | Contract (path, n) ->
    let path =
      if Filename.is_relative path then Filename.concat env.directory path else path
    in
    bind env n (Contract (Contract.import path)), []
  | Setting (name, value) ->
    let policy =
      match List.assoc_opt value C.policies with
      | Some p -> p
      | None ->
        Diagnostic.fail Invalid_policy "A missing policy is skip, propagate or error"
    in
    (match name with
     | "missing" -> { env with missing = Some policy }, []
     | "group_missing" -> { env with group_missing = Some policy }, []
     | _ ->
       Diagnostic.fail
         Unknown_setting
         ("Unknown setting " ^ name ^ ". Expected missing or group_missing"))
;;

let program env declarations =
  List.fold_left
    (fun (env, items) d ->
       let env, more = item env d in
       env, items @ more)
    (env, [])
    declarations
;;

let describe = function
  | Value { scheme; _ } -> Some (T.display scheme.body)
  | Table_value { table; _ } -> Some (T.display (T.Table (T.closed table.schema)))
  | Function { scheme = Some scheme; _ } -> Some (T.display scheme.body)
  | Function { template; _ } -> Some ("fn(" ^ String.concat ", " template.params ^ ")")
  | Constructor { scheme; _ } -> Some (T.display scheme.body)
  | Contract _ -> Some "contract"
  | Module m -> Some ("module " ^ m)
  | Inline _ -> None
;;

let bindings env =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (n, b) ->
       if Hashtbl.mem seen n
       then None
       else (
         Hashtbl.add seen n ();
         Option.map (fun d -> n, b, d) (describe b)))
    env.names
  |> List.rev
;;

let lookup env name = List.assoc_opt name env.names
let builtins () = List.map (fun (n, b) -> n, T.display (builtin_type 1 b)) Prelude.all

let explain (t : C.table) =
  let rec source (t : C.table) =
    match t.source with
    | Read f -> [ Printf.sprintf "read %s" (Filename.basename f.path) ]
    | Load { table; contract; _ } ->
      [ Printf.sprintf "load %s from %s" table.name (Filename.basename contract.path) ]
    | Bound id -> [ "start from " ^ id.name ]
    | Join j ->
      source j.left
      @ source j.right
      @ [ Printf.sprintf "%s join on %s" (C.join_name j.kind) j.key ]
  in
  let step (s : C.stage) =
    match s.step with
    | Filter _ -> "filter rows"
    | Mutate { name; by = []; _ } -> "mutate " ^ name
    | Mutate { name; by; _ } ->
      Printf.sprintf "mutate %s by %s" name (String.concat ", " by)
    | Select names -> "select " ^ String.concat ", " names
    | Rename changes ->
      "rename "
      ^ String.concat ", " (List.map (fun (fresh, old) -> old ^ " to " ^ fresh) changes)
    | Summarize { by; measures } ->
      "summarize "
      ^ String.concat ", " (List.map fst measures)
      ^ if by = [] then "" else " by " ^ String.concat ", " by
    | Arrange _ -> "arrange"
    | Take n -> Printf.sprintf "take the first %d rows" n
    | Distinct _ -> "distinct rows"
    | Assert_key columns -> "check that " ^ String.concat ", " columns ^ " is a key"
    | Pivot_longer _ -> "pivot longer"
    | Pivot_wider _ -> "pivot wider"
  in
  source t @ List.map step t.stages
;;

let rec origin env (t : C.table) =
  match t.source with
  | Read f -> Filename.basename f.path ^ ", schema read from the file"
  | Load { table; contract; _ } ->
    Printf.sprintf "%s in %s" table.name (Filename.basename contract.path)
  | Bound id ->
    (match
       List.find_map
         (function
           | _, Table_value { id = bound; table } when bound.stamp = id.stamp ->
             Some table
           | _ -> None)
         env.names
     with
     | Some table -> origin env table
     | None -> id.name)
  | Join j ->
    Printf.sprintf
      "%s join of %s and %s on %s"
      (C.join_name j.kind)
      (origin env j.left)
      (origin env j.right)
      j.key
;;
