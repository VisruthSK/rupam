open Syntax
module T = Types
module C = TransformIR
open Scope
open Terms
open Exhaustive

type ctx =
  { env : env
  ; level : int
  ; columns : T.row option
  ; missing : C.policy option
  }

let policy_of (x : Syntax.expr) =
  match x.form with
  | Name (_, n) when List.mem_assoc n C.policies -> List.assoc n C.policies
  | _ -> fail x.span Invalid_policy "A missing policy is skip, keep or error"
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
       | Float _ -> T.Float
       | Text _ -> T.String
       | Bool _ -> T.Bool
       | Int _ | Number _ -> T.Int)
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
    lift ctx.env span v.shape [ v ] (function
      | [ v ] -> make v.shape (T.present v.typ) (Unary (op, v))
      | _ -> assert false)
  | Binary (op, a, b) -> binary ctx span op (here a) (here b)
  | Chain (first, links) ->
    chain ctx span (here first) (List.map (fun (op, x) -> op, here x) links)
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
    lift ctx.env span v.shape [ v ] (function
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
       let id = fresh_id ctx.env n in
       let body =
         expr
           { ctx with
             env = bind ctx.env n (Value { id; scheme = T.generalize ctx.level v.typ })
           }
           body
       in
       make body.shape body.typ (Let (id, v, body)))
  | Lambda (params, body) ->
    let ids = List.map (fresh_id ctx.env) params in
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
  | Match_present (subject, arms) ->
    let subject = here subject in
    match_ ctx span subject (present_arms span subject arms)
  | Fallback (a, b) -> fallback span (here a) (here b)
  | Sequence r ->
    let a = lower ctx (here r.first) r.first_closed in
    let b = upper ctx (here r.last) r.last_closed in
    make Scalar (T.List T.Int) (Sequence (a, b))
  | Index (subject, selection) ->
    let subject = here subject in
    let element = T.fresh ctx.level in
    expect span (T.List element) subject.typ;
    let bound (x : Syntax.expr) inclusive =
      match x.form with
      | Last -> None
      | _ -> Some (upper ctx (here x) inclusive)
    in
    (match selection with
     | Element { form = Sequence r; _ } ->
       let low =
         match r.first.form with
         | Last -> None
         | _ -> Some (lower ctx (here r.first) r.first_closed)
       in
       make Scalar subject.typ (Slice (subject, low, bound r.last r.last_closed))
     | Element { form = Last; _ } -> make Scalar element (Element (subject, None))
     | Element i -> make Scalar element (Element (subject, Some (position ctx (here i))))
     | From (i, closed) ->
       make Scalar subject.typ (Slice (subject, Some (lower ctx (here i) closed), None))
     | Upto (i, inclusive) ->
       make Scalar subject.typ (Slice (subject, None, bound i inclusive)))
  | Last -> fail span Wrong_arguments "end refers to the last position inside x[...]"
  | Tap _ ->
    fail
      span
      Unsupported_feature
      "Inspections like !view() belong to interactive pipelines at the prompt, not to \
       functions or scripts"
  | Range _ -> fail span R_range "`a:b` selects columns. For integers write `a=..=b`"
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

and chain ctx span first links =
  let shared (x : C.expr) =
    match x.node with
    | Constant _ | Variable _ | Column _ -> None, x
    | _ ->
      let id = fresh_id ctx.env "middle" in
      Some (id, x), make x.span x.shape x.typ (Variable id)
  in
  let last = List.length links - 1 in
  let bound, links =
    List.mapi (fun i (op, x) -> if i = last then (None, x), op else shared x, op) links
    |> List.map (fun ((binding, x), op) -> binding, (op, x))
    |> List.split
  in
  let _, comparisons =
    List.fold_left
      (fun (left, acc) (op, right) -> right, binary ctx span op left right :: acc)
      (first, [])
      links
  in
  let conjunction =
    match List.rev comparisons with
    | [] -> assert false
    | c :: rest -> List.fold_left (fun a b -> binary ctx span And a b) c rest
  in
  List.fold_right
    (fun binding body ->
       match binding with
       | None -> body
       | Some (id, value) -> make span body.C.shape body.typ (Let (id, value, body)))
    bound
    conjunction

and binary ctx span op a b =
  let make = make span in
  let number t = expect span (T.fresh ~kind:Number ctx.level) t in
  let a, b, result =
    match op with
    | Div | Power ->
      number (T.present a.typ);
      number (T.present b.typ);
      to_float a, to_float b, T.Float
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
  lift ctx.env span shape [ a; b ] (function
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
  | Name (Bare, n) when List.mem_assoc n TransformIR.join_kinds || Verbs.mem n ->
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
               let id = fresh_id ctx.env "v" in
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
  | (Float | Option Float), Int -> to_float v
  | Option inner, _ when not (T.is_optional v.typ) ->
    expect v.span inner element;
    v
  | Option _, _ ->
    expect v.span param v.typ;
    v
  | _ ->
    expect v.span param element;
    v

and position ctx (x : C.expr) =
  expect x.span T.Int x.typ;
  ignore ctx;
  x

and lower ctx (x : C.expr) closed =
  let x = position ctx x in
  if closed
  then x
  else
    make
      x.span
      Scalar
      T.Int
      (Binary (Add, x, make x.span Scalar T.Int (Constant (Int 1L))))

and upper ctx (x : C.expr) inclusive =
  let x = position ctx x in
  if inclusive
  then x
  else
    make
      x.span
      Scalar
      T.Int
      (Binary (Sub, x, make x.span Scalar T.Int (Constant (Int 1L))))

and fallback span v alternative =
  let v, alternative, t = common span v alternative in
  make
    span
    (shape_of span [ v; alternative ])
    (if T.is_optional alternative.typ then T.optional t else t)
    (Coalesce (v, alternative))

and present_arms span (subject : C.expr) arms =
  if not (T.is_optional subject.typ)
  then arms
  else (
    let absent = { Syntax.form = Literal Absent; span } in
    { Syntax.pattern = Pliteral Absent; guard = None; body = absent }
    :: List.map
         (fun (a : Syntax.arm) -> { a with pattern = Tag ("Some", [ a.pattern ]) })
         arms)

and builtin ctx span n b args =
  let make = make span in
  let missing, values = arguments ctx span n args in
  let values = List.map (expr ctx) values in
  match b, values with
  | Prelude.Coalesce, (_ :: _ :: _ as values) ->
    let rec chain = function
      | [ last ] -> last
      | v :: rest -> fallback span v (chain rest)
      | [] -> assert false
    in
    chain values
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
       lift ctx.env span shape values (fun values ->
         make shape (T.resolve result) (Builtin (row, values))))
  | Reduction Count, [] ->
    if ctx.columns = None then fail span Column_required "n() counts the rows of a table";
    make Scalar T.Int Count
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
              missing = skip ignores them, missing = keep makes the result missing, \
              missing = error stops the run")
    in
    let element = T.present v.typ in
    let v, result =
      match r with
      | Sum | Min | Max ->
        expect span (T.fresh ~kind:Number ctx.level) element;
        v, T.Option element
      | Mean | Median | Var | Sd -> to_float v, T.Option T.Float
      | Distinct -> v, T.Int
      | First | Last -> v, T.Option element
      | Count -> v, T.Int
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
           | (Int | Float), _ | Var _, None | (Bool | String), None -> ()
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
  lift ctx.env span shape shown (fun values ->
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
      let id = fresh_id ctx.env n in
      C.Bind id, [ n, id, t ]
    | Pas (n, p) ->
      let id = fresh_id ctx.env n in
      let p, vars = pattern t p in
      C.Named (id, p), (n, id, t) :: vars
    | Pliteral Absent ->
      expect span (T.Option (T.fresh ctx.level)) t;
      C.Missing, []
    | Pliteral l ->
      let c = constant span l in
      (match c with
       | Number _ -> expect span (T.fresh ~kind:Number ctx.level) (T.present t)
       | Float _ -> expect span T.Float (T.present t)
       | Text _ -> expect span T.String (T.present t)
       | Bool _ -> expect span T.Bool (T.present t)
       | Int _ -> ());
      C.Equals c, []
    | Prange { low; high } ->
      expect span (T.fresh ~kind:Number ctx.level) (T.present t);
      List.iter
        (fun (b : Syntax.bound) ->
           match b.value with
           | Syntax.Float _ -> expect span T.Float (T.present t)
           | _ -> ())
        (Option.to_list low @ Option.to_list high);
      let bound =
        Option.map (fun (b : Syntax.bound) -> constant span b.value, b.closed)
      in
      C.Between (bound low, bound high), []
    | Precord names ->
      let fields = List.map (fun n -> n, T.fresh ctx.level) names in
      expect span (T.Record { fields; rest = Some (T.fresh ctx.level) }) t;
      let ids = List.map (fun (n, ft) -> n, fresh_id ctx.env n, ft) fields in
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
    | Between (low, high) ->
      both
        (Option.map
           (fun (c, closed) -> compare (if closed then Greater_equal else Greater) c)
           low)
        (Option.map
           (fun (c, closed) -> compare (if closed then Less_equal else Less) c)
           high)
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
       | Int -> Constant (Int (Int64.of_string s))
       | Float -> Constant (Float (exact_float e.span s))
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
    | Sequence (a, b) -> Sequence (settle a, settle b)
    | Element (x, i) -> Element (settle x, Option.map settle i)
    | Slice (x, a, b) -> Slice (settle x, Option.map settle a, Option.map settle b)
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
    let id = fresh_id env name in
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
           bind inner p (Inline (make body.span Columnar t (Variable (fresh_id env p)))))
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

let declaration env (d : Syntax.declaration) =
  let ctx = { env; level = 1; columns = None; missing = env.missing } in
  match d with
  | Let (name, Some params, body) | Let (name, None, { form = Lambda (params, body); _ })
    -> function_ env name params body
  | Let (name, None, x) ->
    let e = finish ~default:true (expr ctx x) in
    let id = fresh_id env name in
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
      | None -> Diagnostic.fail Invalid_policy "A missing policy is skip, keep or error"
    in
    (match name with
     | "missing" -> { env with missing = Some policy }, []
     | "group_missing" -> { env with group_missing = Some policy }, []
     | _ ->
       Diagnostic.fail
         Unknown_setting
         ("Unknown setting " ^ name ^ ". Expected missing or group_missing"))
;;

let item env (i : Syntax.item) =
  Diagnostic.locate i.span (fun () -> declaration env i.declaration)
;;

let program env declarations =
  List.fold_left
    (fun (env, items) d ->
       let env, more = item env d in
       env, items @ more)
    (env, [])
    declarations
;;

let builtins () = List.map (fun (n, b) -> n, T.display (builtin_type 1 b)) Prelude.all
