open Syntax
open Shape

type binding =
  | Module of string
  | Value of Shape.expr
  | Function of string list * Syntax.expr * env

and env = (string * binding) list

let lookup name bindings =
  match List.assoc_opt name bindings with
  | Some value -> value
  | None ->
    let hint =
      List.map (fun (n, _) -> Distance.between n name, n) bindings |> List.sort compare
    in
    let message =
      match hint with
      | (d, n) :: _ when d <= 2 -> ". Did you mean " ^ n ^ "?"
      | _ -> ""
    in
    raise (Error ("Unknown name: " ^ name ^ message))
;;

let typ (t : Shape.typ) =
  let base : Infer.typ =
    match t.primitive with
    | Int64 -> Int
    | Float64 -> Float
    | Bool -> Bool
    | String -> String
  in
  if t.nullable then Infer.Option base else base
;;

let rec types env =
  List.fold_right
    (fun (name, binding) (inferred : Infer.env) ->
       match binding with
       | Module _ ->
         Infer.extend
           inferred
           [ name, ([], Record ([ "sqrt", Arrow ([ Float ], Float) ], None)) ]
       | Value x -> Infer.extend inferred [ name, ([], typ x.typ) ]
       | Function (params, body, closure) ->
         let local = Infer.rows (types closure) [] in
         (match Infer.expression local 2 { body with form = Lambda (params, body) } with
          | t -> Infer.extend inferred [ name, Infer.generalize 1 t ]
          | exception Infer.Error _ -> inferred))
    env
    Infer.empty
;;

let unsupported what = raise (Error (what ^ " is not available inside a table verb yet"))

let policy (x : Syntax.expr) =
  match x.form with
  | Name (_, "skip") -> Skip
  | Name (_, "propagate") -> Propagate
  | Name (_, "error") -> Fail
  | _ -> raise (Error "missing must be skip, propagate or error")
;;

let rec elaborate ?missing env columns source =
  let elaborate = elaborate ?missing in
  let go = elaborate env columns in
  match source.form with
  | Literal (Bool b) -> constant (`Bool b)
  | Literal (Text s) -> constant (`String s)
  | Literal Absent -> null
  | Literal (Float s) -> constant (`Float (float_of_string s))
  | Literal (Int s) when Infer.promoted source -> constant (`Float (exact_float s))
  | Literal (Int s) ->
    (match Int64.of_string_opt s with
     | Some n -> constant (`Int n)
     | None -> raise (Error "Integer literal is outside the Int64 range"))
  | Name (Column, name) -> lookup name columns
  | Name (Bare, name) when List.mem_assoc name columns -> lookup name columns
  | Name (_, name) ->
    (match lookup name env with
     | Value x -> x
     | Function _ | Module _ -> raise (Error (name ^ " is a function, not a value")))
  | Annotated (x, _) -> go x
  | If (c, t, f) -> choose (go c) (go t) (go f)
  | Binary (op, a, b) -> binary op (go a) (go b)
  | Unary (op, a) -> unary op (go a)
  | Local (name, { form = Lambda (params, f); _ }, body) ->
    elaborate ((name, Function (params, f, env)) :: env) columns body
  | Local (name, x, body) -> elaborate ((name, Value (go x)) :: env) columns body
  | Pipe (x, { form = Call (f, args); _ }) ->
    go { source with form = Call (f, (None, x) :: args) }
  | Pipe (x, f) -> go { source with form = Call (f, [ None, x ]) }
  | Match (subject, branches) -> choice ?missing env columns (go subject) branches
  | Call ({ form = Name (Bare, "Some"); _ }, [ (None, x) ]) ->
    let x = go x in
    { x with typ = { x.typ with nullable = true } }
  | Call ({ form = Field ({ form = Name (Bare, ns); _ }, "sqrt"); _ }, [ (None, arg) ]) ->
    (match List.assoc_opt ns env with
     | Some (Module "polars") -> ()
     | _ ->
       raise (Error "polars.sqrt requires explicit import polars and the Python target"));
    let v = go arg in
    { v with node = Polars_sqrt v }
  | Call ({ form = Name (Bare, name); _ }, args) ->
    let missing =
      match List.assoc_opt (Some "missing") args with
      | Some p -> Some (policy p)
      | None -> missing
    in
    let args = List.filter (fun (label, _) -> label <> Some "missing") args in
    let values =
      List.map
        (function
          | None, x -> go x
          | Some label, _ -> raise (Error (name ^ " takes no argument named " ^ label)))
        args
    in
    (match List.assoc_opt name env, Prelude.find name, values with
     | Some (Function (params, body, closure)), _, _ ->
       let locals = List.map2 (fun p v -> p, Value v) params values in
       elaborate (locals @ closure) [] body
     | Some _, _, _ -> raise (Error (name ^ " is not a function"))
     | None, Some Coalesce, [ x; fallback ] -> coalesce x fallback
     | None, Some (Row row), _ -> apply row values
     | None, Some (Reduction Count), [] -> count
     | None, Some (Reduction r), [ x ] -> reduce ?policy:missing r x
     | _ -> raise (Error ("Wrong arguments to " ^ name)))
  | Call ({ form = Lambda (params, body); _ }, args)
    when List.length params = List.length args ->
    let locals = List.map2 (fun p (_, a) -> p, Value (go a)) params args in
    elaborate (locals @ env) columns body
  | Call _ -> unsupported "Calling a computed function"
  | Lambda _ -> unsupported "An anonymous function"
  | List _ -> unsupported "A list"
  | Tuple _ -> unsupported "A tuple"
  | Record _ -> unsupported "A record"
  | Field _ -> unsupported "Field access"
  | Range _ -> unsupported "A range"
  | Formula _ -> unsupported "A formula"
  | Member (x, { form = List items; _ }) -> member (go x) (List.map go items)
  | Member _ -> unsupported "%in% with a computed set"

and choice ?missing env columns subject arms =
  let elaborate = elaborate ?missing in
  let known e = coalesce e (constant (`Bool false)) in
  let both a b =
    match a, b with
    | None, t | t, None -> t
    | Some a, Some b -> Some (binary And a b)
  in
  let literal = function
    | Int s -> constant (`Int (Int64.of_string s))
    | Float s -> constant (`Float (float_of_string s))
    | Text s -> constant (`String s)
    | Bool b -> constant (`Bool b)
    | Absent -> null
  in
  let rec test subject pattern =
    let present = { subject with typ = { subject.typ with nullable = false } } in
    match pattern with
    | Wildcard -> None, []
    | Bind n -> None, [ n, Value subject ]
    | Pas (n, p) ->
      let t, vars = test subject p in
      t, (n, Value subject) :: vars
    | Tag ("Some", [ p ]) ->
      let t, vars = test present p in
      both (Some (apply Present [ subject ])) t, vars
    | Tag ("None", []) | Pliteral Absent -> Some (apply Absent [ subject ]), []
    | Pliteral l -> Some (known (binary Equal subject (literal l))), []
    | Prange { low; high; inclusive } ->
      let bound op l = known (binary op subject (literal l)) in
      ( both
          (Option.map (bound Greater_equal) low)
          (Option.map (bound (if inclusive then Less_equal else Less)) high)
      , [] )
    | Palt alternatives ->
      let tests = List.map (test subject) alternatives in
      let vars = snd (List.hd tests) in
      (match List.map fst tests with
       | tests when List.exists Option.is_none tests -> None, vars
       | t :: rest ->
         ( Some
             (List.fold_left
                (fun a b -> binary Or a b)
                (Option.get t)
                (List.map Option.get rest))
         , vars )
       | [] -> None, vars)
    | Tag _ | Ptuple _ | Precord _ -> unsupported "This pattern"
  in
  let arm a =
    let t, vars = test subject a.pattern in
    let env = vars @ env in
    let guard = Option.map (fun g -> known (elaborate env columns g)) a.guard in
    both t guard, elaborate env columns a.body
  in
  match List.rev_map arm arms with
  | [] -> raise (Error "match needs at least one branch")
  | (_, last) :: earlier ->
    List.fold_left
      (fun otherwise (test, body) ->
         match test with
         | Some test -> choose test body otherwise
         | None -> body)
      last
      earlier
;;

let expression ?missing env columns source =
  let fields = List.map (fun (n, (x : Shape.expr)) -> n, typ x.typ) columns in
  ignore (Infer.expression (Infer.rows (types env) fields) 1 source);
  elaborate ?missing env columns source
;;

let define env = function
  | Let (name, Some params, body) | Let (name, None, { form = Lambda (params, body); _ })
    -> (name, Function (params, body, env)) :: env
  | Let (name, None, body) -> (name, Value (expression env [] body)) :: env
  | _ -> raise (Error "Expected a let declaration")
;;

let mutate env columns bindings =
  List.fold_left
    (fun schema (label, source) ->
       match label with
       | None -> raise (Error "mutate requires named columns")
       | Some name when List.mem_assoc name schema ->
         List.map
           (fun ((n, _) as field) ->
              if n = name then n, expression env schema source else field)
           schema
       | Some name -> schema @ [ name, expression env schema source ])
    columns
    bindings
;;
