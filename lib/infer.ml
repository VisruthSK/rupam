open Syntax

type constraint_ =
  | Any
  | Numeric

type typ =
  | Variant of string * typ list
  | List of typ
  | Option of typ
  | Tuple of typ list
  | Record of (string * typ) list * typ option
  | Int
  | Float
  | Bool
  | String
  | Arrow of typ list * typ
  | Var of cell ref

and cell =
  | Free of int * constraint_
  | Link of typ

type scheme = cell ref list * typ

exception Error of span * string

let fresh level c = Var (ref (Free (level, c)))

let rec repr = function
  | Var { contents = Link t } -> repr t
  | t -> t
;;

let rec variables f t =
  match repr t with
  | Var cell -> f cell
  | List t | Option t -> variables f t
  | Variant (_, ts) | Tuple ts -> List.iter (variables f) ts
  | Record (fs, tail) ->
    List.iter (fun (_, t) -> variables f t) fs;
    Option.iter (variables f) tail
  | Arrow (args, result) -> List.iter (variables f) (result :: args)
  | _ -> ()
;;

let unify span =
  let fail () = raise (Error (span, "Incompatible types")) in
  let rec equal x y =
    match repr x, repr y with
    | Var a, Var b when a == b -> ()
    | Var ({ contents = Free (level, c) } as a), t
    | t, Var ({ contents = Free (level, c) } as a) ->
      (match c, t with
       | ( Numeric
         , (Bool | String | Arrow _ | List _ | Option _ | Tuple _ | Record _ | Variant _)
         ) -> fail ()
       | _ -> ());
      variables
        (fun b ->
           match !b with
           | Free (n, k) ->
             if a == b then raise (Error (span, "Infinite type"));
             let k =
               match t with
               | Var _ when c = Numeric -> Numeric
               | _ -> k
             in
             b := Free (min n level, k)
           | Link _ -> assert false)
        t;
      a := Link t
    | List a, List b | Option a, Option b -> equal a b
    | Variant (a, xs), Variant (b, ys) when a = b && List.length xs = List.length ys ->
      List.iter2 equal xs ys
    | Tuple xs, Tuple ys when List.length xs = List.length ys -> List.iter2 equal xs ys
    | Record (xs, tx), Record (ys, ty) ->
      let only xs ys = List.filter (fun (n, _) -> not (List.mem_assoc n ys)) xs in
      List.iter (fun (n, t) -> Option.iter (equal t) (List.assoc_opt n ys)) xs;
      let left = only xs ys
      and right = only ys xs in
      (match tx, ty with
       | None, None -> if left <> [] || right <> [] then fail ()
       | Some a, None ->
         if left <> [] then fail ();
         equal a (Record (right, None))
       | None, Some b ->
         if right <> [] then fail ();
         equal b (Record (left, None))
       | Some a, Some b when left = [] && right = [] -> equal a b
       | Some a, Some b ->
         if repr a == repr b then fail ();
         let rest = fresh max_int Any in
         equal a (Record (right, Some rest));
         equal b (Record (left, Some rest)))
    | Arrow (xs, x), Arrow (ys, y) when List.length xs = List.length ys ->
      List.iter2 equal xs ys;
      equal x y
    | Int, Int | Float, Float | Bool, Bool | String, String -> ()
    | _ -> fail ()
  in
  equal
;;

let generalize level t =
  let vars = ref [] in
  variables
    (fun v ->
       match !v with
       | Free (n, _) when n > level && not (List.exists (( == ) v) !vars) ->
         vars := v :: !vars
       | _ -> ())
    t;
  !vars, t
;;

let instantiate level (vars, t) =
  let replacements =
    List.map
      (fun v ->
         match !v with
         | Free (_, c) -> v, fresh level c
         | _ -> assert false)
      vars
  in
  let rec copy t =
    match repr t with
    | Var v ->
      (match List.find_opt (fun (k, _) -> v == k) replacements with
       | Some (_, new_t) -> new_t
       | None -> t)
    | List a -> List (copy a)
    | Option a -> Option (copy a)
    | Variant (n, ts) -> Variant (n, List.map copy ts)
    | Tuple ts -> Tuple (List.map copy ts)
    | Record (fs, tail) ->
      Record (List.map (fun (n, t) -> n, copy t) fs, Option.map copy tail)
    | Arrow (ps, r) -> Arrow (List.map copy ps, copy r)
    | t -> t
  in
  copy t
;;

let rec annotation env params level = function
  | Syntax.Arow (fs, tail) ->
    Record
      ( List.map (fun (n, t) -> n, annotation env params level t) fs
      , Option.map
          (fun n ->
             match List.assoc_opt n params with
             | Some t -> t
             | None -> fresh level Any)
          tail )
  | Atuple ts -> Tuple (List.map (annotation env params level) ts)
  | Aarrow (a, b) ->
    Arrow ([ annotation env params level a ], annotation env params level b)
  | Syntax.Optional t -> Option (annotation env params level t)
  | Named (n, args) ->
    let args = List.map (annotation env params level) args in
    (match n, args with
     | "Int64", [] -> Int
     | "Float64", [] -> Float
     | "String", [] -> String
     | "Bool", [] -> Bool
     | "List", [ t ] -> List t
     | "Option", [ t ] -> Option t
     | n, [] when List.mem_assoc n params -> List.assoc n params
     | n, ts ->
       (match List.assoc_opt ("type " ^ n) env with
        | Some (_, Variant (_, ps)) when List.length ps = List.length ts -> Variant (n, ts)
        | Some scheme ->
          (match instantiate level scheme with
           | Arrow (params, result) when List.length params = List.length ts ->
             List.iter2 (unify (Lexing.dummy_pos, Lexing.dummy_pos)) params ts;
             result
           | _ -> failwith ("Wrong type arity: " ^ n))
        | _ -> failwith ("Unknown type or wrong arity: " ^ n)))
;;

let rec expression env level source =
  let check = expression env level
  and equal = unify source.span in
  let fail m = raise (Error (source.span, m)) in
  let call f args =
    let r = fresh level Any in
    equal f (Arrow (args, r));
    r
  in
  match source.form with
  | Annotated (x, a) ->
    let t = annotation env [] level a in
    equal t (check x);
    t
  | Match (subject, branches) ->
    let subject = check subject
    and result = fresh level Any in
    let rec bind t = function
      | Precord ns ->
        let fs = List.map (fun n -> n, fresh level Any) ns in
        equal t (Record (fs, Some (fresh level Any)));
        List.map (fun (n, t) -> n, ([], t)) fs
      | Wildcard -> []
      | Bind n -> [ n, ([], t) ]
      | Pliteral s ->
        equal t (check { source with form = Literal s });
        []
      | Ptuple ps ->
        let ts = List.map (fun _ -> fresh level Any) ps in
        equal t (Tuple ts);
        List.concat (List.map2 bind ts ps)
      | Tag (n, ps) ->
        let ts = List.map (fun _ -> fresh level Any) ps in
        (match n, ts with
         | "Some", [ v ] -> equal t (Option v)
         | _ ->
           let constr =
             match List.assoc_opt n env with
             | Some s -> instantiate level s
             | None -> fail ("Unknown constructor " ^ n)
           in
           equal constr (if ts = [] then t else Arrow (ts, t)));
        List.concat (List.map2 bind ts ps)
    in
    List.iter
      (fun (p, body) ->
         let vars = bind subject p in
         let ns = List.map fst vars in
         if List.length ns <> List.length (List.sort_uniq compare ns)
         then fail "Repeated pattern variable";
         equal result (expression (vars @ env) level body))
      branches;
    let rec irrefutable = function
      | Precord _ | Bind _ | Wildcard -> true
      | Ptuple ps -> List.for_all irrefutable ps
      | _ -> false
    in
    let covers n =
      List.exists
        (function
          | Tag (tag, ps), _ -> tag = n && List.for_all irrefutable ps
          | Pliteral s, _ -> s = n
          | _ -> false)
        branches
    in
    if not (List.exists (fun (p, _) -> irrefutable p) branches)
    then (
      let tags =
        match repr subject with
        | Bool -> [ "true"; "false" ]
        | Option _ -> [ "Some"; "None" ]
        | Variant (n, _) ->
          List.filter_map
            (fun (tag, (_, t)) ->
               let t =
                 match t with
                 | Arrow (_, r) -> r
                 | r -> r
               in
               match t with
               | Variant (m, _)
                 when n = m
                      && (not (String.starts_with ~prefix:"type " tag))
                      && Char.uppercase_ascii tag.[0] = tag.[0] -> Some tag
               | _ -> None)
            env
        | _ -> []
      in
      if tags = [] || not (List.for_all covers tags) then fail "Non-exhaustive match");
    result
  | Name (Bare, n) ->
    (match List.assoc_opt n env with
     | Some s -> instantiate level s
     | None -> fail ("Unknown name: " ^ n))
  | Literal ("true" | "false") -> Bool
  | Literal "None" -> Option (fresh level Any)
  | Call ({ form = Name (Bare, "Some"); _ }, [ (None, x) ]) -> Option (check x)
  | List xs ->
    let t = fresh level Any in
    List.iter (fun x -> equal t (check x)) xs;
    List t
  | Tuple xs -> Tuple (List.map check xs)
  | Record fs ->
    let names = List.map fst fs in
    if List.length names <> List.length (List.sort_uniq compare names)
    then fail "Duplicate record field";
    Record (List.map (fun (n, x) -> n, check x) fs, None)
  | Field (x, n) ->
    let field = fresh level Any in
    equal (check x) (Record ([ n, field ], Some (fresh level Any)));
    field
  | Local (n, x, y) ->
    let t = expression env (level + 1) x in
    expression ((n, generalize level t) :: env) level y
  | Literal s when s.[0] = '"' -> String
  | Literal s ->
    if
      String.exists
        (function
          | '.' | 'e' | 'E' -> true
          | _ -> false)
        s
    then Float
    else Int
  | Lambda (ps, body) ->
    let ts = List.map (fun _ -> fresh level Any) ps in
    let locals = List.map2 (fun p t -> p, ([], t)) ps ts in
    Arrow (ts, expression (locals @ env) level body)
  | Call (f, args) -> call (check f) (List.map (fun (_, x) -> check x) args)
  | Binary ("|>", x, f) ->
    (match f.form with
     | Call (g, args) -> call (check g) (check x :: List.map (fun (_, a) -> check a) args)
     | _ -> call (check f) [ check x ])
  | Unary ("not", x) ->
    equal (check x) Bool;
    Bool
  | Unary ("-", x) ->
    let t = fresh level Numeric in
    equal t (check x);
    t
  | Binary (("and" | "or"), a, b) ->
    equal (check a) Bool;
    equal (check b) Bool;
    Bool
  | Binary (op, a, b) ->
    let t = fresh level Numeric in
    equal t (check a);
    equal t (check b);
    (match op with
     | ">" | "<" | ">=" | "<=" | "==" | "!=" -> Bool
     | "+" | "-" | "*" -> t
     | "/" ->
       equal t Float;
       Float
     | _ -> fail ("Unsupported operator: " ^ op))
  | If (c, a, b) ->
    equal (check c) Bool;
    let t = check a in
    equal t (check b);
    t
  | _ -> fail "Unsupported scalar expression"
;;

let declaration env = function
  | Alias (name, params, body) ->
    if List.mem_assoc ("type " ^ name) env then failwith ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, fresh 1 Any) params in
    let t = annotation env ps 1 body in
    ("type " ^ name, generalize 0 (Arrow (List.map snd ps, t))) :: env, t
  | Type (name, params, constructors) ->
    if List.mem_assoc ("type " ^ name) env then failwith ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, fresh 1 Any) params in
    let t = Variant (name, List.map snd ps) in
    let env = ("type " ^ name, generalize 0 t) :: env in
    let env =
      List.fold_left
        (fun env (n, fs) ->
           if List.mem_assoc n env then failwith ("Duplicate constructor " ^ n);
           let args = List.map (fun (_, a) -> annotation env ps 1 a) fs in
           (n, generalize 0 (if args = [] then t else Arrow (args, t))) :: env)
        env
        constructors
    in
    env, t
  | Let (name, ps, body) ->
    let x =
      match ps with
      | None -> body
      | Some ps -> { body with form = Lambda (ps, body) }
    in
    let t = expression env 1 x in
    (name, generalize 0 t) :: env, t
  | Evaluate x -> env, expression env 1 x
  | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
  | Contract _ -> failwith "Contract loading is unavailable"
;;

let display t =
  let vars = ref [] in
  let rec show t =
    match repr t with
    | Variant (n, []) -> n
    | Variant (n, ts) -> n ^ "<" ^ String.concat ", " (List.map show ts) ^ ">"
    | List t -> "List<" ^ show t ^ ">"
    | Option t -> show t ^ "?"
    | Tuple ts -> "(" ^ String.concat ", " (List.map show ts) ^ ")"
    | Record (fs, tail) ->
      "{"
      ^ String.concat
          ", "
          (List.map (fun (n, t) -> n ^ ": " ^ show t) fs
           @ Option.to_list (Option.map (fun t -> ".." ^ show t) tail))
      ^ "}"
    | Int -> "Int64"
    | Float -> "Float64"
    | Bool -> "Bool"
    | String -> "String"
    | Arrow (ps, r) -> "(" ^ String.concat ", " (List.map show ps) ^ ") -> " ^ show r
    | Var ({ contents = Free (_, c) } as v) ->
      (match List.find_opt (fun (k, _) -> k == v) !vars with
       | Some (_, s) -> s
       | None ->
         let prefix =
           match c with
           | Numeric -> "num"
           | Any -> "a"
         in
         let s = "'" ^ prefix ^ string_of_int (List.length !vars) in
         vars := (v, s) :: !vars;
         s)
    | Var _ -> assert false
  in
  show t
;;
