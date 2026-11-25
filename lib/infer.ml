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

type env =
  { values : (string * scheme) list
  ; types : (string * scheme) list
  ; variants : (string * (string * scheme) list) list
  ; columns : (string * typ) list
  }

let extend env xs = { env with values = xs @ env.values }

exception Error of span * string

let fresh level c = Var (ref (Free (level, c)))

let rec repr = function
  | Var { contents = Link t } -> repr t
  | t -> t
;;

type decision =
  | Integer of typ
  | Lifted of bool list * typ

module Decisions = Hashtbl.Make (struct
    type t = Syntax.expr

    let equal = ( == )
    let hash = Hashtbl.hash
  end)

let decisions : decision Decisions.t = Decisions.create 256
let decide x d = Decisions.replace decisions x d
let conversions : unit Decisions.t = Decisions.create 64
let convert x = Decisions.replace conversions x ()
let converted x = Decisions.mem conversions x

let promoted x =
  match Decisions.find_opt decisions x with
  | Some (Integer t) ->
    (match repr t with
     | Float -> true
     | _ -> false)
  | _ -> false
;;

let lifted x =
  match Decisions.find_opt decisions x with
  | Some (Lifted (flags, _)) when List.mem true flags -> Some flags
  | _ -> None
;;

let operand x =
  match Decisions.find_opt decisions x with
  | Some (Lifted (_, t)) -> Some (repr t)
  | _ -> None
;;

let unsettled x =
  match Decisions.find_opt decisions x with
  | Some (Integer t) ->
    (match repr t with
     | Var _ -> true
     | _ -> false)
  | _ -> false
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
       | Some (_, (_, s)) -> s
       | None ->
         let same = List.length (List.filter (fun (_, (k, _)) -> k = c) !vars) in
         let s =
           match c, same with
           | Numeric, 0 -> "Num"
           | Numeric, n -> "Num" ^ string_of_int (n + 1)
           | Any, n when n < 4 -> String.make 1 "TUVW".[n]
           | Any, n -> "T" ^ string_of_int (n + 1)
         in
         vars := (v, (c, s)) :: !vars;
         s)
    | Var _ -> assert false
  in
  show t
;;

exception Mismatch

let unify span expected actual =
  let fail () = raise Mismatch in
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
  try equal expected actual with
  | Mismatch ->
    raise
      (Error
         (span, Printf.sprintf "Expected %s, found %s" (display expected) (display actual)))
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
       (match List.assoc_opt n env.types with
        | Some (_, Variant (_, ps)) when List.length ps = List.length ts -> Variant (n, ts)
        | Some scheme ->
          (match instantiate level scheme with
           | Arrow (params, result) when List.length params = List.length ts ->
             List.iter2 (unify (Lexing.dummy_pos, Lexing.dummy_pos)) params ts;
             result
           | _ -> failwith ("Wrong type arity: " ^ n))
        | _ -> failwith ("Unknown type or wrong arity: " ^ n)))
;;

let signature builtin =
  let vars = ref [] in
  let var c =
    let v = ref (Free (max_int, c)) in
    vars := v :: !vars;
    Var v
  in
  let t =
    match (builtin : Prelude.builtin) with
    | Coalesce ->
      let a = var Any in
      Arrow ([ Option a; a ], a)
    | Row Abs ->
      let a = var Numeric in
      Arrow ([ a ], a)
    | Row Sign -> Arrow ([ var Numeric ], Int)
    | Row (Sqrt | Exp | Log | Log2 | Log10 | Floor | Ceiling | Trunc | Round) ->
      Arrow ([ var Numeric ], Float)
    | Row (Pmin | Pmax) ->
      let a = var Numeric in
      Arrow ([ a; a ], a)
    | Row (Present | Absent) -> Arrow ([ Option (var Any) ], Bool)
    | Row Length -> Arrow ([ String ], Int)
    | Row (Lower | Upper | Trim) -> Arrow ([ String ], String)
    | Row (Starts | Ends | Detect) -> Arrow ([ String; String ], Bool)
    | Reduction (Sum | Min | Max) ->
      let a = var Numeric in
      Arrow ([ Option a ], Option a)
    | Reduction (Mean | Median | Var | Sd) ->
      Arrow ([ Option (var Numeric) ], Option Float)
    | Reduction Distinct -> Arrow ([ Option (var Any) ], Int)
    | Reduction Count -> Arrow ([], Int)
  in
  !vars, t
;;

let builtins xs = List.map (fun (n, b) -> n, signature b) xs
let empty = { values = builtins Prelude.scalar; types = []; variants = []; columns = [] }

let rows env columns =
  let reductions = List.map (fun (n, r) -> n, Prelude.Reduction r) Prelude.reductions in
  { env with columns; values = builtins reductions @ env.values }
;;

let optional t =
  match repr t with
  | Option _ -> t
  | t -> Option t
;;

let literal level = function
  | Syntax.Int _ -> Int
  | Syntax.Float _ -> Float
  | Syntax.Text _ -> String
  | Syntax.Bool _ -> Bool
  | Syntax.Absent -> Option (fresh level Any)
;;

type head =
  | Constructor of string
  | Constant of Syntax.literal
  | Product

let uncovered env level equal subject branches =
  let heads t =
    match repr t with
    | Bool -> [ Constant (Syntax.Bool true), []; Constant (Syntax.Bool false), [] ]
    | Option inner -> [ Constant Syntax.Absent, []; Constructor "Some", [ inner ] ]
    | Tuple fields -> [ Product, fields ]
    | Variant (name, _) ->
      List.assoc name env.variants
      |> List.map (fun (tag, scheme) ->
        let fields, result =
          match instantiate level scheme with
          | Arrow (fields, result) -> fields, result
          | result -> [], result
        in
        equal t result;
        Constructor tag, fields)
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
    let integral =
      match repr t with
      | Int -> true
      | _ -> false
    in
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
    let intervals =
      List.filter_map
        (function
          | p :: rest when List.for_all catchall rest -> interval p
          | _ -> None)
        rows
      |> List.sort compare
    in
    let reach, _ =
      List.fold_left
        (fun (reach, closed) (low, high, high_closed) ->
           if low <= reach
           then
             if high > reach
             then high, high_closed
             else reach, closed || (high = reach && high_closed)
           else reach, closed)
        (neg_infinity, false)
        intervals
    in
    reach = infinity
  in
  let specialize head arity = function
    | p :: ps when catchall p -> Some (List.init arity (fun _ -> Wildcard) @ ps)
    | p :: ps ->
      (match head, p with
       | Constructor n, Tag (m, vs) when n = m -> Some (vs @ ps)
       | Constant l, Pliteral m when l = m -> Some ps
       | Product, Ptuple vs -> Some (vs @ ps)
       | _ -> None)
    | [] -> None
  in
  let rebuild head arity witness =
    let args = List.filteri (fun i _ -> i < arity) witness
    and rest = List.filteri (fun i _ -> i >= arity) witness in
    let p =
      match head with
      | Constructor n -> Tag (n, args)
      | Constant l -> Pliteral l
      | Product -> Ptuple args
    in
    p :: rest
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
  let unguarded = List.filter (fun arm -> arm.guard = None) branches in
  match missing [ subject ] (List.map (fun arm -> [ arm.pattern ]) unguarded) with
  | Some [ p ] -> Some p
  | _ -> None
;;

let rec expression env level source =
  let check = expression env level
  and equal = unify source.span in
  let fail m = raise (Error (source.span, m)) in
  let strip t =
    match repr t with
    | Option inner -> inner, true
    | t -> t, false
  in
  let call f arguments =
    let args = List.map check arguments in
    match repr f with
    | Arrow (ps, r) when List.length ps = List.length args ->
      let integer t =
        match repr t with
        | Int -> true
        | _ -> false
      in
      let flags =
        List.map2
          (fun p (x, a) ->
             match repr p, repr a with
             | Option _, Option _ | Var { contents = Free (_, Any) }, _ ->
               equal p a;
               false
             | Option inner, Int when repr inner = Float ->
               convert x;
               false
             | Option inner, _ ->
               equal inner a;
               false
             | Float, Int ->
               convert x;
               false
             | Float, Option inner when integer inner ->
               convert x;
               true
             | _, Option inner ->
               equal p inner;
               true
             | _ ->
               equal p a;
               false)
          ps
          (List.combine arguments args)
      in
      decide source (Lifted (flags, r));
      if List.mem true flags then optional r else r
    | Arrow (ps, _) ->
      fail
        (Printf.sprintf
           "Expected %d argument%s, got %d"
           (List.length ps)
           (if List.length ps = 1 then "" else "s")
           (List.length args))
    | _ ->
      let r = fresh level Any in
      equal f (Arrow (args, r));
      r
  in
  let operands a b =
    let ta, la = strip (check a)
    and tb, lb = strip (check b) in
    let common =
      match repr ta, repr tb with
      | Int, Float ->
        convert a;
        tb
      | Float, Int ->
        convert b;
        ta
      | _ ->
        equal ta tb;
        ta
    in
    decide source (Lifted ([ la; lb ], common));
    common, la || lb
  in
  let lift lifted t = if lifted then optional t else t in
  match source.form with
  | Annotated (x, a) ->
    let t = annotation env [] level a in
    let v = check x in
    (match repr t, repr v with
     | Option _, (Option _ | Var _) -> equal t v
     | Option inner, _ -> equal inner v
     | _ -> equal t v);
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
      | Pliteral l ->
        equal t (literal level l);
        []
      | Ptuple ps ->
        let ts = List.map (fun _ -> fresh level Any) ps in
        equal t (Tuple ts);
        List.concat (List.map2 bind ts ps)
      | Prange { low; high; _ } ->
        equal (fresh level Numeric) t;
        List.iter
          (function
            | Syntax.Float _ -> equal Float t
            | _ -> ())
          (Option.to_list low @ Option.to_list high);
        []
      | Pas (n, p) -> (n, ([], t)) :: bind t p
      | Palt [] -> []
      | Palt (first :: rest) ->
        let vars = bind t first in
        List.iter
          (fun p ->
             let others = bind t p in
             if
               List.sort compare (List.map fst others)
               <> List.sort compare (List.map fst vars)
             then fail "Every alternative must bind the same names";
             List.iter (fun (n, (_, ty)) -> equal (snd (List.assoc n vars)) ty) others)
          rest;
        vars
      | Tag (n, ps) ->
        let ts = List.map (fun _ -> fresh level Any) ps in
        (match n, ts with
         | "Some", [ v ] -> equal t (Option v)
         | _ ->
           let constr =
             match List.find_map (fun (_, cs) -> List.assoc_opt n cs) env.variants with
             | Some s -> instantiate level s
             | None -> fail ("Unknown constructor " ^ n)
           in
           equal constr (if ts = [] then t else Arrow (ts, t)));
        List.concat (List.map2 bind ts ps)
    in
    List.iter
      (fun arm ->
         let vars = bind subject arm.pattern in
         let ns = List.map fst vars in
         if List.length ns <> List.length (List.sort_uniq compare ns)
         then fail "Repeated pattern variable";
         let inner = extend env vars in
         Option.iter (fun g -> equal Bool (expression inner level g)) arm.guard;
         equal result (expression inner level arm.body))
      branches;
    (match uncovered env level equal subject branches with
     | Some p -> fail ("Non-exhaustive match. Missing case " ^ Format.pattern p)
     | None -> ());
    result
  | Member (x, set) ->
    let element, lifted = strip (check x) in
    equal (List element) (check set);
    decide source (Lifted ([ lifted; false ], element));
    lift lifted Bool
  | Name (Column, n) ->
    (match List.assoc_opt n env.columns with
     | Some t -> t
     | None -> fail ("No column named " ^ n))
  | Name (Bare, n) when List.mem_assoc n env.columns -> List.assoc n env.columns
  | Name (_, n) ->
    (match List.assoc_opt n env.values with
     | Some s -> instantiate level s
     | None ->
       let names = List.map fst env.columns @ List.map fst env.values in
       (match
          List.filter (fun m -> Distance.between m n <= 2) names |> List.sort_uniq compare
        with
        | [] -> fail ("Unknown name: " ^ n)
        | m :: _ -> fail ("Unknown name: " ^ n ^ ". Did you mean " ^ m ^ "?")))
  | Literal (Int _) ->
    let t = fresh level Numeric in
    decide source (Integer t);
    t
  | Literal l -> literal level l
  | Call ({ form = Name (Bare, "Some"); _ }, [ (None, x) ]) -> optional (check x)
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
    expression (extend env [ n, generalize level t ]) level y
  | Lambda (ps, body) ->
    let ts = List.map (fun _ -> fresh level Any) ps in
    let locals = List.map2 (fun p t -> p, ([], t)) ps ts in
    Arrow (ts, expression (extend env locals) level body)
  | Call (f, args) ->
    let configuration (label, _) = label = Some "missing" in
    call (check f) (List.map snd (List.filter (fun a -> not (configuration a)) args))
  | Pipe (x, f) ->
    (match f.form with
     | Call (g, args) -> call (check g) (x :: List.map snd args)
     | _ -> call (check f) [ x ])
  | Unary (op, x) ->
    let t, lifted = strip (check x) in
    decide source (Lifted ([ lifted ], t));
    (match op with
     | Not -> equal Bool t
     | Negate -> equal (fresh level Numeric) t);
    lift lifted t
  | Binary (op, a, b) ->
    let t, lifted = operands a b in
    lift
      lifted
      (match op with
       | And | Or ->
         equal Bool t;
         Bool
       | Equal | Not_equal -> Bool
       | Less | Greater | Less_equal | Greater_equal ->
         (match repr t with
          | String -> ()
          | t -> equal (fresh level Numeric) t);
         Bool
       | Add | Sub | Mul | Mod ->
         equal (fresh level Numeric) t;
         t
       | Div | Power ->
         equal (fresh level Numeric) t;
         (match repr t with
          | Int ->
            convert a;
            convert b
          | _ -> ());
         Float)
  | If (c, a, b) ->
    (match repr (check c) with
     | Option _ ->
       fail
         "This condition is missing where its inputs are missing. Wrap it in \
          coalesce(condition, FALSE) or coalesce(condition, TRUE)"
     | t -> equal Bool t);
    let ta = check a
    and tb = check b in
    (match repr ta, repr tb with
     | Option inner, (Int | Float | Bool | String) ->
       equal inner tb;
       ta
     | (Int | Float | Bool | String), Option inner ->
       equal ta inner;
       tb
     | _ ->
       equal ta tb;
       ta)
  | Formula _ -> fail "Formulas (y ~ x) are reserved for the model and graphics libraries"
  | _ -> fail "Unsupported scalar expression"
;;

let settle t =
  variables
    (fun v ->
       match !v with
       | Free (_, Numeric) -> v := Link Int
       | _ -> ())
    t;
  t
;;

let declaration env = function
  | Alias (name, params, body) ->
    if List.mem_assoc name env.types then failwith ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, fresh 1 Any) params in
    let t = annotation env ps 1 body in
    { env with types = (name, generalize 0 (Arrow (List.map snd ps, t))) :: env.types }, t
  | Type (name, params, constructors) ->
    if List.mem_assoc name env.types then failwith ("Duplicate type " ^ name);
    let ps = List.map (fun n -> n, fresh 1 Any) params in
    let t = Variant (name, List.map snd ps) in
    let env = { env with types = (name, generalize 0 t) :: env.types } in
    let known tag = List.exists (fun (_, cs) -> List.mem_assoc tag cs) env.variants in
    let tags =
      List.fold_left
        (fun tags (tag, fs) ->
           if known tag || List.mem_assoc tag tags
           then failwith ("Duplicate constructor " ^ tag);
           let args = List.map (fun (_, a) -> annotation env ps 1 a) fs in
           (tag, generalize 0 (if args = [] then t else Arrow (args, t))) :: tags)
        []
        constructors
      |> List.rev
    in
    { (extend env tags) with variants = (name, tags) :: env.variants }, t
  | Let (name, ps, body) ->
    let x =
      match ps with
      | None -> body
      | Some ps -> { body with form = Lambda (ps, body) }
    in
    let t = expression env 1 x in
    let t = if ps = None then settle t else t in
    extend env [ name, generalize 0 t ], t
  | Evaluate x -> env, settle (expression env 1 x)
  | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
  | Contract _ -> failwith "Contract loading is unavailable"
  | Setting (("missing" | "group_missing"), ("skip" | "propagate" | "error")) ->
    env, Tuple []
  | Setting (("missing" | "group_missing"), _) ->
    failwith "A missing policy is skip, propagate or error"
  | Setting (name, _) -> failwith ("Unknown setting " ^ name ^ ". Expected missing or group_missing")
;;

let program env ds = List.fold_left (fun env d -> fst (declaration env d)) env ds

let rec descriptor t =
  match repr t with
  | Int -> `String "int"
  | Float -> `String "float"
  | Bool -> `String "bool"
  | String -> `String "string"
  | List t -> `List [ `String "list"; descriptor t ]
  | Option t -> `List [ `String "option"; descriptor t ]
  | Tuple ts -> `List (`String "tuple" :: List.map descriptor ts)
  | Record (fields, _) ->
    `List
      [ `String "record"
      ; `List (List.map (fun (n, t) -> `List [ `String n; descriptor t ]) fields)
      ]
  | Variant _ | Var _ -> `String "any"
  | Arrow _ -> `String "function"
;;
