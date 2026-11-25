open Shape

type value =
  | I of int64
  | F of float
  | B of bool
  | S of string
  | Missing

type result =
  | One of value
  | Many of value array

let arithmetic op a b =
  let order c =
    match op with
    | Less -> B (c < 0)
    | Greater -> B (c > 0)
    | Less_equal -> B (c <= 0)
    | Greater_equal -> B (c >= 0)
    | Equal -> B (c = 0)
    | Not_equal -> B (c <> 0)
    | _ -> invalid_arg "Unexpected operator"
  in
  match op, a, b with
  | And, B false, _ | And, _, B false -> B false
  | Or, B true, _ | Or, _, B true -> B true
  | _, Missing, _ | _, _, Missing -> Missing
  | And, B a, B b -> B (a && b)
  | Or, B a, B b -> B (a || b)
  | Add, I a, I b -> I (Integer.add a b)
  | Sub, I a, I b -> I (Integer.sub a b)
  | Mul, I a, I b -> I (Integer.mul a b)
  | Mod, I a, I b -> I (Int64.rem a b)
  | Add, F a, F b -> F (a +. b)
  | Sub, F a, F b -> F (a -. b)
  | Mul, F a, F b -> F (a *. b)
  | Div, F a, F b -> F (a /. b)
  | Mod, F a, F b -> F (Float.rem a b)
  | Power, F a, F b -> F (Float.pow a b)
  | (Less | Greater | Less_equal | Greater_equal | Equal | Not_equal), _, _ ->
    order (compare a b)
  | _ -> invalid_arg "Unexpected operands in expression"
;;

let apply op v =
  match op, v with
  | _, Missing -> Missing
  | Syntax.Not, B b -> B (not b)
  | Negate, I i -> I (Integer.sub 0L i)
  | Negate, F f -> F (-.f)
  | _ -> invalid_arg "Unexpected operand"
;;

let present values = List.filter (fun v -> v <> Missing) (Array.to_list values)

let number = function
  | F f -> f
  | I i -> Int64.to_float i
  | _ -> invalid_arg "Expected a number"
;;

let aggregate (reduction : Prelude.reduction) values =
  let xs = present values in
  let floats = List.map number xs in
  let n = List.length xs in
  let mean () = List.fold_left ( +. ) 0. floats /. float_of_int n in
  let variance () =
    let m = mean () in
    List.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0. floats
    /. float_of_int (n - 1)
  in
  match reduction, xs with
  | Distinct, _ -> I (Int64.of_int (List.length (List.sort_uniq compare xs)))
  | Count, _ -> I (Int64.of_int (Array.length values))
  | _, [] -> Missing
  | (Var | Sd), [ _ ] -> Missing
  | Sum, I _ :: _ ->
    I
      (List.fold_left
         (fun acc v ->
            match v with
            | I i -> Integer.add acc i
            | _ -> acc)
         0L
         xs)
  | Sum, _ -> F (List.fold_left ( +. ) 0. floats)
  | Mean, _ -> F (mean ())
  | Median, _ ->
    let sorted = Array.of_list (List.sort compare floats) in
    F
      (if n mod 2 = 1
       then sorted.(n / 2)
       else (sorted.((n / 2) - 1) +. sorted.(n / 2)) /. 2.)
  | Var, _ -> F (variance ())
  | Sd, _ -> F (sqrt (variance ()))
  | Min, x :: rest -> List.fold_left min x rest
  | Max, x :: rest -> List.fold_left max x rest
;;

let round_even f =
  let r = Float.round f in
  if Float.abs (f -. Float.trunc f) = 0.5 && Float.rem r 2. <> 0.
  then r -. Float.copy_sign 1. f
  else r
;;

let apply_row (row : Prelude.row) args =
  let text = function
    | S s -> s
    | _ -> invalid_arg "Expected a string"
  in
  match row, args with
  | Present, [ v ] -> B (v <> Missing)
  | Absent, [ v ] -> B (v = Missing)
  | _, args when List.mem Missing args -> Missing
  | Abs, [ I i ] -> I (if i < 0L then Integer.sub 0L i else i)
  | Abs, [ v ] -> F (Float.abs (number v))
  | Sign, [ v ] ->
    I (Int64.of_float (Float.copy_sign (if number v = 0. then 0. else 1.) (number v)))
  | Sqrt, [ v ] -> F (sqrt (number v))
  | Exp, [ v ] -> F (exp (number v))
  | Log, [ v ] -> F (log (number v))
  | Log2, [ v ] -> F (Float.log2 (number v))
  | Log10, [ v ] -> F (log10 (number v))
  | Floor, [ v ] -> F (floor (number v))
  | Ceiling, [ v ] -> F (ceil (number v))
  | Trunc, [ v ] -> F (Float.trunc (number v))
  | Round, [ v ] -> F (round_even (number v))
  | Pmin, [ a; b ] -> if compare a b <= 0 then a else b
  | Pmax, [ a; b ] -> if compare a b >= 0 then a else b
  | Length, [ v ] ->
    let s = text v in
    let rec count i n =
      if i >= String.length s
      then n
      else count (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
    in
    I (Int64.of_int (count 0 0))
  | Lower, [ v ] -> S (String.lowercase_ascii (text v))
  | Upper, [ v ] -> S (String.uppercase_ascii (text v))
  | Trim, [ v ] -> S (String.trim (text v))
  | Starts, [ v; p ] -> B (String.starts_with ~prefix:(text p) (text v))
  | Ends, [ v; p ] -> B (String.ends_with ~suffix:(text p) (text v))
  | _ -> invalid_arg "Unexpected arguments"
;;

let present_or_fail = function
  | Missing ->
    Diagnostic.fail Missing_under_error "filter condition is missing and missing = error"
  | v -> v
;;

let evaluate ~rows ~input expression =
  let rec at i x =
    match x.node with
    | Constant (`Bool b) -> B b
    | Constant (`Float f) -> F f
    | Constant (`Int n) -> I n
    | Constant (`String s) -> S s
    | Null -> Missing
    | Input id ->
      (match input id with
       | Many xs -> xs.(i)
       | One v -> v)
    | Broadcast (_, e) -> at i e
    | Binary (op, a, b) -> arithmetic op (at i a) (at i b)
    | Unary (op, a) -> apply op (at i a)
    | Required a -> present_or_fail (at i a)
    | Coalesce (a, b) ->
      (match at i a with
       | Missing -> at i b
       | v -> v)
    | Choose (c, t, f) ->
      (match at i c with
       | B true -> at i t
       | B false -> at i f
       | _ -> invalid_arg "Expected Bool")
    | Polars_sqrt a | Apply (Sqrt, [ a ]) -> apply_row Sqrt [ at i a ]
    | Apply (row, args) -> apply_row row (List.map (at i) args)
    | Reduce (r, policy, e) ->
      let values = Array.init rows (fun j -> at j e) in
      (match policy with
       | Propagate when Array.mem Missing values -> Missing
       | Fail when Array.mem Missing values ->
         Diagnostic.fail
           Missing_under_error
           "a reduction met a missing value and missing = error"
       | Skip | Propagate | Fail -> aggregate r values)
    | Count -> I (Int64.of_int rows)
    | Text parts ->
      let pieces =
        List.map
          (function
            | Fixed s -> Some s
            | Shown (_, Some _) ->
              Diagnostic.fail
                Unsupported_feature
                "Format specs are not in the reference evaluator"
            | Shown (x, None) ->
              (match at i x with
               | Missing -> None
               | S s -> Some s
               | I n -> Some (Int64.to_string n)
               | F f -> Some (Display.float f)
               | B b -> Some (Display.bool b)))
          parts
      in
      if List.mem None pieces
      then Missing
      else S (String.concat "" (List.filter_map Fun.id pieces))
    | Convert a ->
      (match at i a with
       | I v -> F (Int64.to_float v)
       | v -> v)
    | Member (a, items) ->
      (match at i a with
       | Missing -> Missing
       | v -> B (List.exists (fun item -> at i item = v) items))
  in
  match expression.typ.shape with
  | Scalar -> One (at 0 expression)
  | Column _ -> Many (Array.init rows (fun i -> at i expression))
;;
