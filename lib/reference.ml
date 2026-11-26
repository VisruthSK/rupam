module C = TransformIR
module T = Types

type value =
  | Int of int64
  | Float of float
  | Bool of bool
  | Text of string
  | Missing
  | Tuple of value list
  | List of value list
  | Record of (string * value) list
  | Variant of string * value list
  | Closure of C.id list * C.expr * env
  | Primitive of string
  | Frame of frame

and frame =
  { columns : (string * value array) list
  ; rows : int
  }

and env = (int * value) list

type rows =
  { frame : frame
  ; group : int array
  ; row : int
  }

let fail (e : C.expr) code message = Diagnostic.fail ~span:e.span code message

let rec display = function
  | Int n -> Int64.to_string n
  | Float f -> Display.float f
  | Bool b -> Display.bool b
  | Text s -> Printf.sprintf "%S" s
  | Missing -> "None"
  | Tuple vs -> "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | List vs -> "[" ^ String.concat ", " (List.map display vs) ^ "]"
  | Record fs ->
    "{" ^ String.concat ", " (List.map (fun (n, v) -> n ^ " = " ^ display v) fs) ^ "}"
  | Variant (n, []) -> n
  | Variant (n, vs) -> n ^ "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | Closure _ | Primitive _ -> "<function>"
  | Frame f -> Printf.sprintf "<table of %d rows>" f.rows
;;

let rows = function
  | Frame f ->
    Some
      ( List.map fst f.columns
      , List.init f.rows (fun i -> List.map (fun (_, column) -> display column.(i)) f.columns)
      )
  | _ -> None
;;

let integer = function
  | Int i -> i
  | _ -> invalid_arg "Expected an integer"
;;

let number = function
  | Int i -> Int64.to_float i
  | Float f -> f
  | _ -> invalid_arg "Expected a number"
;;

let text = function
  | Text s -> s
  | _ -> invalid_arg "Expected a string"
;;

let truth = function
  | Bool b -> b
  | _ -> invalid_arg "Expected a Bool"
;;

let checked e f =
  match f () with
  | Float x when not (Float.is_finite x) ->
    fail e Arithmetic_overflow "Arithmetic overflow: a result is not finite"
  | v -> v
  | exception Integer.Overflow ->
    fail e Arithmetic_overflow "Arithmetic overflow: Int64 out of range"
;;

let compare_values a b =
  match a, b with
  | Int a, Float b -> compare (Int64.to_float a) b
  | Float a, Int b -> compare a (Int64.to_float b)
  | a, b -> compare a b
;;

let binary e (op : Syntax.binary) a b =
  match op, a, b with
  | And, Bool false, _ | And, _, Bool false -> Bool false
  | Or, Bool true, _ | Or, _, Bool true -> Bool true
  | _, Missing, _ | _, _, Missing -> Missing
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | (And | Or), _, _ -> invalid_arg "Expected Bool operands"
  | Equal, a, b -> Bool (compare_values a b = 0)
  | Not_equal, a, b -> Bool (compare_values a b <> 0)
  | Less, a, b -> Bool (compare_values a b < 0)
  | Greater, a, b -> Bool (compare_values a b > 0)
  | Less_equal, a, b -> Bool (compare_values a b <= 0)
  | Greater_equal, a, b -> Bool (compare_values a b >= 0)
  | Add, Int a, Int b -> checked e (fun () -> Int (Integer.add a b))
  | Sub, Int a, Int b -> checked e (fun () -> Int (Integer.sub a b))
  | Mul, Int a, Int b -> checked e (fun () -> Int (Integer.mul a b))
  | Mod, Int a, Int b ->
    let r = Int64.rem a b in
    Int (if r <> 0L && r < 0L <> (b < 0L) then Int64.add r b else r)
  | Add, a, b -> checked e (fun () -> Float (number a +. number b))
  | Sub, a, b -> checked e (fun () -> Float (number a -. number b))
  | Mul, a, b -> checked e (fun () -> Float (number a *. number b))
  | Div, a, b -> checked e (fun () -> Float (number a /. number b))
  | Power, a, b -> checked e (fun () -> Float (Float.pow (number a) (number b)))
  | Mod, a, b ->
    let a = number a
    and b = number b in
    let r = Float.rem a b in
    Float (if r <> 0. && r < 0. <> (b < 0.) then r +. b else r)
;;

let round_even f =
  let r = Float.round f in
  let r =
    if Float.abs (f -. Float.trunc f) = 0.5 && Float.rem r 2. <> 0.
    then r -. Float.copy_sign 1. f
    else r
  in
  Float.copy_sign r f
;;

let missing_last a b =
  match a, b with
  | Missing, Missing -> 0
  | Missing, _ -> 1
  | _, Missing -> -1
  | a, b -> compare_values a b
;;

let sequence e first last step =
  let ints = List.for_all (function Int _ -> true | _ -> false) [ first; last; step ] in
  let first = number first and last = number last and step = number step in
  if step = 0.
  then fail e Wrong_arguments "seq needs a nonzero step"
  else (
    let rec go i acc =
      let v = first +. (float_of_int i *. step) in
      if (step > 0. && v <= last) || (step < 0. && v >= last)
      then go (i + 1) ((if ints then Int (Int64.of_float v) else Float v) :: acc)
      else List.rev acc
    in
    List (go 0 []))
;;

let builtin e call (row : Prelude.row) args =
  match row, args with
  | Present, [ v ] -> Bool (v <> Missing)
  | Absent, [ v ] -> Bool (v = Missing)
  | Size, [ List xs ] -> Int (Int64.of_int (List.length xs))
  | Reverse, [ List xs ] -> List (List.rev xs)
  | Sorted, [ List xs ] -> List (List.stable_sort missing_last xs)
  | Deduplicate, [ List xs ] ->
    List
      (List.rev
         (List.fold_left (fun seen x -> if List.mem x seen then seen else x :: seen) [] xs))
  | Map_each, [ List xs; f ] -> List (List.map (fun x -> call f [ x ]) xs)
  | Retain, [ List xs; f ] -> List (List.filter (fun x -> call f [ x ] = Bool true) xs)
  | Fold, [ List xs; init; f ] -> List.fold_left (fun acc x -> call f [ acc; x ]) init xs
  | _, args when List.mem Missing args -> Missing
  | Abs, [ Int i ] -> checked e (fun () -> Int (if i < 0L then Integer.sub 0L i else i))
  | Abs, [ v ] -> Float (Float.abs (number v))
  | Sign, [ v ] ->
    Int (Int64.of_float (Float.copy_sign (if number v = 0. then 0. else 1.) (number v)))
  | Sqrt, [ v ] -> checked e (fun () -> Float (sqrt (number v)))
  | Exp, [ v ] -> checked e (fun () -> Float (exp (number v)))
  | Log, [ v ] -> checked e (fun () -> Float (log (number v)))
  | Log2, [ v ] -> checked e (fun () -> Float (Float.log2 (number v)))
  | Log10, [ v ] -> checked e (fun () -> Float (log10 (number v)))
  | Floor, [ v ] -> Float (floor (number v))
  | Ceiling, [ v ] -> Float (ceil (number v))
  | Trunc, [ v ] -> Float (Float.trunc (number v))
  | Round, [ v ] -> Float (round_even (number v))
  | Round_digits, [ v; d ] ->
    let scale = 10. ** number d in
    Float (round_even (number v *. scale) /. scale)
  | Pmin, [ a; b ] -> if compare_values a b <= 0 then a else b
  | Pmax, [ a; b ] -> if compare_values a b >= 0 then a else b
  | Length, [ v ] -> Int (Int64.of_int (Display.width (text v)))
  | Lower, [ v ] -> Text (String.lowercase_ascii (text v))
  | Upper, [ v ] -> Text (String.uppercase_ascii (text v))
  | Trim, [ v ] -> Text (String.trim (text v))
  | Starts, [ v; p ] -> Bool (String.starts_with ~prefix:(text p) (text v))
  | Ends, [ v; p ] -> Bool (String.ends_with ~suffix:(text p) (text v))
  | Detect, [ v; p ] -> Bool (Re.execp (Re.compile (Re.Perl.re (text p))) (text v))
  | Sequence, [ first; last; step ] -> sequence e first last step
  | _ -> invalid_arg "Wrong arguments to a builtin"
;;

let aggregate e (reduction : Prelude.reduction) policy values =
  if policy = C.Fail && Array.mem Missing values
  then fail e Missing_under_error "A reduction met a missing value and missing = error"
  else if
    policy = C.Keep
    && (not (Prelude.missing_is_value reduction))
    && Array.mem Missing values
  then Missing
  else (
    let xs = List.filter (( <> ) Missing) (Array.to_list values) in
    let floats = List.map number in
    let n = List.length xs in
    let mean () = List.fold_left ( +. ) 0. (floats xs) /. float_of_int n in
    let variance () =
      let m = mean () in
      List.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0. (floats xs)
      /. float_of_int (n - 1)
    in
    match reduction, xs with
    | Count, _ -> Int (Int64.of_int (Array.length values))
    | (First | Last), _ ->
      let kept = if policy = C.Keep then Array.to_list values else xs in
      (match if reduction = First then kept else List.rev kept with
       | v :: _ -> v
       | [] -> Missing)
    | Distinct, _ ->
      let kept = if policy = C.Keep then Array.to_list values else xs in
      Int (Int64.of_int (List.length (List.sort_uniq compare kept)))
    | _, [] -> Missing
    | (Var | Sd), [ _ ] -> Missing
    | Sum, Int _ :: _ ->
      checked e (fun () ->
        Int
          (List.fold_left
             (fun acc v ->
                match v with
                | Int i -> Integer.add acc i
                | _ -> acc)
             0L
             xs))
    | Sum, _ -> checked e (fun () -> Float (List.fold_left ( +. ) 0. (floats xs)))
    | Mean, _ -> Float (mean ())
    | Median, _ ->
      let sorted = Array.of_list (List.sort compare (floats xs)) in
      Float
        (if n mod 2 = 1
         then sorted.(n / 2)
         else (sorted.((n / 2) - 1) +. sorted.(n / 2)) /. 2.)
    | Var, _ -> Float (variance ())
    | Sd, _ -> Float (sqrt (variance ()))
    | Min, x :: rest ->
      List.fold_left (fun a b -> if compare_values a b <= 0 then a else b) x rest
    | Max, x :: rest ->
      List.fold_left (fun a b -> if compare_values a b >= 0 then a else b) x rest)
;;

let rec matches env (p : C.pattern) v =
  match p, v with
  | Wildcard, _ -> Some env
  | Bind id, v -> Some ((id.stamp, v) :: env)
  | Named (id, p), v -> matches ((id.stamp, v) :: env) p v
  | Missing, Missing -> Some env
  | Missing, _ -> None
  | Present _, Missing -> None
  | Present p, v -> matches env p v
  | Equals c, v -> if compare_values (constant c) v = 0 then Some env else None
  | Between (low, high), v when v <> Missing ->
    let within side (c, closed) =
      let d = side * compare_values v (constant c) in
      if closed then d >= 0 else d > 0
    in
    let above = Option.fold ~none:true ~some:(within 1) low in
    let below = Option.fold ~none:true ~some:(within (-1)) high in
    if above && below then Some env else None
  | Between _, _ -> None
  | Tag (n, ps), Variant (m, vs) when n = m -> all env ps vs
  | Tag _, _ -> None
  | Elements ps, Tuple vs -> all env ps vs
  | Elements _, _ -> None
  | Fields fs, Record values ->
    Some (List.map (fun (n, id) -> id.C.stamp, List.assoc n values) fs @ env)
  | Fields _, _ -> None
  | Either ps, v -> List.find_map (fun p -> matches env p v) ps

and all env ps vs =
  match ps, vs with
  | [], [] -> Some env
  | p :: ps, v :: vs -> Option.bind (matches env p v) (fun env -> all env ps vs)
  | _ -> None

and constant : C.constant -> value = function
  | Int i -> Int i
  | Float f -> Float f
  | Number s -> Int (Int64.of_string s)
  | Text s -> Text s
  | Bool b -> Bool b
;;

let rec eval env ?rows (e : C.expr) =
  let here = eval env ?rows in
  match e.node with
  | Constant c ->
    (match c, T.repr (T.present e.typ) with
     | Number s, Float -> Float (float_of_string s)
     | c, _ -> constant c)
  | Absent -> Missing
  | Variable { stamp = 0; name } -> Primitive name
  | Variable id ->
    (match List.assoc_opt id.stamp env with
     | Some v -> v
     | None -> fail e Unknown_name ("Unbound " ^ id.name))
  | Column n ->
    (match rows with
     | Some { frame; row; _ } -> (List.assoc n frame.columns).(row)
     | None -> invalid_arg "A column outside a table")
  | Count ->
    (match rows with
     | Some { group; _ } -> Int (Int64.of_int (Array.length group))
     | None -> invalid_arg "n() outside a table")
  | Reduce (r, policy, x) ->
    (match rows with
     | Some ({ group; _ } as at) ->
       aggregate
         e
         r
         policy
         (Array.map (fun row -> eval env ~rows:{ at with row } x) group)
     | None -> invalid_arg "A reduction outside a table")
  | Builtin (row, xs) -> builtin e (apply e) row (List.map here xs)
  | Unary (op, x) ->
    (match op, here x with
     | _, Missing -> Missing
     | Not, Bool b -> Bool (not b)
     | Negate, Int i -> checked e (fun () -> Int (Integer.sub 0L i))
     | Negate, Float f -> Float (-.f)
     | _ -> invalid_arg "Wrong operand")
  | Binary (((And | Or) as op), a, b) ->
    (match op, here a with
     | And, Bool false -> Bool false
     | Or, Bool true -> Bool true
     | _, va -> binary e op va (here b))
  | Binary (op, a, b) -> binary e op (here a) (here b)
  | Convert x ->
    (match here x with
     | Int i ->
       let f = Int64.to_float i in
       if Int64.of_float f <> i
       then fail e Inexact_float "An integer is not exactly representable as Float64"
       else Float f
     | v -> v)
  | Lift (bound, body) ->
    let values = List.map (fun (id, x) -> id.C.stamp, here x) bound in
    if List.exists (fun (_, v) -> v = Missing) values
    then Missing
    else eval (values @ env) ?rows body
  | If (c, a, b) -> if truth (here c) then here a else here b
  | Coalesce (a, b) ->
    (match here a with
     | Missing -> here b
     | v -> v)
  | Require x ->
    (match here x with
     | Missing -> fail e Missing_under_error "A condition is missing and missing = error"
     | v -> v)
  | Match (x, arms) ->
    let v = here x in
    (match
       List.find_map
         (fun (arm : C.arm) ->
            Option.bind (matches env arm.pattern v) (fun inner ->
              match arm.guard with
              | Some g when not (truth (eval inner ?rows g)) -> None
              | _ -> Some (inner, arm.body)))
         arms
     with
     | Some (inner, body) -> eval inner ?rows body
     | None -> fail e Nonexhaustive_match "No match arm applies")
  | Lambda (ids, body) -> Closure (ids, body, env)
  | Apply (f, xs) -> apply e (here f) (List.map here xs)
  | Construct (n, xs) -> Variant (n, List.map here xs)
  | Let (id, v, body) -> eval ((id.stamp, here v) :: env) ?rows body
  | Tuple xs -> Tuple (List.map here xs)
  | List xs -> List (List.map here xs)
  | Sequence (a, b) ->
    (match here a, here b with
     | Int a, Int b ->
       List
         (List.init
            (max 0 (Int64.to_int (Int64.sub b a) + 1))
            (fun k -> Int (Int64.add a (Int64.of_int k))))
     | _ -> invalid_arg "Expected integers")
  | Element (x, i) ->
    (match here x with
     | List items ->
       let n = List.length items in
       let i = Option.fold ~none:n ~some:(fun i -> Int64.to_int (integer (here i))) i in
       if i < 1 || i > n
       then fail e Wrong_arguments (Printf.sprintf "index %d is outside 1..%d" i n)
       else List.nth items (i - 1)
     | _ -> invalid_arg "Expected a list")
  | Slice (x, a, b) ->
    (match here x with
     | List items ->
       let n = List.length items in
       let a = Option.fold ~none:1 ~some:(fun i -> Int64.to_int (integer (here i))) a in
       let b = Option.fold ~none:n ~some:(fun i -> Int64.to_int (integer (here i))) b in
       if a < 1 || b > n
       then fail e Wrong_arguments (Printf.sprintf "slice %d..=%d is outside 1..%d" a b n)
       else List (List.filteri (fun k _ -> k + 1 >= a && k + 1 <= b) items)
     | _ -> invalid_arg "Expected a list")
  | Record fs -> Record (List.map (fun (n, x) -> n, here x) fs)
  | Field (x, n) ->
    (match here x with
     | Record fs -> List.assoc n fs
     | _ -> invalid_arg "Expected a record")
  | Member (x, set) ->
    (match here x, here set with
     | Missing, _ -> Missing
     | v, List items -> Bool (List.exists (fun i -> compare_values i v = 0) items)
     | _ -> invalid_arg "Expected a list")
  | Foreign (_, "sqrt", [ x ]) -> builtin e (apply e) Sqrt [ here x ]
  | Foreign (m, f, _) ->
    fail e Dialect_capability (m ^ "." ^ f ^ " has no reference semantics")
  | Text pieces ->
    let parts =
      List.map
        (function
          | C.Fixed s -> Some s
          | Shown (x, Some _) ->
            fail x Unsupported_feature "Format specs are not in the reference evaluator"
          | Shown (x, None) ->
            (match here x with
             | Missing -> None
             | Text s -> Some s
             | v -> Some (display v)))
        pieces
    in
    if List.mem None parts
    then Missing
    else Text (String.concat "" (List.filter_map Fun.id parts))
  | Table t -> Frame (table env t)

and apply e f args =
  match f with
  | Closure (ids, body, captured) ->
    eval (List.map2 (fun (id : C.id) v -> id.stamp, v) ids args @ captured) body
  | Primitive name ->
    (match Prelude.find name with
     | Some (Row row) -> builtin e (apply e) row args
     | Some Coalesce ->
       (match args with
        | [ Missing; b ] -> b
        | [ a; _ ] -> a
        | _ -> invalid_arg "coalesce takes two values")
     | _ -> invalid_arg ("Cannot apply " ^ name))
  | _ -> invalid_arg "Expected a function"

and table env (t : C.table) : frame =
  let frame =
    match t.source with
    | Read file | Load { file; _ } -> read file
    | Bound id ->
      (match List.assoc_opt id.stamp env with
       | Some (Frame f) -> f
       | _ -> invalid_arg ("Unbound table " ^ id.name))
    | Join j -> join env j
  in
  List.fold_left (fun frame (s : C.stage) -> stage env frame s.step) frame t.stages

and read (file : C.file) =
  let reader =
    match file.format with
    | Parquet -> "read_parquet"
    | Csv | Tsv -> "read_csv"
  in
  let names, rows =
    Data_file.query ("SELECT * FROM " ^ reader ^ "(" ^ Data_file.literal file.path ^ ")")
  in
  let parse typ = function
    | None -> Missing
    | Some s ->
      (match T.repr (T.present typ) with
       | Int -> Int (Int64.of_string s)
       | Float ->
         let f = float_of_string s in
         if Float.is_nan f then Missing else Float f
       | Bool -> Bool (String.lowercase_ascii s = "true")
       | _ -> Text s)
  in
  { rows = Array.length rows
  ; columns =
      List.map
        (fun (n, typ) ->
           let i = Option.get (Array.find_index (String.equal n) names) in
           n, Array.map (fun row -> parse typ row.(i)) rows)
        file.columns
  }

and join env (j : C.join) =
  let left = table env j.left
  and right = table env j.right in
  let key frame = List.assoc j.key frame.columns in
  let lk = key left
  and rk = key right in
  let matches i =
    if lk.(i) = Missing
    then []
    else
      List.filter
        (fun k -> rk.(k) <> Missing && compare_values rk.(k) lk.(i) = 0)
        (List.init right.rows Fun.id)
  in
  let pairs =
    List.concat_map
      (fun i ->
         match matches i, j.kind with
         | [], (Left | Full | Anti) -> [ Some i, None ]
         | [], (Inner | Right | Semi) -> []
         | _, Anti -> []
         | _, Semi -> [ Some i, None ]
         | ks, _ -> List.map (fun k -> Some i, Some k) ks)
      (List.init left.rows Fun.id)
  in
  let unmatched_right =
    match j.kind with
    | Right | Full ->
      List.filter_map
        (fun k ->
           if List.exists (fun (_, r) -> r = Some k) pairs
           then None
           else Some (None, Some k))
        (List.init right.rows Fun.id)
    | _ -> []
  in
  let left_unmatched = List.exists (fun (_, r) -> r = None) pairs in
  let right_unmatched = unmatched_right <> [] in
  if
    j.covered
    && (match j.kind with
        | Left -> left_unmatched
        | Right -> right_unmatched
        | _ -> left_unmatched || right_unmatched)
  then
    Diagnostic.fail
      Join_keys
      ("A join with unmatched = error found rows with no match on " ^ j.key);
  let pairs = Array.of_list (pairs @ unmatched_right) in
  let pick frame side n =
    Array.map
      (fun pair ->
         match side pair with
         | Some i -> (List.assoc n frame.columns).(i)
         | None -> Missing)
      pairs
  in
  let columns =
    List.map
      (fun (n, _) ->
         if n = j.key
         then
           ( n
           , Array.map
               (function
                 | Some i, _ -> lk.(i)
                 | None, Some k -> rk.(k)
                 | None, None -> Missing)
               pairs )
         else n, pick left fst n)
      j.left.schema
    @
    match j.kind with
    | Semi | Anti -> []
    | _ ->
      List.filter_map
        (fun (n, _) -> if n = j.key then None else Some (n, pick right snd n))
        j.right.schema
  in
  { rows = Array.length pairs; columns }

and stage env frame (step : C.step) =
  let all = Array.init frame.rows Fun.id in
  let column ?(group = all) x =
    Array.map (fun row -> eval env ~rows:{ frame; group; row } x) group
  in
  let subset indices =
    { rows = Array.length indices
    ; columns =
        List.map (fun (n, vs) -> n, Array.map (Array.get vs) indices) frame.columns
    }
  in
  let groups keys =
    let table = Hashtbl.create 16
    and order = ref [] in
    Array.iter
      (fun i ->
         let key = List.map (fun k -> (List.assoc k frame.columns).(i)) keys in
         match Hashtbl.find_opt table key with
         | Some rows -> Hashtbl.replace table key (i :: rows)
         | None ->
           order := key :: !order;
           Hashtbl.replace table key [ i ])
      all;
    List.rev_map
      (fun key -> key, Array.of_list (List.rev (Hashtbl.find table key)))
      !order
  in
  match step with
  | Filter x ->
    subset
      (Array.of_seq (Seq.filter (fun i -> (column x).(i) = Bool true) (Array.to_seq all)))
  | Mutate { name; value; by } ->
    let out = Array.make frame.rows Missing in
    let groups = if by = [] then [ [], all ] else groups by in
    List.iter
      (fun (_, group) ->
         Array.iteri (fun k i -> out.(i) <- (column ~group value).(k)) group)
      groups;
    let columns =
      if List.mem_assoc name frame.columns
      then List.map (fun (n, vs) -> n, if n = name then out else vs) frame.columns
      else frame.columns @ [ name, out ]
    in
    { frame with columns }
  | Select names ->
    { frame with columns = List.map (fun n -> n, List.assoc n frame.columns) names }
  | Rename changes ->
    { frame with
      columns =
        List.map
          (fun (n, vs) ->
             match List.find_opt (fun (_, old) -> old = n) changes with
             | Some (fresh, _) -> fresh, vs
             | None -> n, vs)
          frame.columns
    }
  | Summarize { by; measures } ->
    let groups = if by = [] then [ [], all ] else groups by in
    let keyed =
      List.mapi
        (fun k n -> n, Array.of_list (List.map (fun (key, _) -> List.nth key k) groups))
        by
    in
    let measured =
      List.map
        (fun (n, x) ->
           ( n
           , Array.of_list
               (List.map
                  (fun (_, group) ->
                     eval
                       env
                       ~rows:
                         { frame
                         ; group
                         ; row = (if Array.length group = 0 then 0 else group.(0))
                         }
                       x)
                  groups) ))
        measures
    in
    { rows = List.length groups; columns = keyed @ measured }
  | Arrange keys ->
    let keys = List.map (fun (x, d) -> column x, d) keys in
    let order = Array.copy all in
    Array.stable_sort
      (fun i j ->
         List.fold_left
           (fun c (vs, d) ->
              if c <> 0
              then c
              else (
                match vs.(i), vs.(j) with
                | Missing, Missing -> 0
                | Missing, _ -> 1
                | _, Missing -> -1
                | a, b ->
                  if d = C.Descending then compare_values b a else compare_values a b))
           0
           keys)
      order;
    subset order
  | Take n -> subset (Array.sub all 0 (min n frame.rows))
  | Distinct names ->
    let selected =
      { frame with columns = List.map (fun n -> n, List.assoc n frame.columns) names }
    in
    let seen = Hashtbl.create 16 in
    let keep =
      Array.of_seq
        (Seq.filter
           (fun i ->
              let key = List.map (fun (_, vs) -> vs.(i)) selected.columns in
              if Hashtbl.mem seen key
              then false
              else (
                Hashtbl.add seen key ();
                true))
           (Array.to_seq all))
    in
    { rows = Array.length keep
    ; columns =
        List.map (fun (n, vs) -> n, Array.map (Array.get vs) keep) selected.columns
    }
  | Assert_key names ->
    let seen = Hashtbl.create 16 in
    Array.iter
      (fun i ->
         let key = List.map (fun n -> (List.assoc n frame.columns).(i)) names in
         if List.mem Missing key
         then
           Diagnostic.fail
             Not_a_key
             (String.concat ", " names ^ " is not a key: some rows are missing it");
         if Hashtbl.mem seen key
         then
           Diagnostic.fail
             Not_a_key
             (String.concat ", " names ^ " is not a key: some rows repeat it");
         Hashtbl.add seen key ())
      all;
    frame
  | Pivot_longer { ids; columns; names_to; values_to } ->
    let ids = List.map (fun n -> n, List.assoc n frame.columns) ids in
    let width = List.length columns in
    let rows = frame.rows * width in
    let at i = i / width
    and which i = List.nth columns (i mod width) in
    { rows
    ; columns =
        List.map (fun (n, vs) -> n, Array.init rows (fun i -> vs.(at i))) ids
        @ [ names_to, Array.init rows (fun i -> Text (which i))
          ; ( values_to
            , Array.init rows (fun i -> (List.assoc (which i) frame.columns).(at i)) )
          ]
    }
  | Pivot_wider { ids; names_from; values_from; levels } ->
    let groups = groups ids in
    let names = List.assoc names_from frame.columns
    and values = List.assoc values_from frame.columns in
    let cell group level =
      match List.filter (fun i -> names.(i) = Text level) (Array.to_list group) with
      | [] -> Missing
      | [ i ] -> values.(i)
      | _ -> Diagnostic.fail Duplicate_pivot_cell "Two rows produce the same pivot cell"
    in
    Array.iter
      (fun i ->
         match names.(i) with
         | Text l when List.mem l levels -> ()
         | _ -> Diagnostic.fail Pivot_shape "A pivot key is not one of the levels")
      all;
    { rows = List.length groups
    ; columns =
        List.mapi
          (fun k n -> n, Array.of_list (List.map (fun (key, _) -> List.nth key k) groups))
          ids
        @ List.map
            (fun l -> l, Array.of_list (List.map (fun (_, g) -> cell g l) groups))
            levels
    }
;;

let run env items =
  List.fold_left
    (fun (env, shown) -> function
       | C.Define (id, e) -> (id.stamp, eval env e) :: env, shown
       | Variant _ -> env, shown
       | Show e -> env, shown @ [ e, eval env e ]
       | Write (e, _) -> env, shown @ [ e, eval env e ])
    (env, [])
    items
;;
