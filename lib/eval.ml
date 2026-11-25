open Syntax

type value =
  | Int of int64
  | Float of float
  | Bool of bool
  | Text of string
  | Tuple of value list
  | List of value list
  | Record of (string * value) list
  | Variant of string * value list
  | Constructor of string * int
  | Option of value option
  | Closure of string list * expr * (string * value) list

exception Error of span * string

let rec display = function
  | Int n -> Int64.to_string n
  | Float n -> string_of_float n
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
  | Text s -> Printf.sprintf "%S" s
  | Constructor (n, _) -> n
  | Variant (n, []) -> n
  | Variant (n, vs) -> n ^ "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | Tuple vs -> "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | List vs -> "[" ^ String.concat ", " (List.map display vs) ^ "]"
  | Record fs ->
    "{" ^ String.concat ", " (List.map (fun (n, v) -> n ^ " = " ^ display v) fs) ^ "}"
  | Option None -> "None"
  | Option (Some v) -> "Some(" ^ display v ^ ")"
  | Closure _ -> "<function>"
;;

let rec expression env source =
  let eval = expression env
  and fail message = raise (Error (source.span, message)) in
  let apply fn arguments =
    match fn with
    | Constructor (n, k) when k = List.length arguments -> Variant (n, arguments)
    | Closure (params, body, captured) when List.length params = List.length arguments ->
      expression (List.combine params arguments @ captured) body
    | Closure _ -> fail "Wrong argument count"
    | _ -> fail "Expected function"
  in
  let args =
    List.map (function
      | None, x -> eval x
      | _ -> fail "Expected positional argument")
  in
  match source.form with
  | Annotated (x, _) -> eval x
  | Match (x, branches) ->
    let value = eval x in
    let rec bind p v =
      match p, v with
      | Precord ns, Record fs -> Some (List.map (fun n -> n, List.assoc n fs) ns)
      | Wildcard, _ -> Some []
      | Bind n, _ -> Some [ n, v ]
      | Pliteral l, v when v = eval { source with form = Literal l } -> Some []
      | Prange { low; high; inclusive }, v ->
        let bound l = eval { source with form = Literal l } in
        let number = function
          | Int i -> Int64.to_float i
          | Float f -> f
          | _ -> nan
        in
        let x = number v in
        let above = Option.fold ~none:true ~some:(fun l -> x >= number (bound l)) low in
        let below =
          Option.fold
            ~none:true
            ~some:(fun h ->
              let h = number (bound h) in
              if inclusive then x <= h else x < h)
            high
        in
        if above && below then Some [] else None
      | Palt ps, v -> List.find_map (fun p -> bind p v) ps
      | Pas (n, p), v -> Option.map (fun vars -> (n, v) :: vars) (bind p v)
      | Tag ("None", []), Option None -> Some []
      | Tag ("Some", [ p ]), Option (Some x) -> bind p x
      | Ptuple ps, Tuple vs -> many ps vs
      | Tag (n, ps), Variant (m, vs) when n = m -> many ps vs
      | _ -> None
    and many ps vs =
      match ps, vs with
      | [], [] -> Some []
      | p :: ps, v :: vs ->
        Option.bind (bind p v) (fun a -> Option.map (( @ ) a) (many ps vs))
      | _ -> None
    in
    (match
       List.find_map
         (fun arm ->
            Option.bind (bind arm.pattern value) (fun vars ->
              match arm.guard with
              | None -> Some (vars, arm.body)
              | Some g ->
                (match expression (vars @ env) g with
                 | Bool true -> Some (vars, arm.body)
                 | _ -> None)))
         branches
     with
     | Some (vars, body) -> expression (vars @ env) body
     | None -> fail "Non-exhaustive match")
  | Tuple xs -> Tuple (List.map eval xs)
  | List xs -> List (List.map eval xs)
  | Record fs -> Record (List.map (fun (n, x) -> n, eval x) fs)
  | Field (x, n) ->
    (match eval x with
     | Record fs -> List.assoc n fs
     | _ -> fail "Expected record")
  | Local (n, x, y) -> expression ((n, eval x) :: env) y
  | Call ({ form = Name (Bare, "Some"); _ }, [ (None, x) ]) -> Option (Some (eval x))
  | Literal Absent -> Option None
  | Literal (Syntax.Bool b) -> Bool b
  | Literal (Syntax.Text s) -> Text s
  | Literal (Syntax.Float s) ->
    (match float_of_string_opt s with
     | Some f -> Float f
     | None -> fail "Expected numeric literal")
  | Literal (Syntax.Int s) ->
    (match Int64.of_string_opt s with
     | Some n -> Int n
     | None -> fail "Expected Int64 literal")
  | Name (Bare, name) ->
    (match List.assoc_opt name env with
     | Some v -> v
     | None -> fail ("Unknown name: " ^ name))
  | Lambda (params, body) -> Closure (params, body, env)
  | If (c, yes, no) ->
    (match eval c with
     | Bool b -> eval (if b then yes else no)
     | _ -> fail "Expected Bool")
  | Call (f, xs) -> apply (eval f) (args xs)
  | Pipe (x, f) ->
    let v = eval x in
    (match f.form with
     | Call (g, xs) -> apply (eval g) (v :: args xs)
     | _ -> apply (eval f) [ v ])
  | Unary (Not, x) ->
    (match eval x with
     | Bool b -> Bool (not b)
     | _ -> fail "Expected Bool")
  | Unary (Negate, x) ->
    (match eval x with
     | Int n ->
       (try Int (Integer.sub 0L n) with
        | Integer.Overflow -> fail "Int64 overflow")
     | Float f -> Float (-.f)
     | _ -> fail "Expected number")
  | Binary (And, a, b) ->
    (match eval a with
     | Bool true -> eval b
     | Bool false -> Bool false
     | _ -> fail "Expected Bool")
  | Binary (Or, a, b) ->
    (match eval a with
     | Bool false -> eval b
     | Bool true -> Bool true
     | _ -> fail "Expected Bool")
  | Binary (op, a, b) ->
    let x = eval a in
    (try
       match op, x, eval b with
       | Add, Int a, Int b -> Int (Integer.add a b)
       | Sub, Int a, Int b -> Int (Integer.sub a b)
       | Mul, Int a, Int b -> Int (Integer.mul a b)
       | Add, Float a, Float b -> Float (a +. b)
       | Sub, Float a, Float b -> Float (a -. b)
       | Mul, Float a, Float b -> Float (a *. b)
       | Power, Float a, Float b -> Float (Float.pow a b)
       | Power, Int a, Int b when b >= 0L ->
         let rec go acc n =
           if n = 0L then acc else go (Integer.mul acc a) (Int64.pred n)
         in
         Int (go 1L b)
       | Div, Float a, Float b -> Float (a /. b)
       | Less, Int a, Int b -> Bool (a < b)
       | Less, Float a, Float b -> Bool (a < b)
       | Less_equal, Int a, Int b -> Bool (a <= b)
       | Less_equal, Float a, Float b -> Bool (a <= b)
       | Greater_equal, Int a, Int b -> Bool (a >= b)
       | Greater_equal, Float a, Float b -> Bool (a >= b)
       | Equal, Int a, Int b -> Bool (a = b)
       | Equal, Float a, Float b -> Bool (a = b)
       | Not_equal, Int a, Int b -> Bool (a <> b)
       | Not_equal, Float a, Float b -> Bool (a <> b)
       | Greater, Int a, Int b -> Bool (a > b)
       | Greater, Float a, Float b -> Bool (a > b)
       | _ -> fail ("Unsupported operands for " ^ binary_symbol op)
     with
     | Integer.Overflow -> fail "Int64 overflow")
  | _ -> fail "Unsupported expression in scalar evaluator"
;;

let run env declarations =
  List.fold_left
    (fun (env, _) -> function
       | Let (name, params, body) ->
         let value =
           match params with
           | Some ps -> Closure (ps, body, env)
           | None -> expression env body
         in
         (name, value) :: env, Some value
       | Evaluate body -> env, Some (expression env body)
       | Alias _ -> env, None
       | Type (_, _, vs) ->
         ( List.fold_left
             (fun env (n, fields) ->
                ( n
                , if fields = []
                  then Variant (n, [])
                  else Constructor (n, List.length fields) )
                :: env)
             env
             vs
         , None )
       | Import (n, _) -> failwith ("Module " ^ n ^ " requires a table backend")
       | Contract _ -> raise (Failure "Contract loading is unavailable")
       | Setting _ -> env, None)
    (env, None)
    declarations
;;
