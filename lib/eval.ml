open Syntax

type value =
  | Int of int64 | Float of float | Bool of bool | Text of string
  | Tuple of value list | List of value list | Record of (string * value) list
  | Variant of string * value list | Constructor of string * int
  | Option of value option
  | Closure of string list * expr * (string * value) list

exception Error of span * string

let rec display = function
  | Int n -> Int64.to_string n | Float n -> string_of_float n
  | Bool b -> string_of_bool b | Text s -> Printf.sprintf "%S" s
  | Constructor (n,_) -> n | Variant (n,[]) -> n
  | Variant (n,vs) -> n ^ "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | Tuple vs -> "(" ^ String.concat ", " (List.map display vs) ^ ")"
  | List vs -> "[" ^ String.concat ", " (List.map display vs) ^ "]"
  | Record fs -> "{" ^ String.concat ", " (List.map (fun (n,v) -> n ^ " = " ^ display v) fs) ^ "}"
  | Option None -> "None" | Option (Some v) -> "Some(" ^ display v ^ ")"
  | Closure _ -> "<function>"

let rec expression env source =
  let eval = expression env and fail message = raise (Error (source.span, message)) in
  let apply fn arguments = match fn with
    | Constructor (n,k) when k=List.length arguments -> Variant (n,arguments)
    | Closure (params, body, captured) when List.length params = List.length arguments ->
        expression (List.combine params arguments @ captured) body
    | Closure _ -> fail "Wrong argument count" | _ -> fail "Expected function" in
  let args = List.map (function None, x -> eval x | _ -> fail "Expected positional argument") in
  match source.form with
  | Annotated (x,_) -> eval x
  | Match (x,branches) ->
      let value = eval x in
      let rec bind p v = match p,v with
        | Precord ns,Record fs -> Some (List.map (fun n -> n,List.assoc n fs) ns)
        | Wildcard,_ -> Some [] | Bind n,_ -> Some [n,v]
        | Pliteral s,v when v=eval {source with form=Literal s} -> Some []
        | Tag ("Some",[p]),Option (Some x) -> bind p x
        | Ptuple ps,Tuple vs -> many ps vs
        | Tag (n,ps),Variant (m,vs) when n=m -> many ps vs
        | _ -> None
      and many ps vs = match ps,vs with
        | [],[] -> Some []
        | p::ps,v::vs -> Option.bind (bind p v) (fun a -> Option.map ((@) a) (many ps vs))
        | _ -> None in
      (match List.find_map (fun (p,body) -> Option.map (fun vars -> vars,body) (bind p value)) branches with
       | Some (vars,body) -> expression (vars @ env) body | None -> fail "Non-exhaustive match")
  | Tuple xs -> Tuple (List.map eval xs)
  | List xs -> List (List.map eval xs)
  | Record fs -> Record (List.map (fun (n,x) -> n,eval x) fs)
  | Field (x,n) -> (match eval x with Record fs -> List.assoc n fs | _ -> fail "Expected record")
  | Local (n,x,y) -> expression ((n,eval x)::env) y
  | Literal "None" -> Option None
  | Call ({form=Name (Bare,"Some");_},[None,x]) -> Option (Some (eval x))
  | Literal "true" -> Bool true | Literal "false" -> Bool false
  | Literal s when s.[0] = '"' -> Text (Scanf.unescaped (String.sub s 1 (String.length s - 2)))
  | Literal s when String.contains s '.' || String.contains s 'e' || String.contains s 'E' ->
      (try Float (float_of_string s) with Failure _ -> fail "Expected numeric literal")
  | Literal s -> (try Int (Int64.of_string s) with Failure _ -> fail "Expected Int64 literal")
  | Name (Bare, name) -> (match List.assoc_opt name env with Some v -> v | None -> fail ("Unknown name: " ^ name))
  | Lambda (params, body) -> Closure (params, body, env)
  | If (c, yes, no) -> (match eval c with Bool b -> eval (if b then yes else no) | _ -> fail "Expected Bool")
  | Call (f, xs) -> apply (eval f) (args xs)
  | Binary ("|>", x, f) ->
      let v = eval x in
      (match f.form with Call (g, xs) -> apply (eval g) (v :: args xs) | _ -> apply (eval f) [v])
  | Unary ("not",x) -> (match eval x with Bool b -> Bool (not b) | _ -> fail "Expected Bool")
  | Unary ("-",x) -> (match eval x with Int n -> (try Int (Integer.sub 0L n) with Integer.Overflow -> fail "Int64 overflow")
      | Float f -> Float (-.f) | _ -> fail "Expected number")
  | Binary ("and",a,b) -> (match eval a with Bool true -> eval b | Bool false -> Bool false | _ -> fail "Expected Bool")
  | Binary ("or",a,b) -> (match eval a with Bool false -> eval b | Bool true -> Bool true | _ -> fail "Expected Bool")
  | Binary (op, a, b) ->
      let x = eval a in
      (try match op, x, eval b with
       | "+", Int a, Int b -> Int (Integer.add a b)
       | "-", Int a, Int b -> Int (Integer.sub a b)
       | "*", Int a, Int b -> Int (Integer.mul a b)
       | "+", Float a, Float b -> Float (a +. b)
       | "-", Float a, Float b -> Float (a -. b)
       | "*", Float a, Float b -> Float (a *. b)
       | "/", Float a, Float b -> Float (a /. b)
       | "<", Int a, Int b -> Bool (a < b) | "<", Float a, Float b -> Bool (a < b)
       | "<=", Int a, Int b -> Bool (a <= b) | "<=", Float a, Float b -> Bool (a <= b)
       | ">=", Int a, Int b -> Bool (a >= b) | ">=", Float a, Float b -> Bool (a >= b)
       | "==", Int a, Int b -> Bool (a = b) | "==", Float a, Float b -> Bool (a = b)
       | "!=", Int a, Int b -> Bool (a <> b) | "!=", Float a, Float b -> Bool (a <> b)
       | ">", Int a, Int b -> Bool (a > b)
       | ">", Float a, Float b -> Bool (a > b)
       | _ -> fail ("Unsupported operands for " ^ op)
       with Integer.Overflow -> fail "Int64 overflow")
  | _ -> fail "Unsupported expression in scalar evaluator"

let run env declarations =
  List.fold_left (fun (env, _) -> function
    | Let (name, params, body) ->
        let value = match params with Some ps -> Closure (ps, body, env) | None -> expression env body in
        (name, value) :: env, Some value
    | Evaluate body -> env, Some (expression env body)
    | Alias _ -> env,None
    | Type (_,_,vs) -> List.fold_left (fun env (n,fields) ->
        (n,(if fields=[] then Variant (n,[]) else Constructor (n,List.length fields)))::env) env vs,None
    | Import (n,_) -> failwith ("Module " ^ n ^ " requires a table backend")
  | Contract _ -> raise (Failure "Contract loading is unavailable")) (env, None) declarations
