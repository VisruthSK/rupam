open Syntax

type value =
  | Int of int64 | Float of float | Bool of bool | Text of string
  | Closure of string list * expr * (string * value) list

exception Error of span * string

let display = function
  | Int n -> Int64.to_string n | Float n -> string_of_float n
  | Bool b -> string_of_bool b | Text s -> Printf.sprintf "%S" s
  | Closure _ -> "<function>"

let rec expression env source =
  let eval = expression env and fail message = raise (Error (source.span, message)) in
  let apply fn arguments = match fn with
    | Closure (params, body, captured) when List.length params = List.length arguments ->
        expression (List.combine params arguments @ captured) body
    | Closure _ -> fail "Wrong argument count" | _ -> fail "Expected function" in
  let args = List.map (function None, x -> eval x | _ -> fail "Expected positional argument") in
  match source.form with
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
    | Contract _ -> raise (Failure "Contract loading is unavailable")) (env, None) declarations
