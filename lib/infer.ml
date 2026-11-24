open Syntax

type constraint_ = Any | Numeric
type typ = Int | Float | Bool | String | Arrow of typ list * typ | Var of cell ref
and cell = Free of int * constraint_ | Link of typ
type scheme = cell ref list * typ
exception Error of span * string

let fresh level c = Var (ref (Free (level, c)))
let rec repr = function Var { contents = Link t } -> repr t | t -> t

let rec variables f t = match repr t with
  | Var cell -> f cell
  | Arrow (args, result) -> List.iter (variables f) (result :: args)
  | _ -> ()

let unify span =
  let fail () = raise (Error (span, "Incompatible types")) in
  let rec equal x y = match repr x, repr y with
    | Var a, Var b when a == b -> ()
    | Var ({ contents = Free (level, c) } as a), t
    | t, Var ({ contents = Free (level, c) } as a) ->
        (match c, t with Numeric, (Bool | String | Arrow _) -> fail () | _ -> ());
        variables (fun b -> match !b with
          | Free (n, k) ->
              if a == b then raise (Error (span, "Infinite type"));
              let k = match t with Var _ when c = Numeric -> Numeric | _ -> k in
              b := Free (min n level, k)
          | Link _ -> assert false) t;
        a := Link t
    | Arrow (xs, x), Arrow (ys, y) when List.length xs = List.length ys ->
        List.iter2 equal xs ys; equal x y
    | Int, Int | Float, Float | Bool, Bool | String, String -> ()
    | _ -> fail () in
  equal

let generalize level t =
  let vars = ref [] in
  variables (fun v -> match !v with
    | Free (n, _) when n > level && not (List.exists ((==) v) !vars) -> vars := v :: !vars
    | _ -> ()) t;
  !vars, t

let instantiate level (vars, t) =
  let replacements = List.map (fun v -> match !v with
    | Free (_, c) -> v, fresh level c | _ -> assert false) vars in
  let rec copy t = match repr t with
    | Var v -> (match List.find_opt (fun (k, _) -> v == k) replacements with
      | Some (_, new_t) -> new_t | None -> t)
    | Arrow (ps, r) -> Arrow (List.map copy ps, copy r)
    | t -> t in
  copy t

let rec expression env level source =
  let check = expression env level and equal = unify source.span in
  let fail m = raise (Error (source.span, m)) in
  let call f args =
    let r = fresh level Any in equal f (Arrow (args, r)); r in
  match source.form with
  | Name (Bare, n) -> (match List.assoc_opt n env with
    | Some s -> instantiate level s | None -> fail ("Unknown name: " ^ n))
  | Literal ("true" | "false") -> Bool
  | Literal "None" -> fail "None requires an optional type"
  | Literal s when s.[0] = '"' -> String
  | Literal s -> if String.exists (function '.' | 'e' | 'E' -> true | _ -> false) s then Float else Int
  | Lambda (ps, body) ->
      let ts = List.map (fun _ -> fresh level Any) ps in
      let locals = List.map2 (fun p t -> p, ([], t)) ps ts in
      Arrow (ts, expression (locals @ env) level body)
  | Call (f, args) -> call (check f) (List.map (fun (_, x) -> check x) args)
  | Binary ("|>", x, f) ->
      (match f.form with
       | Call (g, args) -> call (check g) (check x :: List.map (fun (_, a) -> check a) args)
       | _ -> call (check f) [check x])
  | Binary (op, a, b) ->
      let t = fresh level Numeric in equal t (check a); equal t (check b);
      (match op with ">" -> Bool | "+" | "-" | "*" -> t | "/" -> equal t Float; Float
       | _ -> fail ("Unsupported operator: " ^ op))
  | If (c, a, b) -> equal (check c) Bool; let t = check a in equal t (check b); t
  | _ -> fail "Unsupported scalar expression"

let declaration env = function
  | Let (name, ps, body) ->
      let x = match ps with None -> body | Some ps -> { body with form = Lambda (ps, body) } in
      let t = expression env 1 x in (name, generalize 0 t) :: env, t
  | Evaluate x -> env, expression env 1 x
  | Contract _ -> failwith "Contract loading is unavailable"

let display t =
  let vars = ref [] in
  let rec show t = match repr t with
    | Int -> "Int64" | Float -> "Float64" | Bool -> "Bool" | String -> "String"
    | Arrow (ps, r) -> "(" ^ String.concat ", " (List.map show ps) ^ ") -> " ^ show r
    | Var ({ contents = Free (_, c) } as v) ->
        (match List.find_opt (fun (k, _) -> k == v) !vars with
         | Some (_, s) -> s
         | None ->
             let prefix = match c with Numeric -> "num" | Any -> "a" in
             let s = "'" ^ prefix ^ string_of_int (List.length !vars) in
             vars := (v, s) :: !vars; s)
    | Var _ -> assert false in
  show t
