open Shape

type value = I of int64 | F of float | B of bool | S of string | Missing
type result = One of value | Many of value array

let map f = function One v -> One (f v) | Many vs -> Many (Array.map f vs)

let combine f x y = match x, y with
  | One a, One b -> One (f a b)
  | Many a, Many b when Array.length a = Array.length b -> Many (Array.map2 f a b)
  | _ -> raise (Error "Misaligned inputs in elaborated expression")

let arithmetic op a b = match a, b with
  | Missing, _ | _, Missing -> Missing
  | I a, I b ->
      (match op with
       | Add -> I (Integer.add a b) | Sub -> I (Integer.sub a b)
       | Mul -> I (Integer.mul a b) | Greater -> B (a > b)
       | Div -> raise (Error "Expected checked conversion to Float64"))
  | F a, F b ->
      (match op with
       | Add -> F (a +. b) | Sub -> F (a -. b) | Mul -> F (a *. b)
       | Div -> F (a /. b) | Greater -> B (a > b))
  | _ -> raise (Error "Unexpected operands in numeric expression")

let aggregate reducer values =
  let count = ref 0 and sum = ref 0. and mean = ref 0. and m2 = ref 0. in
  Array.iter (function
    | Missing -> ()
    | F x ->
        incr count;
        sum := !sum +. x;
        let delta = x -. !mean in
        mean := !mean +. delta /. float_of_int !count;
        m2 := !m2 +. delta *. (x -. !mean)
    | _ -> raise (Error "Expected a Float64 column")) values;
  match reducer, !count with
  | _, 0 | Sd, 1 -> Missing
  | Sum, _ -> F !sum | Mean, _ -> F (!sum /. float_of_int !count)
  | Sd, n -> F (sqrt (!m2 /. float_of_int (n - 1)))

let evaluate ~rows ~input expression =
  let rec eval expr = match expr.node with
    | Constant (`Bool b) -> One (B b)
    | Constant (`Float f) -> One (F f)
    | Constant (`Int n) -> One (I n)
    | Input id -> input id
    | Broadcast (_, e) ->
        (match eval e with One v -> Many (Array.make rows v) | Many _ -> raise (Error "Expected scalar"))
    | Binary (op, a, b) -> combine (arithmetic op) (eval a) (eval b)
    | Choose (c,t,f) ->
        let branch = function B true -> t | B false -> f | _ -> raise (Error "Expected Bool") in
        (match eval c with One v -> eval (branch v)
         | Many vs ->
             let rec row i x = match x.node with
               | Broadcast (_,v) -> (match eval v with One v -> v | _ -> assert false)
               | Input id -> (match input id with Many xs -> xs.(i) | One v -> v)
               | Binary (op,a,b) -> arithmetic op (row i a) (row i b)
               | Coalesce (a,b) -> (match row i a with Missing -> row i b | v -> v)
               | Choose (c,t,f) -> (match row i c with B true -> row i t | B false -> row i f | _ -> assert false)
               | Polars_sqrt a -> (match row i a with Missing -> Missing | F f -> F (sqrt f) | _ -> assert false)
               | Abs a -> (match row i a with Missing -> Missing | F v -> F (Float.abs v)
                   | I v -> I (if v<0L then Integer.sub 0L v else v) | _ -> assert false)
               | Reduce _ | Constant _ -> (match eval x with One v -> v | _ -> assert false) in
             Many (Array.mapi (fun i v -> row i (branch v)) vs))
    | Coalesce (a, b) -> combine (fun x y -> if x = Missing then y else x) (eval a) (eval b)
    | Polars_sqrt e -> map (function Missing -> Missing | F f -> F (sqrt f) | _ -> assert false) (eval e)
    | Abs e -> map (function
        | Missing -> Missing | F x -> F (Float.abs x)
        | I x -> I (if x < 0L then Integer.sub 0L x else x)
        | B _ | S _ -> raise (Error "Expected number")) (eval e)
    | Reduce (r, e) -> (match eval e with Many vs -> One (aggregate r vs)
        | One _ -> raise (Error "Expected column")) in
  eval expression
