open Syntax
open Shape

type binding =
  | Module of string
  | Value of Shape.expr
  | Function of string list * Syntax.expr * env

and env = (string * binding) list

let distance a b =
  let row = Array.init (String.length b + 1) Fun.id in
  String.iteri
    (fun i x ->
       let diagonal = ref row.(0) in
       row.(0) <- i + 1;
       String.iteri
         (fun j y ->
            let old = row.(j + 1) in
            row.(j + 1) <- min (!diagonal + Bool.to_int (x <> y)) (1 + min row.(j) old);
            diagonal := old)
         b)
    a;
  row.(String.length b)
;;

let lookup name bindings =
  match List.assoc_opt name bindings with
  | Some value -> value
  | None ->
    let hint =
      List.map (fun (n, _) -> distance n name, n) bindings |> List.sort compare
    in
    let message =
      match hint with
      | (d, n) :: _ when d <= 2 -> ". Did you mean " ^ n ^ "?"
      | _ -> ""
    in
    raise (Error ("Unknown name: " ^ name ^ message))
;;

let rec expression env columns source =
  let check = expression env columns in
  match source.form with
  | Literal "true" -> constant (`Bool true)
  | Literal "false" -> constant (`Bool false)
  | Literal s
    when String.exists
           (function
             | '.' | 'e' | 'E' -> true
             | _ -> false)
           s ->
    (match float_of_string_opt s with
     | Some n -> constant (`Float n)
     | None -> raise (Error "Expected a number"))
  | Literal s ->
    (match Int64.of_string_opt s with
     | Some n -> constant (`Int n)
     | None -> raise (Error "Expected Int64 literal"))
  | Name (Column, name) -> lookup name columns
  | Name (Bare, name) when List.mem_assoc name columns -> lookup name columns
  | Name (_, name) ->
    (match lookup name env with
     | Value x -> x
     | Function _ | Module _ -> raise (Error "Expected a value"))
  | If (c, t, f) -> choose (check c) (check t) (check f)
  | Binary (op, a, b) ->
    let operator =
      match op with
      | "+" -> Add
      | "*" -> Mul
      | "-" -> Sub
      | "/" -> Div
      | ">" -> Greater
      | _ -> raise (Error ("Unsupported operator: " ^ op))
    in
    binary operator (check a) (check b)
  | Call ({ form = Field ({ form = Name (Bare, ns); _ }, "sqrt"); _ }, [ (None, arg) ]) ->
    (match List.assoc_opt ns env with
     | Some (Module "polars") -> ()
     | _ ->
       raise (Error "polars.sqrt requires explicit import polars and the Python target"));
    let v = check arg in
    if v.typ.primitive <> Float64 then raise (Error "polars.sqrt requires Float64");
    { v with node = Polars_sqrt v }
  | Call ({ form = Name (Bare, name); _ }, args) ->
    let values =
      List.map
        (function
          | None, x -> check x
          | Some _, _ -> raise (Error "Expected positional argument"))
        args
    in
    (match List.assoc_opt name env, values with
     | Some (Function (params, body, closure)), _ ->
       if List.length params <> List.length values
       then raise (Error "Wrong argument count");
       let locals = List.map2 (fun p v -> p, Value v) params values in
       expression (locals @ closure) [] body
     | None, [ x; fallback ] when name = "coalesce" -> coalesce x fallback
     | None, [ x ] when name = "abs" -> abs x
     | None, [ x ] when name = "sum" -> reduce Sum x
     | None, [ x ] when name = "mean" -> reduce Mean x
     | None, [ x ] when name = "sd" -> reduce Sd x
     | _ -> raise (Error ("Invalid call to " ^ name)))
  | _ -> raise (Error "Unsupported value expression")
;;

let define env = function
  | Let (name, Some params, body) -> (name, Function (params, body, env)) :: env
  | Let (name, None, body) -> (name, Value (expression env [] body)) :: env
  | _ -> raise (Error "Expected a let declaration")
;;

let derive env columns bindings =
  List.fold_left
    (fun schema (label, source) ->
       match label with
       | None -> raise (Error "derive requires named columns")
       | Some name when List.mem_assoc name schema ->
         raise (Error "Column exists. Use replace.")
       | Some name -> schema @ [ name, expression env schema source ])
    columns
    bindings
;;
