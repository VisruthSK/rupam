open Syntax

let helpers = {|def rp_int(x):
    if x < -(1 << 63) or x >= (1 << 63):
        raise OverflowError("Int64 overflow")
    return x

def rp_num(x):
    return rp_int(x) if type(x) is int else x
|}

let compile program =
  let serial = ref 0 in
  let fresh () = incr serial; "v" ^ string_of_int !serial in
  let error x msg = raise (Infer.Error (x.span, msg)) in
  let rec emit env x =
    let expr = emit env in
    let apply f xs = f ^ "(" ^ String.concat ", " xs ^ ")" in
    let args xs = List.map (function None, a -> expr a | _ -> error x "Expected positional arguments") xs in
    match x.form with
    | Name (Bare, name) -> List.assoc name env
    | Literal "true" -> "True" | Literal "false" -> "False"
    | Literal s when s.[0] = '"' ->
        let text = Scanf.unescaped (String.sub s 1 (String.length s - 2)) in
        let codes = List.init (String.length text) (fun i -> string_of_int (Char.code text.[i])) in
        "bytes([" ^ String.concat ", " codes ^ "]).decode('utf-8')"
    | Literal s when String.exists (function '.' | 'e' | 'E' -> true | _ -> false) s -> "float('" ^ s ^ "')"
    | Literal s -> "rp_int(int('" ^ s ^ "'))"
    | Call (f, xs) -> apply (expr f) (args xs)
    | Lambda (ps, body) ->
        let names = List.map (fun p -> p, fresh ()) ps in
        "(lambda " ^ String.concat ", " (List.map snd names) ^ ": " ^ emit (names @ env) body ^ ")"
    | If (c, a, b) -> "(" ^ expr a ^ " if " ^ expr c ^ " else " ^ expr b ^ ")"
    | Binary ("|>", a, f) ->
        let v = fresh () in
        let body = match f.form with Call (g, xs) -> apply (expr g) (v :: args xs) | _ -> apply (expr f) [v] in
        "(lambda " ^ v ^ ": " ^ body ^ ")(" ^ expr a ^ ")"
    | Binary (("+" | "-" | "*" as op), a, b) -> "rp_num(" ^ expr a ^ " " ^ op ^ " " ^ expr b ^ ")"
    | Binary (">", a, b) -> "(" ^ expr a ^ " > " ^ expr b ^ ")"
    | _ -> error x "Expression has no supported Python lowering" in
  let _, _, lines = List.fold_left (fun (types, env, lines) d ->
    let types, _ = Infer.declaration types d in
    match d with
    | Let (n, ps, body) ->
        let x = match ps with None -> body | Some ps -> {body with form = Lambda (ps, body)} in
        let id = fresh () in types, (n,id)::env, (id ^ " = " ^ emit env x)::lines
    | Evaluate x -> types, env, ("print(" ^ emit env x ^ ")")::lines
    | Contract _ -> failwith "Contract lowering is unavailable") ([], [], []) program in
  String.concat "\n" (helpers :: List.rev lines) ^ "\n"
