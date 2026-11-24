open Syntax

let helpers = {|import math

def rp_div(a, b):
    if b != 0.0:
        return a / b
    if a == 0.0 or math.isnan(a):
        return math.nan
    return math.copysign(math.inf, math.copysign(1.0, a) * math.copysign(1.0, b))

def rp_int(x):
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
    | Literal "None" -> "None"
    | Literal "true" -> "True" | Literal "false" -> "False"
    | Annotated (v,_) -> expr v
    | Tuple xs -> "(" ^ String.concat ", " (List.map expr xs) ^ ")"
    | List xs -> "[" ^ String.concat ", " (List.map expr xs) ^ "]"
    | Record fs -> "{" ^ String.concat ", " (List.map (fun (n,v) -> Polars.quoted n ^ ": " ^ expr v) fs) ^ "}"
    | Field (v,n) -> "(" ^ expr v ^ ")[" ^ Polars.quoted n ^ "]"
    | Local (n,v,body) ->
        let id = fresh () in apply ("(lambda " ^ id ^ ": " ^ emit ((n,id)::env) body ^ ")") [expr v]
    | Call ({form=Name (Bare,"Some");_},[None,v]) -> "('Some', " ^ expr v ^ ")"
    | Match (subject,branches) ->
        let id = fresh () in
        let rec pattern path = function
          | Precord ns -> [],List.map (fun n -> n,path ^ "[" ^ Polars.quoted n ^ "]") ns
          | Wildcard -> [],[] | Bind n -> [],[n,path]
          | Pliteral s -> [path ^ " == " ^ expr {x with form=Literal s}],[]
          | Tag (n,ps) ->
              let tests,bindings = children path 1 ps in
              (("isinstance(" ^ path ^ ", tuple)") :: (path ^ "[0] == " ^ Polars.quoted n) :: tests),bindings
          | Ptuple ps -> children path 0 ps
        and children path offset ps =
          let tests,bindings = List.mapi (fun i p -> pattern (Printf.sprintf "%s[%d]" path (i+offset)) p) ps |> List.split in
          List.concat tests,List.concat bindings in
        let branch (p,body) rest =
          let tests,bindings = pattern id p in
          let value = emit (bindings @ env) body in
          match tests with [] -> value | _ -> "(" ^ value ^ " if " ^ String.concat " and " tests ^ " else " ^ rest ^ ")" in
        apply ("(lambda " ^ id ^ ": " ^ List.fold_right branch branches "None" ^ ")") [expr subject]
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
    | Binary ("/",a,b) -> apply "rp_div" [expr a;expr b]
    | Unary ("-",v) -> "rp_num(-" ^ expr v ^ ")"
    | Unary ("not",v) -> "(not " ^ expr v ^ ")"
    | Binary ((">" | "<" | ">=" | "<=" | "==" | "!=" | "and" | "or" as op), a, b) ->
        "(" ^ expr a ^ " " ^ op ^ " " ^ expr b ^ ")"
    | _ -> error x "Expression has no supported Python lowering" in
  let _, _, lines = List.fold_left (fun (types, env, lines) d ->
    let types, _ = Infer.declaration types d in
    match d with
    | Let (n, ps, body) ->
        let x = match ps with None -> body | Some ps -> {body with form = Lambda (ps, body)} in
        let id = fresh () in types, (n,id)::env, (id ^ " = " ^ emit env x)::lines
    | Evaluate x -> types, env, ("print(" ^ emit env x ^ ")")::lines
    | Alias _ -> types,env,lines
    | Type (_,_,constructors) ->
        List.fold_left (fun (types,env,lines) (n,fs) ->
          let id = fresh () and ps = List.map (fun _ -> fresh ()) fs in
          let tuple = "(" ^ Polars.quoted n ^ ", " ^ String.concat ", " ps ^ ")" in
          let value = if ps=[] then tuple else "lambda " ^ String.concat ", " ps ^ ": " ^ tuple in
          types,(n,id)::env,(id ^ " = " ^ value)::lines) (types,env,lines) constructors
    | Import (n,_) -> failwith ("Module " ^ n ^ " requires a table backend")
  | Contract _ -> failwith "Contract lowering is unavailable") ([], [], []) program in
  String.concat "\n" (helpers :: List.rev lines) ^ "\n"
