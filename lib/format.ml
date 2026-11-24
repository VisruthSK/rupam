open Syntax

let comma f xs = String.concat ", " (List.map f xs)

let rec annotation = function
  | Atuple ts -> "(" ^ comma annotation ts ^ ")"
  | Aarrow (a,b) -> "(" ^ annotation a ^ " -> " ^ annotation b ^ ")"
  | Arow (fs,tail) -> "{" ^ String.concat ", " (List.map (fun (n,t) -> n ^ ": " ^ annotation t) fs @
      Option.to_list (Option.map (fun n -> ".." ^ n) tail)) ^ "}"
  | Named (n,[]) -> n | Named (n,ts) -> n ^ "<" ^ comma annotation ts ^ ">"
  | Optional t -> annotation t ^ "?"

let rec pattern = function
  | Precord ns -> "{" ^ String.concat ", " ns ^ "}"
  | Wildcard -> "_" | Bind n | Pliteral n | Tag (n,[]) -> n
  | Tag (n,ps) -> n ^ "(" ^ comma pattern ps ^ ")" | Ptuple ps -> "(" ^ comma pattern ps ^ ")"

let rec expr x = match x.form with
  | Name (scope, name) ->
      (match scope with Bare -> "" | Column -> "$" | Lexical -> "^") ^ Identifier.quote name
  | Match (x,bs) -> "match " ^ expr x ^ " {" ^ comma (fun (p,x) -> pattern p ^ " => " ^ expr x) bs ^ "}"
  | Annotated (x,_) -> expr x
  | Tuple xs -> "(" ^ comma expr xs ^ ")"
  | List xs -> "[" ^ comma expr xs ^ "]"
  | Record fields -> "{" ^ comma (fun (n,x) -> Identifier.quote n ^ " = " ^ expr x) fields ^ "}"
  | Local (n,x,y) -> "(let " ^ n ^ " = " ^ expr x ^ " in " ^ expr y ^ ")"
  | Literal text -> text
  | Field (record, name) -> expr record ^ "." ^ Identifier.quote name
  | Call (callee, args) ->
      expr callee ^ "(" ^ comma (fun (label, x) ->
        Option.fold ~none:"" ~some:(fun l -> Identifier.quote l ^ " = ") label ^ expr x) args ^ ")"
  | Binary (op, a, b) -> "(" ^ expr a ^ " " ^ op ^ " " ^ expr b ^ ")"
  | Unary (op, a) -> "(" ^ op ^ " " ^ expr a ^ ")"
  | If (c, y, n) -> "(if " ^ expr c ^ " then " ^ expr y ^ " else " ^ expr n ^ ")"
  | Lambda (ps, body) -> "(fn(" ^ String.concat ", " ps ^ ") => " ^ expr body ^ ")"

let declaration = function
  | Import (n,alias) -> "import " ^ n ^ (if n=alias then "" else " as " ^ alias)
  | Alias (n,ps,t) -> "type " ^ n ^ (if ps=[] then "" else "<" ^ String.concat ", " ps ^ ">") ^ " = " ^ annotation t
  | Type (n,ps,vs) -> "type " ^ n ^ (if ps=[] then "" else "<" ^ String.concat ", " ps ^ ">") ^ " = " ^
      String.concat " " (List.map (fun (tag,fs) -> "| " ^ tag ^ (if fs=[] then "" else "(" ^ comma (fun (n,t) -> n ^ ": " ^ annotation t) fs ^ ")")) vs)
  | Let (n,None,{form=Annotated (x,t);_}) -> "let " ^ n ^ ": " ^ annotation t ^ " = " ^ expr x
  | Contract (path, name) -> Printf.sprintf "use contract %S as %s" path name
  | Let (name, ps, body) ->
      let params = Option.fold ~none:"" ~some:(fun ps -> "(" ^ String.concat ", " ps ^ ")") ps in
      "let " ^ Identifier.quote name ^ params ^ " = " ^ expr body
  | Evaluate x -> expr x

let program ds = String.concat "\n" (List.map declaration ds)
