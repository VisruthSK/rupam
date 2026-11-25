open Syntax

let comma f xs = String.concat ", " (List.map f xs)

let rec annotation = function
  | Atuple ts -> "(" ^ comma annotation ts ^ ")"
  | Aarrow (a, b) -> "(" ^ annotation a ^ " -> " ^ annotation b ^ ")"
  | Arow (fs, tail) ->
    "{"
    ^ String.concat
        ", "
        (List.map (fun (n, t) -> n ^ ": " ^ annotation t) fs
         @ Option.to_list (Option.map (fun n -> ".." ^ n) tail))
    ^ "}"
  | Named (n, []) -> n
  | Named (n, ts) -> n ^ "<" ^ comma annotation ts ^ ">"
  | Optional t -> annotation t ^ "?"
;;

let rec pattern = function
  | Precord ns -> "{" ^ String.concat ", " ns ^ "}"
  | Wildcard -> "_"
  | Bind n | Tag (n, []) -> n
  | Pliteral l -> literal_text l
  | Tag (n, ps) -> n ^ "(" ^ comma pattern ps ^ ")"
  | Ptuple ps -> "(" ^ comma pattern ps ^ ")"
  | Prange { low; high; inclusive } ->
    Option.fold ~none:"" ~some:literal_text low
    ^
      (match high with
      | None -> ".."
      | Some h -> (if inclusive then "..=" else "..<") ^ literal_text h)
  | Palt ps -> String.concat " | " (List.map pattern ps)
  | Pas (n, p) -> n ^ " @ " ^ pattern p
;;

let rec expr x =
  match x.form with
  | Name (scope, name) ->
    (match scope with
     | Bare -> ""
     | Column -> "$"
     | Lexical -> "^")
    ^ Identifier.quote name
  | Match (x, bs) ->
    "match "
    ^ expr x
    ^ " {"
    ^ comma
        (fun a ->
           pattern a.pattern
           ^ Option.fold ~none:"" ~some:(fun g -> " if " ^ expr g) a.guard
           ^ " => "
           ^ expr a.body)
        bs
    ^ "}"
  | Member (a, b) -> "(" ^ expr a ^ " %in% " ^ expr b ^ ")"
  | Interpolate pieces ->
    "\""
    ^ String.concat
        ""
        (List.map
           (function
             | Text_piece s ->
               String.concat "{{" (String.split_on_char '{' s)
               |> String.split_on_char '}'
               |> String.concat "}}"
             | Slot { value; format; echo } ->
               "{"
               ^ Option.fold ~none:(expr value) ~some:Fun.id echo
               ^ Option.fold ~none:"" ~some:(fun f -> ":" ^ f) format
               ^ "}")
           pieces)
    ^ "\""
  | Annotated (x, _) -> expr x
  | Tuple xs -> "(" ^ comma expr xs ^ ")"
  | List xs -> "[" ^ comma expr xs ^ "]"
  | Record fields ->
    "{" ^ comma (fun (n, x) -> Identifier.quote n ^ " = " ^ expr x) fields ^ "}"
  | Local (n, x, y) -> "(let " ^ n ^ " = " ^ expr x ^ " in " ^ expr y ^ ")"
  | Literal l -> literal_text l
  | Field (record, name) -> expr record ^ "." ^ Identifier.quote name
  | Call (callee, args) ->
    expr callee
    ^ "("
    ^ comma
        (fun (label, x) ->
           Option.fold ~none:"" ~some:(fun l -> Identifier.quote l ^ " = ") label ^ expr x)
        args
    ^ ")"
  | Pipe (a, b) -> "(" ^ expr a ^ " |> " ^ expr b ^ ")"
  | Formula (lhs, rhs) ->
    "(" ^ Option.fold ~none:"" ~some:(fun l -> expr l ^ " ") lhs ^ "~ " ^ expr rhs ^ ")"
  | Range (a, b) -> expr a ^ ":" ^ expr b
  | Binary (op, a, b) -> "(" ^ expr a ^ " " ^ binary_symbol op ^ " " ^ expr b ^ ")"
  | Unary (op, a) -> "(" ^ unary_symbol op ^ " " ^ expr a ^ ")"
  | If (c, y, n) -> "(if " ^ expr c ^ " then " ^ expr y ^ " else " ^ expr n ^ ")"
  | Lambda (ps, body) -> "(fn(" ^ String.concat ", " ps ^ ") => " ^ expr body ^ ")"
;;

let declaration = function
  | Import (n, alias) -> "import " ^ n ^ if n = alias then "" else " as " ^ alias
  | Alias (n, ps, t) ->
    "type "
    ^ n
    ^ (if ps = [] then "" else "<" ^ String.concat ", " ps ^ ">")
    ^ " = "
    ^ annotation t
  | Type (n, ps, vs) ->
    "type "
    ^ n
    ^ (if ps = [] then "" else "<" ^ String.concat ", " ps ^ ">")
    ^ " = "
    ^ String.concat
        " "
        (List.map
           (fun (tag, fs) ->
              "| "
              ^ tag
              ^
              if fs = []
              then ""
              else "(" ^ comma (fun (n, t) -> n ^ ": " ^ annotation t) fs ^ ")")
           vs)
  | Let (n, None, { form = Annotated (x, t); _ }) ->
    "let " ^ n ^ ": " ^ annotation t ^ " = " ^ expr x
  | Contract (path, name) -> Printf.sprintf "use contract %S as %s" path name
  | Setting (name, value) -> "use " ^ name ^ " = " ^ value
  | Let (name, ps, body) ->
    let params =
      Option.fold ~none:"" ~some:(fun ps -> "(" ^ String.concat ", " ps ^ ")") ps
    in
    "let " ^ Identifier.quote name ^ params ^ " = " ^ expr body
  | Evaluate x -> expr x
;;

let program ds = String.concat "\n" (List.map declaration ds)
