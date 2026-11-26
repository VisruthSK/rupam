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
  | Prange { low; high } ->
    (match low with
     | None -> ""
     | Some b -> literal_text b.value ^ if b.closed then "=" else "<")
    ^ ".."
    ^ Option.fold
        ~none:""
        ~some:(fun b -> (if b.closed then "=" else "<") ^ literal_text b.value)
        high
  | Palt ps -> String.concat " | " (List.map pattern ps)
  | Pas (n, p) -> n ^ " @ " ^ pattern p
;;

let binary_level : binary -> int = function
  | Power -> 2
  | Mul | Div | Mod -> 4
  | Add | Sub -> 5
  | Less | Greater | Less_equal | Greater_equal | Equal | Not_equal -> 8
  | And -> 10
  | Or -> 11
;;

let level x =
  match x.form with
  | Name _ | Literal _ | Call _ | Field _ | Tuple _ | List _ | Record _ | Interpolate _ ->
    1
  | Annotated _ -> 1
  | Binary (op, _, _) -> binary_level op
  | Unary (Negate, _) -> 3
  | Range _ -> 6
  | Fallback _ -> 7
  | Sequence _ -> 6
  | Index _ | Last -> 1
  | Tap _ -> 1
  | Member _ | Chain _ -> 8
  | Unary (Not, { form = Member _; _ }) -> 8
  | Unary (Not, _) -> 9
  | Formula _ -> 12
  | Pipe _ -> 13
  | Match _ | Match_present _ | Local _ | If _ | Lambda _ -> 14
;;

let rec stages x =
  match x.form with
  | Pipe (a, b) -> stages a @ [ b ]
  | _ -> [ x ]
;;

let rec at limit x =
  let text = expr x in
  if level x > limit then "(" ^ text ^ ")" else text

and expr x =
  match x.form with
  | Name (scope, name) ->
    (match scope with
     | Bare -> ""
     | Column -> "$"
     | Lexical -> "^")
    ^ Identifier.quote name
  | Match (x, bs) ->
    "match "
    ^ at 13 x
    ^ " {"
    ^ comma
        (fun a ->
           pattern a.pattern
           ^ Option.fold ~none:"" ~some:(fun g -> " if " ^ expr g) a.guard
           ^ " => "
           ^ expr a.body)
        bs
    ^ "}"
  | Member (a, b) -> at 7 a ^ " in " ^ at 7 b
  | Fallback (a, b) -> at 6 a ^ " ?? " ^ at 7 b
  | Sequence r ->
    at 5 r.first
    ^ (if r.first_closed then "=" else "<")
    ^ ".."
    ^ (if r.last_closed then "=" else "<")
    ^ at 5 r.last
  | Index (x, Element i) -> at 1 x ^ "[" ^ expr i ^ "]"
  | Index (x, From (i, closed)) ->
    at 1 x ^ "[" ^ at 5 i ^ (if closed then "=.." else "<..") ^ "]"
  | Index (x, Upto (i, inclusive)) ->
    at 1 x ^ "[" ^ (if inclusive then "..=" else "..<") ^ at 5 i ^ "]"
  | Last -> "end"
  | Tap (tap, args) ->
    "!"
    ^ (match tap with
       | Print -> "print"
       | View -> "view"
       | Schema -> "schema"
       | Hist -> "hist"
       | Bar -> "bar")
    ^ "("
    ^ comma
        (fun (label, x) ->
           Option.fold ~none:"" ~some:(fun l -> Identifier.quote l ^ " = ") label ^ expr x)
        args
    ^ ")"
  | Match_present (x, bs) ->
    "match? "
    ^ at 13 x
    ^ " {"
    ^ comma
        (fun a ->
           pattern a.pattern
           ^ Option.fold ~none:"" ~some:(fun g -> " if " ^ expr g) a.guard
           ^ " => "
           ^ expr a.body)
        bs
    ^ "}"
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
  | Local (n, x, y) -> "let " ^ n ^ " = " ^ expr x ^ " in " ^ expr y
  | Literal l -> literal_text l
  | Field (record, name) -> at 1 record ^ "." ^ Identifier.quote name
  | Call (callee, args) ->
    at 1 callee
    ^ "("
    ^ comma
        (fun (label, x) ->
           Option.fold ~none:"" ~some:(fun l -> Identifier.quote l ^ " = ") label ^ expr x)
        args
    ^ ")"
  | Pipe _ ->
    let parts = List.map (at 12) (stages x) in
    let inline = String.concat " |> " parts in
    if List.length parts <= 2 && String.length inline <= 72
    then inline
    else "\n    " ^ String.concat "\n    |> " parts
  | Formula (lhs, rhs) ->
    Option.fold ~none:"" ~some:(fun l -> at 11 l ^ " ") lhs ^ "~ " ^ at 11 rhs
  | Range (a, b) -> at 5 a ^ ":" ^ at 5 b
  | Chain (a, links) ->
    at 7 a
    ^ String.concat
        ""
        (List.map (fun (op, x) -> " " ^ binary_symbol op ^ " " ^ at 7 x) links)
  | Binary (Power, a, b) -> at 1 a ^ " ^ " ^ at 3 b
  | Binary (op, a, b) ->
    let l = binary_level op in
    let left, right = if l = 8 then l - 1, l - 1 else l, l - 1 in
    at left a ^ " " ^ binary_symbol op ^ " " ^ at right b
  | Unary (Negate, a) -> "-" ^ at 1 a
  | Unary (Not, { form = Member (a, b); _ }) -> at 7 a ^ " not in " ^ at 7 b
  | Unary (op, a) -> unary_symbol op ^ " " ^ at 9 a
  | If (c, y, n) -> "if (" ^ expr c ^ ") " ^ expr y ^ " else " ^ expr n
  | Lambda (ps, body) -> "fn(" ^ String.concat ", " ps ^ ") => " ^ expr body
;;

let expression = expr

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
  | Setting (name, value) -> "set " ^ name ^ " = " ^ value
  | Let (name, ps, body) ->
    let params =
      Option.fold ~none:"" ~some:(fun ps -> "(" ^ String.concat ", " ps ^ ")") ps
    in
    let body = expr body in
    "let "
    ^ Identifier.quote name
    ^ params
    ^ if String.starts_with ~prefix:"\n" body then " =" ^ body else " = " ^ body
  | Evaluate x ->
    let text = expr x in
    if String.starts_with ~prefix:"\n" text
    then String.sub text 5 (String.length text - 5)
    else text
;;

let kind = function
  | Import _ | Contract _ -> `Use
  | Setting _ -> `Set
  | Alias _ | Type _ -> `Type
  | Let _ -> `Let
  | Evaluate _ -> `Evaluate
;;

let program (items : item list) =
  let ds = List.map (fun (i : item) -> i.declaration) items in
  let rec go previous = function
    | [] -> []
    | d :: rest ->
      let text = declaration d in
      let multiline = String.contains text '\n' in
      let separate =
        match previous with
        | None -> ""
        | Some (k, wide) -> if k = kind d && not (wide || multiline) then "\n" else "\n\n"
      in
      (separate ^ text) :: go (Some (kind d, multiline)) rest
  in
  String.concat "" (go None ds)
;;
