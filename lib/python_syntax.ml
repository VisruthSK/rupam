type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Power
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Equal
  | Not_equal
  | Both
  | Either
  | And
  | Or
  | Is
  | Is_not
  | In

type unary =
  | Negate
  | Invert
  | Not

type expr =
  | Int of string
  | Float of string
  | Str of string
  | Bool of bool
  | Null
  | Name of string
  | Attribute of expr * string
  | Call of expr * argument list
  | Binary of operator * expr * expr
  | Unary of unary * expr
  | List of expr list
  | Dict of (expr * expr) list
  | Tuple of expr list
  | Subscript of expr * expr
  | Walrus of string * expr
  | Lambda of string list * expr
  | If of expr * expr * expr
  | Chain of expr * link list
  | Located of Syntax.span * expr

and link =
  { span : Syntax.span option
  ; name : string
  ; arguments : argument list
  }

and argument =
  | Positional of expr
  | Keyword of string * expr

type statement =
  | Import of string * string option
  | Assign of string * expr
  | Expression of expr
  | Verbatim of string

let call f args = Call (Name f, List.map (fun a -> Positional a) args)
let meth receiver name arguments = Call (Attribute (receiver, name), arguments)
let str s = Str s
let names xs = List (List.map str xs)

let symbol = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Power -> "**"
  | Less -> "<"
  | Less_equal -> "<="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Equal -> "=="
  | Not_equal -> "!="
  | Both -> "&"
  | Either -> "|"
  | And -> "and"
  | Or -> "or"
  | Is -> "is"
  | Is_not -> "is not"
  | In -> "in"
;;

let binding = function
  | Or -> 2
  | And -> 3
  | Either -> 6
  | Both -> 8
  | Less | Less_equal | Greater | Greater_equal | Equal | Not_equal | Is | Is_not | In ->
    5
  | Add | Subtract -> 10
  | Multiply | Divide | Modulo -> 11
  | Power -> 13
;;

let rec precedence = function
  | Lambda _ -> 0
  | If _ -> 1
  | Binary (op, _, _) -> binding op
  | Unary (Not, _) -> 4
  | Unary _ -> 12
  | Located (_, e) -> precedence e
  | _ -> 15
;;

let link ?span name arguments = { span; name; arguments }

let float f =
  let shortest =
    List.find
      (fun s -> float_of_string s = f)
      [ Printf.sprintf "%.15g" f; Printf.sprintf "%.16g" f; Printf.sprintf "%.17g" f ]
  in
  if
    String.exists
      (function
        | '.' | 'e' | 'n' | 'i' -> true
        | _ -> false)
      shortest
  then shortest
  else shortest ^ ".0"
;;

let quote s = Yojson.Basic.to_string (`String s)

let rec expr d indent level e =
  let p = precedence e in
  let wrap = p < level in
  if wrap then Document.text d "(";
  (match e with
   | Int s | Float s -> Document.text d s
   | Str s -> Document.text d (quote s)
   | Bool b -> Document.text d (if b then "True" else "False")
   | Null -> Document.text d "None"
   | Name n -> Document.text d n
   | Located (span, e) ->
     Document.mark d span;
     expr d indent level e
   | Chain (base, links) -> chain d indent base links
   | Attribute (e, name) ->
     expr d indent 15 e;
     Document.text d ("." ^ name)
   | Call (f, arguments) ->
     expr d indent 15 f;
     Document.text d "(";
     Document.separated d ", " (argument d indent) arguments;
     Document.text d ")"
   | Binary (op, a, b) ->
     let left, right =
       match op with
       | Power -> p + 1, p
       | Less
       | Less_equal
       | Greater
       | Greater_equal
       | Equal
       | Not_equal
       | Is
       | Is_not
       | In -> p + 1, p + 1
       | _ -> p, p + 1
     in
     expr d indent left a;
     Document.text d (" " ^ symbol op ^ " ");
     expr d indent right b
   | Unary (op, e) ->
     Document.text
       d
       (match op with
        | Negate -> "-"
        | Invert -> "~"
        | Not -> "not ");
     expr d indent p e
   | Tuple [ x ] ->
     Document.text d "(";
     expr d indent 0 x;
     Document.text d ",)"
   | Tuple xs ->
     Document.text d "(";
     Document.separated d ", " (expr d indent 0) xs;
     Document.text d ")"
   | Subscript (e, index) ->
     expr d indent 15 e;
     Document.text d "[";
     expr d indent 0 index;
     Document.text d "]"
   | Walrus (name, e) ->
     Document.text d ("(" ^ name ^ " := ");
     expr d indent 0 e;
     Document.text d ")"
   | List xs ->
     Document.text d "[";
     Document.separated d ", " (expr d indent 0) xs;
     Document.text d "]"
   | Dict pairs ->
     Document.text d "{";
     Document.separated
       d
       ", "
       (fun (k, v) ->
          expr d indent 0 k;
          Document.text d ": ";
          expr d indent 0 v)
       pairs;
     Document.text d "}"
   | Lambda (params, body) ->
     Document.text d ("lambda " ^ String.concat ", " params ^ ": ");
     expr d indent 0 body
   | If (test, yes, no) ->
     expr d indent 2 yes;
     Document.text d " if ";
     expr d indent 2 test;
     Document.text d " else ";
     expr d indent 1 no);
  if wrap then Document.text d ")"

and chain d indent base links =
  let inner = indent + 4 in
  Document.text d "(";
  Document.newline d inner;
  expr d inner 15 base;
  List.iter
    (fun { span; name; arguments } ->
       Document.newline d inner;
       Option.iter (Document.mark d) span;
       Document.text d ("." ^ name ^ "(");
       Document.separated d ", " (argument d inner) arguments;
       Document.text d ")")
    links;
  Document.newline d indent;
  Document.text d ")"

and argument d indent = function
  | Positional e -> expr d indent 0 e
  | Keyword (k, e) ->
    Document.text d (k ^ "=");
    expr d indent 0 e
;;

let statement d = function
  | Import (m, None) -> Document.text d ("import " ^ m)
  | Import (m, Some alias) -> Document.text d ("import " ^ m ^ " as " ^ alias)
  | Assign (n, e) ->
    Document.text d (n ^ " = ");
    expr d 0 0 e
  | Expression e -> expr d 0 0 e
  | Verbatim s -> Document.text d (String.trim s)
;;

let program statements =
  let d = Document.create () in
  List.iter
    (fun s ->
       statement d s;
       Document.text d "\n")
    statements;
  Document.contents d
;;

let render e =
  let d = Document.create () in
  expr d 0 0 e;
  fst (Document.contents d)
;;
