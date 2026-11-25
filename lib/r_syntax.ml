type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Power
  | Modulo
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Equal
  | Not_equal
  | Both
  | Either
  | Both_scalar
  | Either_scalar
  | In_set
  | Pipe
  | Tilde

type expr =
  | Number of string
  | String of string
  | Logical of bool
  | Null
  | Na
  | Name of string
  | Namespaced of string * string
  | Call of expr * argument list
  | Binary of operator * expr * expr
  | Not of expr
  | Negate of expr
  | Element of expr * expr
  | Dollar of expr * string
  | Function of string list * expr
  | If of expr * expr * expr
  | Block of (string * expr) list * expr
  | Located of Syntax.span * expr

and argument =
  | Positional of expr
  | Named of string * expr

type statement =
  | Assign of string * expr
  | Expression of expr
  | Verbatim of string

let call f args = Call (Name f, List.map (fun a -> Positional a) args)
let qualified package f args = Call (Namespaced (package, f), args)
let string s = String s

let strings = function
  | [ x ] -> String x
  | xs -> call "c" (List.map string xs)
;;

let number f =
  List.find
    (fun s -> float_of_string s = f)
    [ Printf.sprintf "%.15g" f; Printf.sprintf "%.16g" f; Printf.sprintf "%.17g" f ]
;;

let pipe a b = Binary (Pipe, a, b)

let symbol = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Power -> "^"
  | Modulo -> "%%"
  | Less -> "<"
  | Less_equal -> "<="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Equal -> "=="
  | Not_equal -> "!="
  | Both -> "&"
  | Either -> "|"
  | Both_scalar -> "&&"
  | Either_scalar -> "||"
  | In_set -> "%in%"
  | Pipe -> "|>"
  | Tilde -> "~"
;;

let binding = function
  | Power -> 14
  | In_set | Pipe -> 11
  | Multiply | Divide | Modulo -> 10
  | Add | Subtract -> 9
  | Less | Less_equal | Greater | Greater_equal | Equal | Not_equal -> 8
  | Both | Both_scalar -> 6
  | Either | Either_scalar -> 5
  | Tilde -> 4
;;

let rec precedence = function
  | Binary (op, _, _) -> binding op
  | Negate _ -> 13
  | Not _ -> 7
  | Function _ | If _ -> 1
  | Located (_, e) -> precedence e
  | _ -> 16
;;

let quote s = Yojson.Basic.to_string (`String s)

let identifier name =
  let plain =
    name <> ""
    && String.for_all
         (fun c ->
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '_'
            || c = '.')
         name
    && not (name.[0] >= '0' && name.[0] <= '9')
  in
  if plain then name else "`" ^ name ^ "`"
;;

let rec stages = function
  | Binary (Pipe, a, b) -> stages a @ [ b ]
  | Located (span, Binary (Pipe, a, b)) -> stages a @ [ Located (span, b) ]
  | e -> [ e ]
;;

let rec expr d indent level e =
  let p = precedence e in
  let wrap = p < level in
  if wrap then Document.text d "(";
  (match e with
   | Number s -> Document.text d s
   | String s -> Document.text d (quote s)
   | Logical b -> Document.text d (if b then "TRUE" else "FALSE")
   | Null -> Document.text d "NULL"
   | Na -> Document.text d "NA"
   | Name n -> Document.text d (identifier n)
   | Namespaced (package, n) -> Document.text d (package ^ "::" ^ identifier n)
   | Located (span, e) ->
     Document.mark d span;
     expr d indent level e
   | Binary (Pipe, _, _) ->
     (match stages e with
      | first :: rest ->
        expr d indent 12 first;
        List.iter
          (fun stage ->
             Document.text d " |>";
             Document.newline d (indent + 2);
             expr d (indent + 2) 12 stage)
          rest
      | [] -> ())
   | Binary (op, a, b) ->
     let left, right =
       match op with
       | Power -> p + 1, p
       | Less | Less_equal | Greater | Greater_equal | Equal | Not_equal -> p + 1, p + 1
       | _ -> p, p + 1
     in
     expr d indent left a;
     Document.text d (" " ^ symbol op ^ " ");
     expr d indent right b
   | Not e ->
     Document.text d "!";
     expr d indent p e
   | Negate e ->
     Document.text d "-";
     expr d indent p e
   | Call (f, arguments) ->
     expr d indent 16 f;
     Document.text d "(";
     Document.separated d ", " (argument d indent) arguments;
     Document.text d ")"
   | Dollar (e, n) ->
     expr d indent 16 e;
     Document.text d ("$" ^ identifier n)
   | Element (e, i) ->
     expr d indent 16 e;
     Document.text d "[[";
     expr d indent 0 i;
     Document.text d "]]"
   | Function (params, body) ->
     Document.text d ("function(" ^ String.concat ", " params ^ ") ");
     expr d indent 0 body
   | If (test, yes, no) ->
     Document.text d "if (";
     expr d indent 0 test;
     Document.text d ") ";
     expr d indent 2 yes;
     Document.text d " else ";
     expr d indent 1 no
   | Block (bindings, body) ->
     Document.text d "local({ ";
     List.iter
       (fun (n, e) ->
          Document.text d (identifier n ^ " <- ");
          expr d indent 0 e;
          Document.text d "; ")
       bindings;
     expr d indent 0 body;
     Document.text d " })");
  if wrap then Document.text d ")"

and argument d indent = function
  | Positional e -> expr d indent 0 e
  | Named (n, e) ->
    Document.text d (identifier n ^ " = ");
    expr d indent 0 e
;;

let statement d = function
  | Assign (n, e) ->
    Document.text d (identifier n ^ " <- ");
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
