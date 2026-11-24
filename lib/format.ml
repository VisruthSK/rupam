open Syntax

let comma f xs = String.concat ", " (List.map f xs)

let rec expr x = match x.form with
  | Name (scope, name) ->
      (match scope with Bare -> "" | Column -> "$" | Lexical -> "^") ^ name
  | Literal text -> text
  | Field (record, name) -> expr record ^ "." ^ name
  | Call (callee, args) ->
      expr callee ^ "(" ^ comma (fun (label, x) ->
        Option.fold ~none:"" ~some:(fun l -> l ^ " = ") label ^ expr x) args ^ ")"
  | Binary (op, a, b) -> "(" ^ expr a ^ " " ^ op ^ " " ^ expr b ^ ")"
  | Unary (op, a) -> "(" ^ op ^ " " ^ expr a ^ ")"
  | If (c, y, n) -> "(if " ^ expr c ^ " then " ^ expr y ^ " else " ^ expr n ^ ")"
  | Lambda (ps, body) -> "(fn(" ^ String.concat ", " ps ^ ") => " ^ expr body ^ ")"

let declaration = function
  | Contract (path, name) -> Printf.sprintf "use contract %S as %s" path name
  | Let (name, ps, body) ->
      let params = Option.fold ~none:"" ~some:(fun ps -> "(" ^ String.concat ", " ps ^ ")") ps in
      "let " ^ name ^ params ^ " = " ^ expr body
  | Evaluate x -> expr x

let program ds = String.concat "\n" (List.map declaration ds)
