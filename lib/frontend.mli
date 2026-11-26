val parse : Lexing.lexbuf -> Syntax.item list
val valid_prefix : string -> (string * Syntax.item list) option
val read : ?file:string -> string -> Syntax.item list
val table_draft : string -> string option * Syntax.expr
val incomplete : string -> bool
