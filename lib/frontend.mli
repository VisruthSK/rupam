val parse : Lexing.lexbuf -> Syntax.declaration list
val valid_prefix : string -> (string * Syntax.declaration list) option
val read : ?file:string -> string -> Syntax.declaration list
val incomplete : string -> bool
