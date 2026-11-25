val parse : Lexing.lexbuf -> Syntax.declaration list
val valid_prefix : string -> (string * Syntax.declaration list) option
