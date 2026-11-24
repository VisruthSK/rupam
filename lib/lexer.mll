{
open Parser
let keyword = function
  | "in" -> IN | "let" -> LET | "fn" -> FN | "if" -> IF | "then" -> THEN | "else" -> ELSE
  | "use" -> USE | "as" -> AS | "contract" -> CONTRACT
  | "and" -> AND | "or" -> OR | "not" -> NOT
  | ("true" | "false" | "None") as s -> BOOL s | s -> NAME s
}
let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
rule token = parse
 | [' ' '\t' '\r']+ | '#' [^'\n']* { token lexbuf }
 | '\n' { Lexing.new_line lexbuf; END }
 | id as s { keyword s }
 | digit+ ('.' digit+)? (['e' 'E'] ['+' '-']? digit+)? as n { NUMBER n }
 | '"' (([^'"' '\\' '\n'] | '\\' [^'\n'])* as s) '"' { STRING (Scanf.unescaped s) }
 | '[' { LB } | ']' { RB } | '{' { LC } | '}' { RC }
 | '(' { LP } | ')' { RP } | ',' { COMMA } | '.' { DOT } | ';' { END }
 | "==" { EQUAL } | "!=" { NE } | ">=" { GE } | "<=" { LE } | '=' { EQ }
 | '>' { GT } | '<' { LT } | "=>" { ARROW } | "|>" { PIPE }
 | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIV } | '%' { MOD }
 | ':' { COLON } | '$' { DOLLAR } | '^' { CARET }
 | eof { EOF } | _ as c { failwith (Printf.sprintf "Unexpected character %C" c) }

