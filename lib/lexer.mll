{
open Parser
let keyword = function
  | "import" -> IMPORT | "match" -> MATCH | "type" -> TYPE | "in" -> IN | "let" -> LET | "fn" -> FN | "if" -> IF | "else" -> ELSE
  | "use" -> USE | "as" -> AS | "contract" -> CONTRACT
  | "and" -> AND | "or" -> OR | "not" -> NOT
  | "TRUE" -> TRUE | "FALSE" -> FALSE | "None" -> NONE | s -> NAME s
let digits s = String.concat "" (String.split_on_char '_' s)
}
let digit = ['0'-'9']
let number = digit ('_'? digit)*
let exponent = ['e' 'E'] ['+' '-']? number
let start = ['a'-'z' 'A'-'Z' '_' '\128'-'\255']
let id = start (start | digit)*
rule token = parse
 | [' ' '\t' '\r']+ | '#' [^'\n']* { token lexbuf }
 | '\n' { Lexing.new_line lexbuf; END }
 | id as s { if Identifier.valid s then keyword s else failwith "Invalid Unicode identifier" }
 | '`' { quoted (Buffer.create 16) lexbuf }
 | number as n { INT (digits n) }
 | number ('.' number exponent? | exponent) as n { FLOAT (digits n) }
 | '"' (([^'"' '\\' '\n'] | '\\' [^'\n'])* as s) '"' { STRING (Scanf.unescaped s) }
 | '|' { BAR } | '?' { QUESTION } | '@' { AT }
 | '[' { LB } | ']' { RB } | '{' { LC } | '}' { RC }
 | '(' { LP } | ')' { RP } | ',' { COMMA } | '.' { DOT } | ';' { SEMI }
 | "..=" { DOTDOTEQ } | "..<" { DOTDOTLT } | ".." { DOTDOT } | "->" { TYPEARROW }
 | "==" { EQUAL } | "!=" { NE } | ">=" { GE } | "<=" { LE } | '=' { EQ }
 | '>' { GT } | '<' { LT } | "=>" { ARROW } | "|>" { PIPE }
 | "%in%" { MEMBER }
 | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIV } | '%' { MOD }
 | ':' { COLON } | '$' { DOLLAR } | '^' { CARET } | '~' { TILDE }
 | eof { EOF } | _ as c { failwith (Printf.sprintf "Unexpected character %C" c) }
and quoted buffer = parse
 | "``" { Buffer.add_char buffer '`'; quoted buffer lexbuf }
 | '`' { NAME (Buffer.contents buffer) }
 | [^'`' '\n' '\r']+ as s { Buffer.add_string buffer s; quoted buffer lexbuf }
 | eof | '\n' | '\r' { failwith "Unterminated quoted identifier" }
