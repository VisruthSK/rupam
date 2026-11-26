{
open Parser
let keyword = function
  | "import" -> IMPORT | "match" -> MATCH | "type" -> TYPE | "in" -> IN | "let" -> LET | "fn" -> FN | "if" -> IF | "else" -> ELSE
  | "use" -> USE | "set" -> SET | "end" -> LAST | "as" -> AS | "contract" -> CONTRACT
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
 | [' ' '\t']+ | '#' [^'\n' '\r']* { token lexbuf }
 | "\r\n" | '\n' | '\r' { Lexing.new_line lexbuf; END }
 | id as s { if Identifier.valid s then keyword s else Diagnostic.fail Syntax_error "Invalid Unicode identifier" }
 | '`' { quoted (Buffer.create 16) lexbuf }
 | number as n { INT (digits n) }
 | number ('.' number exponent? | exponent) as n { FLOAT (digits n) }
 | '"' (([^'"' '\\' '\n'] | '\\' [^'\n'])* as s) '"' { STRING (Scanf.unescaped s) }
 | 'd' '"' ([^'"' '\n']* as s) '"' { DATE s }
 | '|' { BAR } | '?' { QUESTION } | '@' { AT }
 | '[' { LB } | ']' { RB } | '{' { LC } | '}' { RC }
 | '(' { LP } | ')' { RP } | ',' { COMMA } | '.' { DOT } | ';' { Diagnostic.fail Syntax_error "Rupam has no `;`. End each item with a new line" }
 | "=..=" { CLOSED_CLOSED } | "=..<" { CLOSED_OPEN } | "<..=" { OPEN_CLOSED } | "<..<" { OPEN_OPEN }
 | "=.." { CLOSED_FROM } | "<.." { OPEN_FROM }
 | "..=" { DOTDOTEQ } | "..<" { DOTDOTLT } | ".." { DOTDOT } | "->" { TYPEARROW }
 | '!' (id as name) { TAP name }
 | "~=" | "\xe2\x89\x88" { APPROX } | "==" { EQUAL } | "!=" { NE } | ">=" { GE } | "<=" { LE } | '=' { EQ }
 | '>' { GT } | '<' { LT } | "=>" { ARROW } | "|>" { PIPE }
 | "??" { FALLBACK } | "match?" { MATCH_PRESENT }
 | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIV } | '%' { MOD }
 | ':' { COLON } | '$' { DOLLAR } | '^' { CARET } | '~' { TILDE }
 | eof { EOF } | _ as c { Diagnostic.fail Syntax_error (Printf.sprintf "Unexpected character %C" c) }
and quoted buffer = parse
 | "``" { Buffer.add_char buffer '`'; quoted buffer lexbuf }
 | '`' { NAME (Buffer.contents buffer) }
 | [^'`' '\n' '\r']+ as s { Buffer.add_string buffer s; quoted buffer lexbuf }
 | eof | '\n' | '\r' { Diagnostic.fail Syntax_error "Unterminated quoted identifier" }
