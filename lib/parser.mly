%{
open Syntax
let at start finish form = { form; span = start, finish }
%}
%token LET USE CONTRACT AS FN IF THEN ELSE
%token <string> NAME NUMBER STRING BOOL
%token LP RP COMMA EQ DOT PIPE DOLLAR CARET ARROW END EOF
%token PLUS MINUS TIMES DIV GT LT GE LE NE EQUAL AND OR NOT COLON MOD
%right ELSE ARROW
%left PIPE
%left OR
%left AND
%nonassoc GT LT GE LE NE EQUAL
%nonassoc COLON
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UNARY
%left LP DOT
%start program
%type <Syntax.declaration list> program
%%
program: items EOF { List.rev $1 };
items: /* empty */ { [] } | items END { $1 } | items decl END { $2 :: $1 };
decl: USE CONTRACT STRING AS NAME { Contract ($3,$5) }
 | LET NAME EQ expr { Let ($2,None,$4) }
 | LET NAME LP params RP EQ expr { Let ($2,Some $4,$7) }
 | expr { Evaluate $1 };
params: /* empty */ { [] } | names { List.rev $1 };
names: NAME { [$1] } | names COMMA NAME { $3 :: $1 };
args: /* empty */ { [] } | arguments { List.rev $1 } | arguments COMMA { List.rev $1 };
arguments: arg { [$1] } | arguments COMMA arg { $3 :: $1 };
arg: expr { None,$1 } | NAME EQ expr { Some $1,$3 };
expr: NAME { at $startpos $endpos (Name (Bare,$1)) } | DOLLAR NAME { at $startpos $endpos (Name (Column,$2)) }
 | CARET NAME { at $startpos $endpos (Name (Lexical,$2)) } | NUMBER { at $startpos $endpos (Literal $1) }
 | BOOL { at $startpos $endpos (Literal $1) } | STRING { at $startpos $endpos (Literal (Printf.sprintf "%S" $1)) }
 | LP expr RP { $2 } | expr DOT NAME { at $startpos $endpos (Field ($1,$3)) }
 | expr LP args RP { at $startpos $endpos (Call ($1,$3)) }
 | IF expr THEN expr ELSE expr { at $startpos $endpos (If ($2,$4,$6)) }
 | FN LP params RP ARROW expr { at $startpos $endpos (Lambda ($3,$6)) }
 | MINUS expr %prec UNARY { at $startpos $endpos (Unary ("-",$2)) }
 | NOT expr %prec UNARY { at $startpos $endpos (Unary ("not",$2)) }
 | expr PLUS expr { at $startpos $endpos (Binary ("+",$1,$3)) }
 | expr MINUS expr { at $startpos $endpos (Binary ("-",$1,$3)) }
 | expr TIMES expr { at $startpos $endpos (Binary ("*",$1,$3)) }
 | expr DIV expr { at $startpos $endpos (Binary ("/",$1,$3)) }
 | expr MOD expr { at $startpos $endpos (Binary ("%",$1,$3)) }
 | expr GT expr { at $startpos $endpos (Binary (">",$1,$3)) }
 | expr LT expr { at $startpos $endpos (Binary ("<",$1,$3)) }
 | expr GE expr { at $startpos $endpos (Binary (">=",$1,$3)) }
 | expr LE expr { at $startpos $endpos (Binary ("<=",$1,$3)) }
 | expr EQUAL expr { at $startpos $endpos (Binary ("==",$1,$3)) }
 | expr NE expr { at $startpos $endpos (Binary ("!=",$1,$3)) }
 | expr PIPE expr { at $startpos $endpos (Binary ("|>",$1,$3)) }
 | expr AND expr { at $startpos $endpos (Binary ("and",$1,$3)) }
 | expr OR expr { at $startpos $endpos (Binary ("or",$1,$3)) }
 | expr COLON expr { at $startpos $endpos (Binary (":",$1,$3)) }
;

