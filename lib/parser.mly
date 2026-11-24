%{
open Syntax
let at start finish form = { form; span = start, finish }
let parameters ps body =
  List.fold_right (fun (n,t) body -> match t with None -> body | Some annotation ->
    let var = {body with form=Name (Bare,n)} in
    {body with form=Local (n,{body with form=Annotated (var,annotation)},body)}) ps body
let result_annotation t body = match t with None -> body | Some a -> {body with form=Annotated (body,a)}
%}
%token DOTDOT TYPEARROW IMPORT TYPE MATCH BAR QUESTION
%token LET USE CONTRACT AS FN IF THEN ELSE
%token <string> NAME NUMBER STRING BOOL
%token IN LB RB LC RC
%token LP RP COMMA EQ DOT PIPE DOLLAR CARET ARROW END EOF
%token PLUS MINUS TIMES DIV GT LT GE LE NE EQUAL AND OR NOT COLON MOD
%right TYPEARROW
%nonassoc QUESTION
%right IN ELSE ARROW
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
decl: IMPORT NAME { Import ($2,$2) } | IMPORT NAME AS NAME { Import ($2,$4) }
 | TYPE NAME type_params EQ annotation { Alias ($2,$3,$5) }
 | TYPE NAME type_params EQ variants { Type ($2,$3,List.rev $5) }
 | USE CONTRACT STRING AS NAME { Contract ($3,$5) }
 | LET NAME COLON annotation EQ expr { Let ($2,None,at $startpos $endpos (Annotated ($6,$4))) }
 | LET NAME EQ expr { Let ($2,None,$4) }
 | LET NAME LP params RP return_type EQ expr { Let ($2,Some (List.map fst $4),parameters $4 (result_annotation $6 $8)) }
 | expr { Evaluate $1 };
type_params: /* empty */ { [] } | LT names GT { List.rev $2 };
annotation: NAME { Named ($1,[]) } | NAME LT annotations GT { Named ($1,List.rev $3) }
 | annotation QUESTION { Optional $1 }
 | annotation TYPEARROW annotation { Aarrow ($1,$3) }
 | LP annotations RP { match List.rev $2 with [t] -> t | ts -> Atuple ts }
 | LC RC { Arow ([],None) }
 | LC typed_fields RC { Arow (List.rev $2,None) }
 | LC DOTDOT NAME RC { Arow ([],Some $3) }
 | LC typed_fields COMMA DOTDOT NAME RC { Arow (List.rev $2,Some $5) };
annotations: annotation { [$1] } | annotations COMMA annotation { $3 :: $1 };
variants: BAR variant { [$2] } | variants BAR variant { $3 :: $1 };
variant: NAME { $1,[] } | NAME LP typed_fields RP { $1,List.rev $3 };
typed_fields: NAME COLON annotation { [$1,$3] } | typed_fields COMMA NAME COLON annotation { ($3,$5) :: $1 };
branches: pattern ARROW expr { [$1,$3] } | branches COMMA pattern ARROW expr { ($3,$5) :: $1 };
pattern: NAME { if $1="_" then Wildcard else if Char.uppercase_ascii $1.[0]=$1.[0] then Tag ($1,[]) else Bind $1 }
 | NAME LP patterns RP { Tag ($1,List.rev $3) }
 | LC names RC { Precord (List.rev $2) }
 | STRING { Pliteral (Printf.sprintf "%S" $1) }
 | BOOL { Pliteral $1 } | NUMBER { Pliteral $1 }
 | LP pattern COMMA patterns RP { Ptuple ($2 :: List.rev $4) };
patterns: pattern { [$1] } | patterns COMMA pattern { $3 :: $1 };
return_type: /* empty */ { None } | COLON annotation { Some $2 };
param: NAME { $1,None } | NAME COLON annotation { $1,Some $3 };
params: /* empty */ { [] } | param_list { List.rev $1 };
param_list: param { [$1] } | param_list COMMA param { $3 :: $1 };
names: NAME { [$1] } | names COMMA NAME { $3 :: $1 };
args: /* empty */ { [] } | arguments { List.rev $1 } | arguments COMMA { List.rev $1 };
arguments: arg { [$1] } | arguments COMMA arg { $3 :: $1 };
arg: expr { None,$1 } | NAME EQ expr { Some $1,$3 };
values: expr { [$1] } | values COMMA expr { $3 :: $1 };
fields: NAME EQ expr { [$1,$3] } | fields COMMA NAME EQ expr { ($3,$5) :: $1 };
expr: MATCH expr LC branches RC { at $startpos $endpos (Match ($2,List.rev $4)) }
 | LB RB { at $startpos $endpos (List []) }
 | LB values RB { at $startpos $endpos (List (List.rev $2)) }
 | LC RC { at $startpos $endpos (Record []) }
 | LC fields RC { at $startpos $endpos (Record (List.rev $2)) }
 | LP expr COMMA values RP { at $startpos $endpos (Tuple ($2 :: List.rev $4)) }
 | LET NAME EQ expr IN expr { at $startpos $endpos (Local ($2,$4,$6)) }
 | NAME { at $startpos $endpos (Name (Bare,$1)) } | DOLLAR NAME { at $startpos $endpos (Name (Column,$2)) }
 | CARET NAME { at $startpos $endpos (Name (Lexical,$2)) } | NUMBER { at $startpos $endpos (Literal $1) }
 | BOOL { at $startpos $endpos (Literal $1) } | STRING { at $startpos $endpos (Literal (Printf.sprintf "%S" $1)) }
 | LP expr RP { $2 } | expr DOT NAME { at $startpos $endpos (Field ($1,$3)) }
 | expr LP args RP { at $startpos $endpos (Call ($1,$3)) }
 | IF expr THEN expr ELSE expr { at $startpos $endpos (If ($2,$4,$6)) }
 | FN LP params RP ARROW expr { at $startpos $endpos (Lambda (List.map fst $3,parameters $3 $6)) }
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
