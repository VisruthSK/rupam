%{
open Syntax
let at start finish form = { form; span = start, finish }
let parameters ps body =
  List.fold_right (fun (n,t) body -> match t with None -> body | Some annotation ->
    let var = {body with form=Name (Bare,n)} in
    {body with form=Local (n,{body with form=Annotated (var,annotation)},body)}) ps body
let result_annotation t body = match t with None -> body | Some a -> {body with form=Annotated (body,a)}
let negative = function Int s -> Int ("-" ^ s) | Float s -> Float ("-" ^ s) | l -> l
let alternatives a b = match a with Palt ps -> Palt (ps @ [b]) | a -> Palt [a; b]
let product = function [p] -> p | ps -> Ptuple ps
let ascending = function Less | Less_equal -> Some true | Greater | Greater_equal -> Some false | _ -> None
let bare start (x : expr) = fst x.span = start
let compare start finish op (a : expr) (b : expr) =
  let reject message = Diagnostic.fail ~span:(start, finish) Syntax_error message in
  let extend first links =
    if ascending op <> None && ascending op = ascending (fst (List.hd links))
    then at start finish (Chain (first, links @ [ op, b ]))
    else reject "Only <, <= or >, >= chain, all in one direction"
  in
  match a.form with
  | Binary (first, x, y) when bare start a && ascending first <> None -> extend x [ first, y ]
  | Chain (x, links) when bare start a -> extend x links
  | (Binary ((Equal | Not_equal | Approx), _, _) | Member _) when bare start a ->
    reject "Equality and membership do not chain"
  | _ -> at start finish (Binary (op, a, b))
let subject start finish = function [x] -> x | xs -> at start finish (Tuple xs)
%}
%token DOTDOT DOTDOTEQ DOTDOTLT TYPEARROW IMPORT TYPE MATCH BAR QUESTION AT
%token SET FALLBACK MATCH_PRESENT LAST
%token CLOSED_CLOSED CLOSED_OPEN OPEN_CLOSED OPEN_OPEN CLOSED_FROM OPEN_FROM
%token LET USE CONTRACT AS FN IF ELSE
%token <string> NAME QUOTED INT FLOAT STRING TAP DATE
%token TRUE FALSE NONE
%token IN LB RB LC RC
%token LP RP COMMA EQ DOT PIPE DOLLAR CARET ARROW END EOF
%token PLUS MINUS TIMES DIV GT LT GE LE NE EQUAL APPROX AND OR NOT COLON MOD TILDE
%right TYPEARROW
%nonassoc QUESTION
%right ELSE ARROW
%left PIPE
%nonassoc TILDE
%left OR
%left AND
%nonassoc NOT
%left GT LT GE LE NE EQUAL APPROX IN
%right FALLBACK
%nonassoc COLON CLOSED_CLOSED CLOSED_OPEN OPEN_CLOSED OPEN_OPEN
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UNARY
%right CARET
%left LP DOT LB
%start program
%type <Syntax.item list> program
%%
between: CLOSED_CLOSED { true, true } | CLOSED_OPEN { true, false } | OPEN_CLOSED { false, true } | OPEN_OPEN { false, false };
from: CLOSED_FROM { true } | OPEN_FROM { false };
program: items EOF { List.rev $1 };
items: /* empty */ { [] } | items separator { $1 } | items decl separator { { declaration = $2; span = $startpos($2), $endpos($2) } :: $1 };
separator: END {};
decl: IMPORT ident { Import ($2,$2) } | IMPORT ident AS ident { Import ($2,$4) }
 | TYPE ident type_params EQ annotation { Alias ($2,$3,$5) }
 | TYPE ident type_params EQ variants { Type ($2,$3,List.rev $5) }
 | USE CONTRACT STRING AS ident { Contract ($3,$5) }
 | SET ident EQ ident { Setting ($2,$4) }
 | LET ident COLON annotation EQ expr { Let ($2,None,at $startpos $endpos (Annotated ($6,$4))) }
 | LET ident EQ expr { Let ($2,None,$4) }
 | LET ident LP params RP return_type EQ expr { Let ($2,Some (List.map fst $4),parameters $4 (result_annotation $6 $8)) }
 | expr { Evaluate $1 };
type_params: /* empty */ { [] } | LT names GT { List.rev $2 };
annotation: ident { Named ($1,[]) } | ident LT annotations GT { Named ($1,List.rev $3) }
 | annotation QUESTION { Optional $1 }
 | annotation TYPEARROW annotation { Aarrow ($1,$3) }
 | LP annotations RP { match List.rev $2 with [t] -> t | ts -> Atuple ts }
 | LB annotation RB { Alist $2 }
 | LC RC { Arow ([],None) }
 | LC typed_fields RC { Arow (List.rev $2,None) }
 | LC DOTDOT ident RC { Arow ([],Some $3) }
 | LC typed_fields COMMA DOTDOT ident RC { Arow (List.rev $2,Some $5) };
annotations: annotation { [$1] } | annotations COMMA annotation { $3 :: $1 };
variants: BAR variant { [$2] } | variants BAR variant { $3 :: $1 };
variant: ident { $1,[] } | ident LP typed_fields RP { $1,List.rev $3 };
typed_fields: ident COLON annotation { [$1,$3] } | typed_fields COMMA ident COLON annotation { ($3,$5) :: $1 };
arms: arm { [$1] } | arms COMMA arm { $3 :: $1 };
arm: patterns ARROW expr { { pattern = product (List.rev $1); guard = None; body = $3 } }
 | patterns IF expr ARROW expr { { pattern = product (List.rev $1); guard = Some $3; body = $5 } };
pattern: alternative { $1 } | pattern BAR alternative { alternatives $1 $3 };
alternative: ident { if $1="_" then Wildcard else if Char.uppercase_ascii $1.[0]=$1.[0] then Tag ($1,[]) else Bind $1 }
 | ident AT alternative { Pas ($1,$3) }
 | ident LP patterns RP { Tag ($1,List.rev $3) }
 | LC names RC { Precord (List.rev $2) }
 | LC names COMMA DOTDOT RC { Precord (List.rev $2) }
 | literal { Pliteral $1 }
 | MINUS bound { Pliteral (negative $2) }
 | signed between signed { Prange { low = Some { value = $1; closed = fst $2 }; high = Some { value = $3; closed = snd $2 } } }
 | DOTDOTEQ signed { Prange { low = None; high = Some { value = $2; closed = true } } }
 | DOTDOTLT signed { Prange { low = None; high = Some { value = $2; closed = false } } }
 | signed from { Prange { low = Some { value = $1; closed = $2 }; high = None } }
 | LP pattern COMMA patterns RP { Ptuple ($2 :: List.rev $4) };
bound: INT { Int $1 } | FLOAT { Float $1 };
signed: bound { $1 } | MINUS bound { negative $2 };
patterns: pattern { [$1] } | patterns COMMA pattern { $3 :: $1 };
literal: INT { Int $1 } | FLOAT { Float $1 } | STRING { Text $1 } | DATE { Date $1 }
 | TRUE { Bool true } | FALSE { Bool false } | NONE { Absent };
return_type: /* empty */ { None } | COLON annotation { Some $2 };
param: ident { $1,None } | ident COLON annotation { $1,Some $3 };
params: /* empty */ { [] } | param_list { List.rev $1 };
param_list: param { [$1] } | param_list COMMA param { $3 :: $1 };
names: ident { [$1] } | names COMMA ident { $3 :: $1 };
args: /* empty */ { [] } | arguments { List.rev $1 } | arguments COMMA { List.rev $1 };
arguments: arg { [$1] } | arguments COMMA arg { $3 :: $1 };
arg: expr { None,$1 } | NAME EQ expr { Some (Plain $1),$3 } | QUOTED EQ expr { Some (Quoted $1),$3 };
values: expr { [$1] } | values COMMA expr { $3 :: $1 };
fields: field { [$1] } | fields COMMA field { $3 :: $1 };
field: ident EQ expr { $1,$3 } | ident { $1,at $startpos $endpos (Name (Bare,$1)) };
rest: expr { $1 } | expr separator { $1 } | block { $1 };
block: LET ident EQ expr separator rest { at $startpos $endpos (Local ($2,$4,$6)) }
 | LET ident LP params RP EQ expr separator rest
   { at $startpos $endpos (Local ($2,at $startpos($7) $endpos($7) (Lambda (List.map fst $4,parameters $4 $7)),$9)) };
expr: MATCH_PRESENT expr LC arms RC { at $startpos $endpos (Match_present ($2,List.rev $4)) }
 | MATCH_PRESENT expr LC arms COMMA RC { at $startpos $endpos (Match_present ($2,List.rev $4)) }
 | MATCH values LC arms RC { at $startpos $endpos (Match (subject $startpos($2) $endpos($2) (List.rev $2),List.rev $4)) }
 | MATCH values LC arms COMMA RC { at $startpos $endpos (Match (subject $startpos($2) $endpos($2) (List.rev $2),List.rev $4)) }
 | LB RB { at $startpos $endpos (List []) }
 | LB values RB { at $startpos $endpos (List (List.rev $2)) }
 | LB values COMMA RB { at $startpos $endpos (List (List.rev $2)) }
 | LC RC { at $startpos $endpos (Record []) }
 | LC fields RC { at $startpos $endpos (Record (List.rev $2)) }
 | LC fields COMMA RC { at $startpos $endpos (Record (List.rev $2)) }
 | LC block RC { $2 }
 | LP expr COMMA values RP { at $startpos $endpos (Tuple ($2 :: List.rev $4)) }
 | ident { at $startpos $endpos (Name (Bare,$1)) } | DOLLAR ident { at $startpos $endpos (Name (Column,$2)) }
 | CARET ident { at $startpos $endpos (Name (Lexical,$2)) } | literal { at $startpos $endpos (Literal $1) }
 | LP expr RP { $2 } | expr DOT ident { at $startpos $endpos (Field ($1,$3)) }
 | expr LP args RP {
     let callee = match $1.form with
       | Name (r, n) -> { $1 with form = Name (r, american n) }
       | _ -> $1 in
     at $startpos $endpos (Call (callee,$3)) }
 | expr LB expr RB { at $startpos $endpos (Index ($1,Element $3)) }
 | expr LB expr from RB { at $startpos $endpos (Index ($1,From ($3,$4))) }
 | expr LB DOTDOTEQ expr RB { at $startpos $endpos (Index ($1,Upto ($4,true))) }
 | expr LB DOTDOTLT expr RB { at $startpos $endpos (Index ($1,Upto ($4,false))) }
 | expr CLOSED_CLOSED expr { at $startpos $endpos (Sequence { first = $1; first_closed = true; last = $3; last_closed = true }) }
 | expr CLOSED_OPEN expr { at $startpos $endpos (Sequence { first = $1; first_closed = true; last = $3; last_closed = false }) }
 | expr OPEN_CLOSED expr { at $startpos $endpos (Sequence { first = $1; first_closed = false; last = $3; last_closed = true }) }
 | expr OPEN_OPEN expr { at $startpos $endpos (Sequence { first = $1; first_closed = false; last = $3; last_closed = false }) }
 | LAST { at $startpos $endpos Last }
 | TAP LP args RP
   { let tap = match List.assoc_opt $1 taps with
       | Some tap -> tap
       | None ->
         Diagnostic.fail ~span:($startpos, $endpos) Syntax_error
           ("Unknown inspection !" ^ $1 ^ ". Use " ^ String.concat ", " (List.map (fun (n, _) -> "!" ^ n) taps))
     in
     at $startpos $endpos (Tap (tap,$3)) }
 | IF LP expr RP expr ELSE expr { at $startpos $endpos (If ($3,$5,$7)) }
 | FN LP params RP ARROW expr { at $startpos $endpos (Lambda (List.map fst $3,parameters $3 $6)) }
 | MINUS expr %prec UNARY
   { (match $2.form with Binary (Power,_,_) when $startpos($2) = fst $2.span -> Diagnostic.fail Syntax_error "Write -(x ^ y) or (-x) ^ y" | _ -> ());
     at $startpos $endpos (Unary (Negate,$2)) }
 | NOT expr { at $startpos $endpos (Unary (Not,$2)) }
 | expr PLUS expr { at $startpos $endpos (Binary (Add,$1,$3)) }
 | expr MINUS expr { at $startpos $endpos (Binary (Sub,$1,$3)) }
 | expr TIMES expr { at $startpos $endpos (Binary (Mul,$1,$3)) }
 | expr DIV expr { at $startpos $endpos (Binary (Div,$1,$3)) }
 | expr MOD expr { at $startpos $endpos (Binary (Mod,$1,$3)) }
 | expr CARET expr { at $startpos $endpos (Binary (Power,$1,$3)) }
 | expr GT expr { compare $startpos $endpos Greater $1 $3 }
 | expr LT expr { compare $startpos $endpos Less $1 $3 }
 | expr GE expr { compare $startpos $endpos Greater_equal $1 $3 }
 | expr LE expr { compare $startpos $endpos Less_equal $1 $3 }
 | expr EQUAL expr { compare $startpos $endpos Equal $1 $3 }
 | expr NE expr { compare $startpos $endpos Not_equal $1 $3 }
 | expr APPROX expr { compare $startpos $endpos Approx $1 $3 }
 | expr FALLBACK expr { at $startpos $endpos (Fallback ($1,$3)) }
 | expr NOT IN expr %prec IN
   { at $startpos $endpos (Unary (Not, at $startpos $endpos (Member ($1,$4)))) }
 | expr IN expr
   { (match $1.form with
      | Binary ((Less | Less_equal | Greater | Greater_equal | Equal | Not_equal | Approx), _, _) | Member _ | Chain _
        when bare $startpos $1 -> Diagnostic.fail ~span:($startpos, $endpos) Syntax_error "Membership does not chain"
      | _ -> ());
     at $startpos $endpos (Member ($1,$3)) }
 | expr PIPE expr { at $startpos $endpos (Pipe ($1,$3)) }
 | expr TILDE expr { at $startpos $endpos (Formula (Some $1,$3)) }
 | TILDE expr %prec TILDE { at $startpos $endpos (Formula (None,$2)) }
 | expr AND expr { at $startpos $endpos (Binary (And,$1,$3)) }
 | expr OR expr { at $startpos $endpos (Binary (Or,$1,$3)) }
 | expr COLON expr { at $startpos $endpos (Range ($1,$3)) }
;
ident: NAME { $1 } | QUOTED { $1 };
