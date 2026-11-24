# Syntax

## Lexical rules

- Source files are UTF-8 with the extension `.rp`.
- `#` starts a comment that runs to the end of the line.
- Whitespace separates tokens and is otherwise insignificant.
- Identifiers are Unicode XID with `_`. Types, constructors and type
  constants start uppercase; values and functions start lowercase. Backticks
  quote any other name: `` `gross revenue` ``. A backtick inside is doubled.
- Keywords: `let in fn if else match type new deriving use contract import
  as test and or not TRUE FALSE None end`.
- A top-level item ends at a newline once complete, or at `;`. A line ending
  in an operator, `=`, `=>`, `,` or an open bracket continues. A line followed
  by one starting with `|>`, `else` or a closing bracket continues.

## Literals

| Form | Type |
| --- | --- |
| `42`, `1_000_000` | `Int64` |
| `3.5`, `1e-3`, `2.0` | `Float64` |
| `TRUE`, `FALSE` | `Bool` |
| `None` | `T?` |
| `"text {expr}"` | `String` |
| `raw"C:\data"` | `String` |
| `d"2026-11-24"` | `Date` |
| `t"09:30"` | `Time` |
| `t"2026-11-24 09:30 America/Los_Angeles"` | `DateTime<"America/Los_Angeles">` |
| `r"^[A-Z]{3}\d+$"` | `Regex` |
| `[1, 2, 3]` | `[Int64]` |
| `(1, "a")` | `(Int64, String)` |
| `{site = "MRS", n = 3}`, `{site, n}` | `{site: String, n: Int64}` |

- `1` is `Int64`; `1.0` is `Float64`. A literal adapts to an expected numeric
  type only when exactly representable.
- Date, time and regex literals are validated during checking.
- Strings interpolate `{expr}`, `{expr:spec}` and `{expr=}`. Specs follow
  Python's format mini-language for numbers and strftime for dates; a spec of
  the wrong type is a type error. `{{` and `}}` are literal braces.
- `"""` strings span lines and strip common indentation. `raw"..."` strings do
  not interpolate or process escapes.

## Grammar

```ebnf
program       = { item } ;
item          = declaration | expr , end ;
end           = newline | ";" ;

declaration   = "use" "contract" string "as" ident
              | "use" setting "=" ident
              | "import" ident [ "as" ident ]
              | "type" Ident [ type_params ] "=" type_body
              | "let" binding
              | "test" string block ;
setting       = "missing" | "float_equality" ;
type_body     = type
              | "new" type [ "deriving" "(" ident { "," ident } ")" ]
              | "|" variant { "|" variant } ;
variant       = Ident [ "(" field_type { "," field_type } ")" ] ;
field_type    = ident ":" type ;
type_params   = "<" ident { "," ident } ">" ;

binding       = ident [ type_params ] "(" [ param { "," param } ] ")"
                [ ":" type ] "=" expr
              | pattern [ ":" type ] "=" expr ;
param         = pattern [ ":" type ] [ "=" expr ] ;

expr          = "fn" "(" [ param { "," param } ] ")" "=>" expr
              | "if" "(" expr ")" expr "else" expr
              | "match" expr "{" arm { "," arm } [ "," ] "}"
              | "let" binding "in" expr
              | "let" pattern "=" expr "else" expr "in" expr
              | block
              | pipeline ;
arm           = pattern [ "if" expr ] "=>" expr ;
block         = "{" "let" binding end { "let" binding end } expr "}" ;

pipeline      = formula { "|>" formula } ;
formula       = [ disjunct ] "~" disjunct | disjunct ;
disjunct      = conjunct { "or" conjunct } ;
conjunct      = comparison { "and" comparison } ;
comparison    = range [ compare_op range ] ;
compare_op    = "==" | "!=" | "~=" | "≈" | "<" | "<=" | ">" | ">=" | "%in%" ;
range         = sum [ ":" sum ] ;
sum           = product { ( "+" | "-" ) product } ;
product       = negation { ( "*" | "/" | "%" ) negation } ;
negation      = [ "-" | "not" ] power ;
power         = postfix [ "^" negation ] ;
postfix       = primary { "(" [ args ] ")" | "." ident | "[" index "]" } ;
args          = arg { "," arg } [ "," ] ;
arg           = ident "=" expr | expr | "_" ;
index         = expr | "end" ;

primary       = ident | "$" ident | "^" ident | literal
              | "(" expr ")"
              | "(" expr "," expr { "," expr } [ "," ] ")"
              | "[" [ expr { "," expr } [ "," ] ] "]"
              | "{" [ field { "," field } [ "," ] ] "}" ;
field         = ident [ "=" expr ] ;

pattern       = alternative { "|" alternative } ;
alternative   = "_" | ident [ "@" alternative ] | literal
              | literal ( "..=" | "..<" ) literal | ( "..=" | "..<" ) literal | literal ".."
              | Ident [ "(" pattern { "," pattern } ")" ]
              | "(" pattern "," pattern { "," pattern } ")"
              | "[" [ pattern { "," pattern } ] [ "," ".." [ ident ] ] "]"
              | "{" field_pattern { "," field_pattern } [ "," ".." ] "}" ;
field_pattern = ident [ "=" pattern ] ;

type          = refined_type [ "->" type ] ;
refined_type  = optional_type [ ( ">" | ">=" | "!=" ) "0" ] ;
optional_type = atom_type [ "?" ] ;
atom_type     = Ident [ "<" type_arg { "," type_arg } ">" ] | ident
              | "[" type "]"
              | "{" [ field_type { "," field_type } ] [ "," ".." ident ] "}"
              | "(" type { "," type } ")" ;
type_arg      = type | string ;
```

- `T?` and `Option<T>` denote the same type; `rupam fmt` writes `T?`.
- Braces starting with `let` are a block; other braces are a record. `{x, y}`
  means `{x = x, y = y}`.
- In `x |> f(a)` the piped value is the first argument; `_` places it
  elsewhere: `x |> lm(y ~ z, data = _)`. `x |> f` means `f(x)`. The piped
  expression is evaluated once.
- `$name` is a column and `^name` an outer binding (see [Tables](tables.md)).
  `^` between operands is exponentiation.

## Precedence

Tightest first:

1. call, field access, indexing
2. `^`, right-associative
3. unary `-`, `not`
4. `*` `/` `%`
5. `+` `-`
6. `:`
7. comparisons, `~=`, `%in%` (non-associative)
8. `and`
9. `or`
10. `~`
11. `|>`
12. `fn`, `if`, `match`, `let ... in` (extend right)

`-x^y`, `a < b < c` and `a ~ b ~ c` are parse errors.

## Ranges and indexing

- `a:b` is inclusive. On integers it is an `[Int64]`; in a column selector it
  is the columns from `a` to `b` in schema order. `a:b` with `b < a` is empty.
- Vectors index from 1. `x[end]` is the last element. An index outside the
  vector is an error. Tables have no positional indexing.

## Patterns

```text
let group = match age {
  ..<18 => Child,
  18..<65 => Adult,
  65.. => Senior,
}

let band = match bmi {
  ..<18.5 => Under,
  18.5..<25.0 => Healthy,
  25.0..<30.0 => Over,
  30.0.. => Obese,
}

match reading {
  Measured(v) if v < 0.0 => Invalid,
  Measured(v @ 0.0..=1.0) => Calibrated(v),
  Measured(v) => Raw(v),
  Offline | Fault(_) => Unavailable,
}
```

- Patterns: `_`, names, literals except floats, ranges (`a..=b` inclusive,
  `a..<b` half-open, `..<b`, `a..`), constructors,
  tuples, vector heads with `..rest`, records with optional `..`,
  alternatives, `@` bindings, guards.
- `let pattern = e else fallback in body` is `fallback` when the pattern does
  not match.
- A `match` must be exhaustive. Ranges that together cover every number are
  exhaustive. Guards do not count toward exhaustiveness.

## Formatting

`rupam fmt` is canonical, idempotent and preserves comments.
