# Syntax

## Lexical rules

- Source files are UTF-8 with the extension `.rp`.
- `#` starts a comment that runs to the end of the line.
- Whitespace separates tokens and is otherwise insignificant.
- Identifiers are Unicode XID with `_`. Types, constructors and type
  constants start uppercase; values and functions start lowercase. Backticks
  quote any other name: `` `gross revenue` ``. A backtick inside is doubled.
- Keywords: `let in fn if else match type new deriving use contract import
  as and or not TRUE FALSE None end`.
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
  type only when exactly representable. Defaulting happens after inference;
  unsolved numeric literal constraints generalise with functions. Underscores
  may occur singly between any two digits, including fraction and exponent
  digits; no fixed grouping width is required. They are ignored numerically.
- Date, time and regex literals are validated during checking.
- Strings interpolate `{expr}`, `{expr:spec}` and `{expr=}`. Specs follow
  Python's format mini-language for numbers and strftime for dates; a spec of
  the wrong type is a type error. `{{` and `}}` are literal braces.
- `"""` strings span lines and strip common indentation. `raw"..."` strings do
  not interpolate or process escapes.

## Grammar

```ebnf
program       = { item , end } [ item ] ;
item          = declaration | expr ;
end           = newline | ";" ;

declaration   = "use" "contract" string "as" ident
              | "use" setting "=" ident
              | "import" ( "r" | "python" ) string "{" foreign_sig { end foreign_sig } [ end ] "}"
              | "import" ident [ "as" ident ]
              | "type" Ident [ type_params ] "=" type_body
              | "let" binding ;
foreign_sig   = ident "(" [ field_type { "," field_type } ] ")" ":" type ;
setting       = "missing" | "group_missing" ;
type_body     = type
              | "new" type [ "deriving" "(" ident { "," ident } ")" ]
              | "|" variant { "|" variant } ;
variant       = Ident [ "(" field_type { "," field_type } ")" | "=" stored_value ] ;
stored_value  = string | signed_number | "TRUE" | "FALSE" ;
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
conjunct      = boolean_not { "and" boolean_not } ;
boolean_not   = "not" boolean_not | comparison ;
comparison    = range [ compare_op range ] ;
compare_op    = "==" | "!=" | "~=" | "≈" | "<" | "<=" | ">" | ">=" | "%in%" ;
range         = sum [ ":" sum ] ;
sum           = product { ( "+" | "-" ) product } ;
product       = negation { ( "*" | "/" | "%" ) negation } ;
negation      = [ "-" ] power ;
power         = postfix [ "^" negation ] ;
postfix       = primary { "(" [ args ] ")" | "." ident | "[" index "]" } ;
args          = arg { "," arg } [ "," ] ;
arg           = arg_name "=" expr | expr | "_" ;
arg_name      = bare_ident | quoted_ident ;
(* Preserve quoting in named arguments until contextual argument
   classification; see Tables. *)
index         = expr | "end" ;

primary       = ident | Ident | "$" ident | "^" ident | literal
              | "(" expr ")"
              | "(" expr "," expr { "," expr } [ "," ] ")"
              | "[" [ expr { "," expr } [ "," ] ] "]"
              | "{" [ field { "," field } [ "," ] ] "}" ;
field         = ident [ "=" expr ] ;

pattern       = alternative { "|" alternative } ;
alternative   = "_" | ident [ "@" alternative ] | literal | signed_number
              | signed_number ( "..=" | "..<" ) signed_number
              | ( "..=" | "..<" ) signed_number | signed_number ".."
              | Ident [ "(" pattern { "," pattern } ")" ]
              | "(" pattern "," pattern { "," pattern } ")"
              | "[" [ pattern { "," pattern } [ "," ".." [ ident ] ]
                      | ".." [ ident ] ] "]"
              | "{" field_pattern { "," field_pattern } [ "," ".." ] "}" ;
field_pattern = ident [ "=" pattern ] ;
signed_number = [ "-" ] numeric_literal ;

type          = refined_type [ "->" type ] ;
refined_type  = optional_type [ ( ">" | ">=" | "!=" ) "0" ] ;
optional_type = atom_type [ "?" ] ;
atom_type     = Ident [ "<" type_arg { "," type_arg } ">" ] | ident
              | "[" type "]"
              | "{" [ field_type { "," field_type } [ "," ".." ident ] | ".." ident ] "}"
              | "(" type { "," type } ")" ;
type_arg      = type | string ;

(* literal, numeric_literal, string, ident, Ident and newline are lexical
   tokens defined by the lexical rules and literal forms above. bare_ident
   is an unquoted value identifier; quoted_ident is a backtick-quoted name.
   ident admits either, subject to the surrounding context. *)
```

- `T?` and `Option<T>` denote the same type; `rupam fmt` writes `T?`.
  Nested options flatten. `Float64? > 0` refines present values.
- In contextual type arguments (`schema =`, `types =`, `parse_enum(enum = ...)`),
  parse a type literal rather than a value expression. `Table` constructs
  from a vector of records. Constructor and type names resolve in the type
  and constructor namespace; ambiguous constructors require a type context.
  Backtick names can denote constructors in constructor contexts. A nullary
  enum constructor may specify a distinct stored scalar with `= "value"`
  or a numeric or Boolean literal. All enum stored values have one primitive
  type, are distinct, and default to constructor names when omitted.
- `import helpers` and formulas using `~` are reserved for deferred features;
  using them now is an unsupported-feature diagnostic. Foreign signatures
  are supported as described in [Dialects](dialects.md#foreign-imports).
- Braces starting with `let` are a block; other braces are a record. `{x, y}`
  means `{x = x, y = y}`.
- In `x |> f(a)` the piped value is the first argument; `_` places it
  elsewhere: `x |> f(a, _)`. `x |> f` means `f(x)`. The piped
  expression is evaluated once. A call admits one pipeline placeholder.
- Positional arguments precede named arguments in ordinary calls; names resolve
  to parameters exactly. Verbs with output declarations may interleave named
  outputs (including backtick-quoted output names) and `across(...)` in their
  documented output positions;
  configuration arguments do not participate in sequential output evaluation. Duplicate, unknown and missing required arguments are errors.
  Defaults are pure expressions in the definition scope, evaluated in
  parameter order and able to refer to earlier parameters. Destructured
  parameters are positional only. Explicit ordinary arguments evaluate once in source order. Table-expression arguments use
  the substitution rules in [Tables](tables.md#column-names).
- `A -> B -> C` is a right-associated function type; `(A, B) -> C` is a
  two-argument signature in function annotation context.
- `$name` is a column and `^name` an outer binding (see [Tables](tables.md)).
  `^` between operands is exponentiation.

## Precedence

Tightest first:

1. call, field access, indexing
2. `^`, right-associative
3. unary `-`
4. `*` `/` `%`
5. `+` `-`
6. `:`
7. comparisons, `~=`, `%in%` (non-associative)
8. `not`
9. `and`
10. `or`
11. `~` (reserved)
12. `|>`
13. `fn`, `if`, `match`, `let ... in` (extend right)

`-x^y`, `a < b < c` and `a ~ b ~ c` are parse errors. The parser rejects
an unparenthesised power directly under unary minus even though the EBNF
recognises its token sequence; write `-(x^y)` or `(-x)^y`. `not x > 0` means
`not (x > 0)`. End-of-file terminates a complete top-level item.

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
  `Some(v)` and `None` for options, tuples, vector heads with `..rest`
  (including `[..rest]`), records with optional `..`,
  alternatives, `@` bindings, guards.
- A binding or parameter without a fallback must have an irrefutable pattern
  for its inferred type.
- `let pattern = e else fallback in body` is `fallback` when the pattern does
  not match.
- A `match` must be exhaustive. Ranges that together cover every number are
  exhaustive. Signed numeric range endpoints are permitted; a standalone
  floating literal is not a pattern. Guards do not count toward exhaustiveness.

## Formatting

`rupam fmt` is canonical, idempotent and preserves comments and valid numeric
separator groupings. It preserves backtick quoting wherever removing it would
change argument classification (e.g. `` `missing` = none(x) `` in `mutate`).
