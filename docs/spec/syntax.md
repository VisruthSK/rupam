# Syntax

## Lexical rules

Source files are UTF-8 with the extension `.rp`. Comments run from `#` to the
end of the line. Whitespace separates tokens and has no other meaning; there
is no indentation-sensitive syntax.

Identifiers start with a Unicode letter or `_` and continue with letters,
digits or `_` (Unicode XID). Names of types, constructors and type-level
constants start with an uppercase letter; values and functions start with a
lowercase letter. Backticks quote any other name, which is how columns such as
`` `gross revenue` `` are written; a backtick inside is doubled.

Keywords: `let in fn if else match type new deriving use contract import as
test and or not TRUE FALSE None end`.

A top-level item ends at a newline once it is complete, or at `;`. A line that
ends in an operator, `=`, `=>`, `,` or an open bracket continues on the next
line, and so does a line followed by one that starts with `|>`, `else` or a
closing bracket.

## Literals

| Literal | Type | Example |
| --- | --- | --- |
| integer | `Int64` | `42`, `1_000_000` |
| float | `Float64` | `3.5`, `1e-3`, `2.0` |
| boolean | `Bool` | `TRUE`, `FALSE` |
| missing | `T?` | `None` |
| string | `String` | `"total: {total}"` |
| raw string | `String` | `raw"C:\data\visits.csv"` |
| date | `Date` | `d"2026-11-24"` |
| time of day | `Time` | `t"09:30"` |
| date-time | `DateTime<Z>` | `t"2026-11-24 09:30 America/Los_Angeles"` |
| regex | `Regex` | `r"^[A-Z]{3}\d+$"` |
| vector | `[T]` | `[1, 2, 3]` |
| tuple | `(A, B)` | `(1, "a")` |
| record | `{a: A, b: B}` | `{site = "MRS", n = 3}`, `{site, n}` |

`1` is always `Int64` and `1.0` is always `Float64`. A literal whose type is
fixed by its context adapts only when the value is exactly representable
(`revenue > 0` compares a `Float64` column with `0.0`).

Date, time, date-time and regex literals are checked when the program is
checked: `d"2026-02-30"` and `r"[a-"` are compile errors.

### Strings

Double-quoted strings interpolate any expression in braces and are
type-checked:

```text
"{n} visits across {n_distinct(site)} sites"
"mean z {mean_z:.2}"          # format spec after ':'
"{total=}"                    # prints total=1234.5
"use {{braces}} literally"
```

The format spec follows Python's format mini-language for numbers (width,
precision, `,` grouping, `%`, `e`) and strftime patterns for dates
(`{when:%Y-%m-%d}`). Applying a spec to a value of the wrong type is a type
error.

Triple-quoted strings span lines and strip their common indentation. Raw
strings (`raw"..."`) neither interpolate nor process escapes.

## Grammar

```ebnf
program      = { item } ;
item         = declaration | expr , end ;
end          = newline | ";" ;

declaration  = "use" "contract" string "as" ident
             | "use" setting "=" ident                 (* use missing = skip *)
             | "import" ident [ "as" ident ]           (* deferred *)
             | "type" Ident [ type_params ] "=" type_body
             | "let" binding
             | "test" string block ;                   (* experimental *)

setting      = "missing" | "float_equality" ;
type_body    = type
             | "new" type [ "deriving" "(" ident { "," ident } ")" ]
             | "|" variant { "|" variant } ;
variant      = Ident [ "(" field_type { "," field_type } ")" ] ;
field_type   = ident ":" type ;
type_params  = "<" ident { "," ident } ">" ;

binding      = ident [ type_params ] "(" [ param { "," param } ] ")"
               [ ":" type ] "=" expr
             | pattern [ ":" type ] "=" expr ;
param        = pattern [ ":" type ] [ "=" expr ] ;

expr         = "fn" "(" [ param { "," param } ] ")" "=>" expr
             | "if" "(" expr ")" expr "else" expr
             | "match" expr "{" arm { "," arm } [ "," ] "}"
             | "let" binding "in" expr
             | "let" pattern "=" expr "else" expr "in" expr
             | block
             | pipeline ;
arm          = pattern [ "if" expr ] "=>" expr ;
block        = "{" "let" binding end { "let" binding end } expr "}" ;

pipeline     = formula { "|>" formula } ;
formula      = [ disjunct ] "~" disjunct | disjunct ;
disjunct     = conjunct { "or" conjunct } ;
conjunct     = comparison { "and" comparison } ;
comparison   = range [ compare_op range ] ;
compare_op   = "==" | "!=" | "~=" | "≈" | "<" | "<=" | ">" | ">=" | "%in%" ;
range        = sum [ ":" sum ] ;
sum          = product { ( "+" | "-" ) product } ;
product      = power { ( "*" | "/" | "%" ) power } ;
power        = negation [ "^" negation ] ;
negation     = [ "-" | "not" ] postfix ;
postfix      = primary { "(" [ args ] ")" | "." ident | "[" index "]" } ;
args         = arg { "," arg } [ "," ] ;
arg          = ident "=" expr | expr | "_" ;
index        = expr | "end" | range_index ;

primary      = ident | "$" ident | "^" ident | literal
             | "(" expr ")"
             | "(" expr "," expr { "," expr } [ "," ] ")"
             | "[" [ expr { "," expr } [ "," ] ] "]"
             | "{" [ field { "," field } [ "," ] ] "}" ;
field        = ident [ "=" expr ] ;

pattern      = alternative { "|" alternative } ;
alternative  = "_" | ident [ "@" alternative ] | literal
             | literal "..=" literal | "..=" literal | literal ".."
             | Ident [ "(" pattern { "," pattern } ")" ]
             | "(" pattern "," pattern { "," pattern } ")"
             | "[" [ pattern { "," pattern } ] [ "," ".." [ ident ] ] "]"
             | "{" field_pattern { "," field_pattern } [ "," ".." ] "}" ;
field_pattern = ident [ "=" pattern ] ;

type         = optional_type [ "->" type ] ;
optional_type = atom_type [ "?" ] ;
atom_type    = Ident [ "<" type_arg { "," type_arg } ">" ] | ident
             | "[" type "]"
             | "{" [ field_type { "," field_type } ] [ "," ".." ident ] "}"
             | "(" type { "," type } ")" ;
type_arg     = type | string ;                         (* DateTime<"UTC"> *)
```

`T?` and `Option<T>` are the same type. `rupam fmt` writes `T?`.

A record literal and a block are both braces. A block always starts with
`let`; anything else in braces is a record. `{x, y}` puns to
`{x = x, y = y}`.

`_` as an argument marks where a piped value goes: `x |> lm(y ~ z, data = _)`.
Without a placeholder, `x |> f(a)` means `f(x, a)` and `x |> f` means `f(x)`.
The piped expression is evaluated once.

## Precedence

From tightest to loosest:

1. calls, field access and indexing;
2. `^` (right-associative);
3. unary `-` and `not`;
4. `*`, `/`, `%`;
5. `+`, `-`;
6. `:`;
7. comparisons, `~=` and `%in%`, which do not chain;
8. `and`;
9. `or`;
10. `~`;
11. `|>`;
12. `fn`, `if`, `match`, `let ... in`, which extend as far right as possible.

`-x^y` is a parse error: write `-(x^y)` or `(-x)^y`. `1:n-1` means `1:(n-1)`.
`a < b < c` is a parse error: write `a < b and b < c`.

`^` between two operands is exponentiation. `^name` before an identifier
refers to an outer binding inside a verb (see [Tables](tables.md)). The two
positions never overlap.

## Ranges

`a:b` is an inclusive range. Between integers it is a vector of `Int64`. In a
column-selection position it names the columns from `a` to `b` in schema
order. A range never counts down: `5:1` and `1:0` are empty. A descending
sequence is written with `seq(5, 1, by = -1)`.

## Indexing

Vectors index from 1. `x[1]` is the first element and `x[end]` the last.
`x[2:end]` drops the first element. An index outside the vector is a runtime
error. Tables cannot be indexed by position; use their verbs.

## Patterns

```text
match age {
  0..=17 => Child,
  18..=64 => Adult,
  _ => Senior,
}

match reading {
  Measured(v) if v < 0.0 => Invalid,
  Measured(v @ 0.0..=1.0) => Calibrated(v),
  Measured(v) => Raw(v),
  Offline | Fault(_) => Unavailable,
}

let [first, ..rest] = values in ...
let {site, n} = summary_row in ...
let Some(v) = reading else None in log(v)
```

Patterns cover `_`, names, literals, inclusive ranges, constructors, tuples,
vector heads with `..rest`, records with optional `..`, alternatives with `|`,
`@` bindings and guards. Float literals are not patterns (float equality is
not exact; see [Types](types.md)); match floats with ranges or guards.

`let pattern = expr else fallback in body` evaluates to `fallback` when the
pattern does not match.

Every `match` must be exhaustive. The checker reports one concrete missing
case. Guards do not count towards exhaustiveness.

## Formatting

`rupam fmt` produces one canonical layout, preserves comments and is
idempotent.
