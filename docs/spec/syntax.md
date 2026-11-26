# Syntax

## Lexical rules

- Source files are UTF-8 with the extension `.rp`.
- `#` starts a comment that runs to the end of the line.
- Whitespace separates tokens and is otherwise insignificant.
- Identifiers are Unicode XID with `_`. Types, constructors and type
  constants start uppercase; values and functions start lowercase. Backticks
  quote any other name: `` `gross revenue` ``. A backtick inside is doubled.
- Keywords: `let in fn if else match type new deriving use contract import
  as and or not TRUE FALSE None end todo`.
- `todo` is a typed hole ([Agents](agent-interface.md#holes)).
- A top-level item ends at a newline once complete, or at `;`. A line ending
  in an operator, `=`, `=>`, `,` or an open bracket continues. A line followed
  by one starting with `|>`, `else` or a closing bracket continues.

## Literals

| Form | Type |
| --- | --- |
| `42`, `1_000_000` | `Int` |
| `3.5`, `1e-3`, `2.0` | `Float` |
| `TRUE`, `FALSE` | `Bool` |
| `None` | `T?` |
| `"text {expr}"` | `String` |
| `raw"C:\data"` | `String` |
| `d"2026-11-24"` | `Date` |
| `t"09:30"` | `Time` |
| `t"2026-11-24 09:30 America/Los_Angeles"` | `DateTime<"America/Los_Angeles">` |
| `r"^[A-Z]{3}\d+$"` | `Regex` |
| `[1, 2, 3]` | `[Int]` |
| `{site = "MRS", n = 3}`, `{site, n}` | `{site: String, n: Int}` |

- `1` is `Int`; `1.0` is `Float`. A literal adapts to an expected numeric
  type only when exactly representable. Defaulting happens after inference;
  unsolved numeric literal constraints generalise with functions. Underscores
  may occur singly between any two digits, including fraction and exponent
  digits; no fixed grouping width is required. They are ignored numerically.
- Date, time and regex literals are validated during checking.
- Strings interpolate `{expr}`, `{expr:spec}` and `{expr=}`. `{expr}` inserts
  `String(expr)`, the rupam display format ([Conversions](types.md#conversions)),
  and requires a `showable` type. `{expr=}` inserts the expression's source
  text, `=`, then its display. Specs follow Python's format mini-language for
  numbers (fill, alignment, sign, width, `,` grouping, precision and the
  types `d`, `f`, `e`, `g` and `%`) and strftime for dates; a spec of the wrong
  type is a type error. Interpolation lifts: a string with an optional part
  is optional and missing where that part is missing. `{{` and `}}` are
  literal braces.
- `"""` strings span lines and strip common indentation. `raw"..."` strings do
  not interpolate or process escapes.

## Grammar

```ebnf
program       = { item , end } [ item ] ;
item          = declaration | expr ;
end           = newline | ";" ;

declaration   = "use" "contract" string "as" ident
              | "set" setting "=" ident
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
              | "match" expr { "," expr } "{" arm { "," arm } [ "," ] "}"
              | "match?" expr "{" arm { "," arm } [ "," ] "}"
              | "let" binding "in" expr
              | "let" pattern "=" expr "else" expr "in" expr
              | block
              | pipeline ;
arm           = pattern { "," pattern } [ "if" expr ] "=>" expr ;
block         = "{" "let" binding end { "let" binding end } expr "}" ;

pipeline      = formula { "|>" formula } ;
formula       = [ disjunct ] "~" disjunct | disjunct ;
disjunct      = conjunct { "or" conjunct } ;
conjunct      = boolean_not { "and" boolean_not } ;
boolean_not   = "not" boolean_not | comparison ;
comparison    = fallback [ compare_op fallback ] ;
compare_op    = "==" | "!=" | "~=" | "≈" | "<" | "<=" | ">" | ">=" | "in" | "not" "in" ;
fallback      = range [ "??" fallback ] ;
range         = sum [ ( "..=" | "..<" | ":" ) sum ] ;
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
index         = expr | "end" | [ index ] ( "..=" | "..<" ) index | index ".." ;

primary       = ident | Ident | "$" ident | "^" ident | literal
              | "(" expr ")"
              | "[" [ expr { "," expr } [ "," ] ] "]"
              | "{" [ field { "," field } [ "," ] ] "}" ;
field         = ident [ "=" expr ] ;

pattern       = alternative { "|" alternative } ;
alternative   = "_" | ident [ "@" alternative ] | literal | signed_number
              | signed_number ( "..=" | "..<" ) signed_number
              | ( "..=" | "..<" ) signed_number | signed_number ".."
              | Ident [ "(" pattern { "," pattern } ")" ]
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
  Nested options flatten. `Float? > 0` refines present values.
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
- A call with `_` arguments is a function of those holes, left to right:
  `round(_, 2)` is `fn(x) => round(x, 2)`. Functions do not curry
  implicitly; `_` is the only partial application.
- `x |> f` applies `f` to `x`. In `x |> f(a)` the piped value is the first
  argument, and `x |> f(a, _)` fills the hole, so both are ordinary
  application. The piped expression is evaluated once.
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
6. `..=` `..<`, and `:` in column selectors
7. `??`, right-associative
8. comparisons, `~=`, `in` and `not in` (non-associative)
9. `not`
10. `and`
11. `or`
12. `~` (reserved)
13. `|>`
14. `fn`, `if`, `match`, `let ... in` (extend right)

`-x^y`, `a < b < c` and `a ~ b ~ c` are parse errors. The parser rejects
an unparenthesised power directly under unary minus even though the EBNF
recognises its token sequence; write `-(x^y)` or `(-x)^y`. `not x > 0` means
`not (x > 0)`. `revenue ?? 0.0 > 100.0` means `(revenue ?? 0.0) > 100.0`;
`a ?? b ?? c` means `a ?? (b ?? c)`. `x not in ys` means `not (x in ys)`.
`||` and `%in%` are not operators. End-of-file terminates a complete top-level item.

## Ranges and indexing

- `a..=b` is the Int vector from `a` to `b` inclusive; `a..<b` excludes
  `b`. There is no bare `a..b`. Ranges never count down: `5..=1` is empty, and
  `1..=5 |> rev()` is `[5, 4, 3, 2, 1]`. `1..=n-1` is `1..=(n-1)`.
- Open ranges (`a..`, `..=b`, `..<b`) appear only in patterns and indexing.
- In a column selector, `a:b` is the columns from `a` to `b` in schema order,
  as in dplyr. Elsewhere `a:b` is an `r-range` error with the fix `a..=b`.
- Inside a string slot, `:` always starts a format spec.
- Vectors index from 1. `x[end]` is the last element. `x[2..]`, `x[..<4]`
  and `x[2..=end]` are slices. An index or slice outside the vector is an
  error. Tables have no positional indexing.

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

- `match a, b { p, q => e }` matches several subjects at once; each arm has
  one pattern per subject.
- Patterns: `_`, names, literals except floats, ranges (`a..=b` inclusive,
  `a..<b` half-open, `..<b`, `..=b`, `a..` meaning `a` and above). A lower
  bound is always inclusive; adjacent half-open arms such as `..<18`,
  `18..<65`, `65..` cover a domain without gaps or overlap, constructors,
  `Some(v)` and `None` for options, vector heads with `..rest`
  (including `[..rest]`), records with optional `..`,
  alternatives, `@` bindings, guards.
- A binding or parameter without a fallback must have an irrefutable pattern
  for its inferred type.
- `let pattern = e else fallback in body` is `fallback` when the pattern does
  not match.
- `match? x { ... }` matches the present payload of one optional subject
  and is `None` when `x` is `None`; a branch that is itself optional does not
  nest. Ordinary `match` handles options explicitly.
- A `match` must be exhaustive over the subject's established domain: its
  type narrowed by refinements, validated contract bounds and flow facts (see
  [Types](types.md#refinement-aware-exhaustiveness)). Ranges that together cover every number are
  exhaustive. Signed numeric range endpoints are permitted; a standalone
  floating literal is not a pattern. Guards do not count toward exhaustiveness.

## Settings

`set missing = skip` and `set group_missing = keep` are lexical declarations
of analytical defaults, not bindings or mutable options. They apply to
subsequent items in the file, are captured by definitions that follow them
and are preserved by `:save`. `use` is reserved for contracts.

## Formatting

Consecutive `set` declarations form one block separated from adjacent
imports and bindings by a blank line; declarations are never reordered.

`rupam fmt` is canonical, idempotent and preserves comments and valid numeric
separator groupings. It preserves backtick quoting wherever removing it would
change argument classification (e.g. `` `missing` = none(x) `` in `mutate`).
