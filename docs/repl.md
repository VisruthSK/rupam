# The REPL

```sh
rupam repl                     # uses the dialect from rupam.toml
rupam repl shop.rp             # loads a program first
rupam repl --target r-dplyr    # overrides rupam.toml
```

The REPL needs a dialect, from `[target] dialect` in `rupam.toml` or from
`--target`. Everything you type is checked by rupam, compiled to that dialect,
and run in a persistent R or Python session, so what you see is what the
generated script computes.

Type a definition or an expression. rupam prints the value with its type:

```text
rupam ❯ let f(x) = x ^ 2
f : (Int) -> Int
rupam ❯ f(10)
100 : Int
```

Type a name to look at it. A table prints its first rows, a contract lists its
tables and columns, and a function shows its parameters:

```text
rupam ❯ orders
rupam ❯ orders |> filter(cost > 1.0, missing = skip) |> print()
rupam ❯ shop
```

Table previews show the checked type below each column name. Float cells
use two decimal places by default. To change them for a project:

```toml
[preview]
digits = 4
```

## Completion

Tab completes from what the cursor is in, and a second Tab cycles through the
menu:

| Where | Completes |
| --- | --- |
| `:` | commands |
| `:target ` | dialects |
| `:schema `, `:preview `, ... | tables and contracts |
| `orders \|> ` | verbs and your table functions |
| `orders \|> filter(` | the table's columns with their types, the verb's options, functions |
| `filter(x, missing = ` | `skip`, `propagate`, `error` |
| `left_join(..., relationship = ` | `one_to_one`, `many_to_one`, `one_to_many`, `many_to_many` |
| `shop.` | the contract's tables |
| `read_csv("` | files next to the program |

Arrow keys walk the history, which lives in `rupam/history` under
`$XDG_STATE_HOME`, `~/.local/state` or, on Windows, `%LOCALAPPDATA%`.
Ctrl+L clears the screen and Ctrl+D leaves.

## Commands

| Command | |
| --- | --- |
| `:options` | effective settings and their sources |
| `:type EXPR` | type, or the schema of a table expression; runs nothing |
| `:schema [TABLE]` | columns and types; every table when no name is given |
| `:preview TABLE` | the first 20 rows, after validation |
| `:explain TABLE` | the plan from source to result |
| `:contract TABLE` | which contract and table the rows come from |
| `:validate TABLE` | runs data-dict validation now |
| `:code [DIALECT] TABLE` | the generated code for one table |
| `:target [DIALECT]` | shows or switches the dialect |
| `:bindings` | everything defined so far |
| `:reload` | re-reads the file you started with |
| `:lock` | validates the loaded file's inputs and records them in `rupam.lock` |
| `:help` | this list |

`TABLE` can be any table expression, not only a name:
`:schema orders |> filter(revenue > 10.0) |> select(region)`.

An unfinished line (an open parenthesis, a trailing `|>`) continues on the
next one. A definition that fails to check leaves the session as it was.

## Editors (further LSP work deferred)

`rupam lsp` speaks the Language Server Protocol over stdio. It reports parse
and type errors as you type and completes top-level names.
