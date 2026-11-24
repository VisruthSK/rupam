# The REPL

```sh
rupam                          # empty session
rupam repl examples/shop.rp    # session with a program loaded
```

Type a definition or an expression and rupam prints its type:

```text
rupam> let f(x) = x * 2
f : (Int64) -> Int64
rupam> f(21)
42 : Int64
```

Arrow keys walk the history, which lives in `rupam/history` under
`$XDG_STATE_HOME`, `~/.local/state` or, on Windows, `%LOCALAPPDATA%`. Tab
completes commands, your bindings and keywords from a menu under the prompt.
Ctrl-C abandons the current line, Ctrl-D leaves, and Ctrl-L clears the screen.

## Commands

| Command | |
| --- | --- |
| `:type EXPR` | inferred type, or the schema for a table expression; runs nothing |
| `:schema TABLE` | fields, types, nullability and contract status |
| `:preview TABLE` | the first 20 rows and 8 columns, after validation |
| `:inspect TABLE` | the same, with `n`/`p` for rows and `l`/`h` for columns |
| `:explain TABLE` | the plan, including validation and any callback fallbacks |
| `:contract TABLE` | which contract and table a value comes from |
| `:validate TABLE` | runs data-dict validation now |
| `:code DIALECT TABLE` | the generated script for one table |
| `:target r-dplyr`, `:target python-polars` | the dialect behind `:preview` and `:inspect`; starts from `rupam.toml` or `--target` |
| `:bindings` | everything defined so far |
| `:reload` | re-reads the file you started with |
| `:help` | this list |

`TABLE` can be any table expression, not only a name:
`:schema orders |> filter(revenue > 10.0) |> select(region)`.

## Unfinished input

When a line is incomplete (an open parenthesis, a trailing `|>`), the prompt
changes to `...` and waits for more. An empty line abandons it. Typing
`:prefix` at the `...` prompt checks the longest complete part of what you have
so far and prints its schema, so you can see what the next stage will receive
before you finish writing it:

```text
rupam> let t = orders |> filter(revenue > 0.0) |> mutate(z = standardize(
  ... :prefix
let t = orders |> filter(revenue > 0.0)
customer_id : Int64
region : String
...
```

A definition that fails to check leaves the session as it was. Earlier
bindings stay usable.

## Editors

`rupam lsp` speaks the Language Server Protocol over stdio. It reports parse
and type errors as you type and completes top-level names.
