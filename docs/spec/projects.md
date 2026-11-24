# Projects

## rupam.toml

A project is a directory containing `rupam.toml`. A file outside any project
can still be checked and formatted.

```toml
[project]
name = "clinic"

[target]
dialect = "r-dplyr"

[check]
strict = true

[compile]
validate = true
```

| Setting | Meaning |
| --- | --- |
| `target.dialect` | the dialect for previews, `run` and `compile` |
| `check.strict` | `true` by default; `false` allows foreign imports and target-specific functions |
| `compile.validate` | whether generated scripts carry the contract preflight; `true` by default |

There is no default dialect. Commands that execute or compile take it from
`--target`, otherwise from `rupam.toml`, otherwise they stop and say how to
set it. Checking never needs a dialect.

`rupam.toml` never changes what a program means. Missing-value and
float-equality settings belong to the file (`use missing = skip`), so a file
means the same thing in every project.

## Environments

If the project directory contains `pyproject.toml` or `uv.lock`, Python runs
through `uv run`. If it contains `renv.lock`, R loads that renv library
first. Previews, `run`, `rupam test` and validation then use exactly the
package versions the project pins, which is what makes a preview and a later
run of the script load the same code. rupam uses the `uv` and R on the PATH
and never installs an interpreter.

## Command line

| Command | |
| --- | --- |
| `rupam` / `rupam repl [FILE]` | the REPL |
| `rupam run FILE` | compile, validate and execute |
| `rupam compile FILE [-o OUT] [--release]` | write the script, source map and manifest |
| `rupam check FILE` | parse and type-check only |
| `rupam fmt FILE` | canonical formatting |
| `rupam test` | run test blocks in every dialect (experimental) |
| `rupam schema FILE` | describe a contract, or print an inferred schema for a data file |
| `rupam explain CODE` | explain a diagnostic |
| `rupam lsp` | the language server |

`--target DIALECT` and `--no-strict` override the project settings.

## Distribution

Releases are self-contained binaries for Linux and macOS (x86-64 and ARM) and
Windows (x86-64). DuckDB is linked statically. Running generated scripts needs
the dialect's language and packages, plus the data-dict CLI when a contract
is used.
