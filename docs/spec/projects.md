# Projects

## rupam.toml

A directory containing `rupam.toml` is a project.

```toml
[target]
dialect = "r-dplyr"

[check]
strict = true
```

- `target.dialect`: `r-dplyr` or `python-polars`. There is no default; a
  command that runs or compiles without a dialect from `--target` or
  `rupam.toml` fails.
- `check.strict`: defaults to `true`.

Nothing else is configurable. Settings that change a program's meaning are
declared in the program (`use missing`, `use float_equality`).

## Environments

- `pyproject.toml` or `uv.lock` in the project: Python runs through
  `uv run --project`.
- `renv.lock` in the project: R loads the project's renv library.
- Otherwise the interpreters on the PATH are used.

Previews, `run`, tests and validation use the same environment.

## Command line

| Command | |
| --- | --- |
| `rupam`, `rupam repl [FILE]` | REPL |
| `rupam run FILE` | compile, validate, execute |
| `rupam compile FILE [-o OUT] [--release]` | script, source map, manifest |
| `rupam check FILE` | parse and type-check |
| `rupam fmt FILE` | format |
| `rupam test` | run tests (experimental) |
| `rupam schema FILE` | describe a contract, or print an inferred schema as a `schema =` literal |
| `rupam explain NAME` | explain a diagnostic |
| `rupam lsp` | language server |

`--target` and `--no-strict` override `rupam.toml`.

## Distribution

Self-contained binaries for Linux and macOS (x86-64, ARM) and Windows
(x86-64), with DuckDB linked statically. Generated scripts need their
dialect's interpreter and packages, and the data-dict CLI when they read
contracted sources.
