# rupam

rupam is a small typed language for data analysis. You write a pipeline once,
check it against a [data-dict](https://data-dict.tidyverse.org/) contract,
explore it in a REPL, and compile it to a plain R (dplyr) or Python (Polars)
script that runs without rupam installed.

```text
use contract "data-dict.yaml" as shop

let standardize(x) = (x - mean(x)) / sd(x)

let regional =
  load(shop.orders)
  |> filter(revenue > 0.0)
  |> derive(margin = revenue - coalesce(cost, 0.0), z = standardize(margin))
  |> summarize(transactions = count(), total = sum(margin), by = region)
  |> arrange(desc(total), region)

save_parquet(regional, "regional.parquet")
```

The checker knows `cost` may be missing, so `margin` needs the `coalesce`.
It knows `sd` can be undefined, so `z` comes out as `Float64?`. And it rejects
`filter(cost > 0.0)` outright, because a comparison with a missing value has
no answer.

## Install

rupam needs OCaml 5.5 and opam. The generated scripts and the REPL's previews
call out to Python or R, and both validate input through the data-dict CLI.

```sh
opam install . --deps-only --with-test --with-dev-setup
opam pin add rupam .          # puts `rupam` on your PATH
uv sync                       # Python side: polars and data-dict
```

For the R target, install `dplyr`, `tidyr`, `bit64`, `gmp`, `jsonlite` and
`nanoparquet`. On Windows, use WSL.

## Use

```sh
rupam                                # empty REPL
rupam repl examples/shop.rp          # REPL with a program loaded
uv run rupam run examples/shop.rp    # check, validate and execute
rupam compile r examples/shop.rp     # print a standalone R script
rupam compile python examples/shop.rp -o shop.py
rupam fmt examples/shop.rp
```

`compile -o` also writes a JSON manifest (dependencies, validation steps,
fidelity notes) and a source map back to the `.rp` file. `rupam --help`
lists every command.

## Documentation

- [Language](docs/language.md): values, functions, types, records, variants and pattern matching
- [Tables](docs/tables.md): contracts, the table verbs, missing values and how code is generated
- [REPL](docs/repl.md): commands, completion and inspecting unfinished pipelines
- [B2T2 datasheet](docs/b2t2.md): coverage of the Brown table-types benchmark
- [Design brief](typed_polyglot_data_science_language_agent_brief.md): the full specification and its rationale

## Development

```sh
dune build @all @runtest      # parser, inference, evaluator, elaboration
dune fmt                      # ocamlformat 0.29.0, Jane Street profile
uv run python -m unittest discover -s test   # runs the generated R and Python
```

CI runs the same three steps on every push and pull request.
