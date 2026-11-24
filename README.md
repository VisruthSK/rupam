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
  |> mutate(margin = revenue - coalesce(cost, 0.0), z = standardize(margin))
  |> summarize(transactions = count(), total = sum(margin), by = region)
  |> arrange(desc(total), region)

save_parquet(regional, "regional.parquet")
```

The checker knows `cost` may be missing, so `margin` needs the `coalesce`.
It knows `sd` can be undefined, so `z` comes out as `Float64?`. And it rejects
`filter(cost > 0.0)` outright, because a comparison with a missing value has
no answer.

## Install

Download the archive for your system from the
[releases page](https://github.com/VisruthSK/rupam/releases), unpack it and
put `rupam` (or `rupam.exe`) on your PATH. Builds exist for Linux and macOS
on x86-64 and ARM, and for Windows on x86-64. The binary is self-contained:
it embeds [DuckDB](https://duckdb.org) 2.0, which rupam uses to read Parquet,
CSV and TSV files.

Previews and `rupam run` execute the generated script, so they need the target
language installed, plus the data-dict CLI for contract validation. The
`python-polars` dialect needs `polars` and `data-dict`. The `r-dplyr` dialect
needs `dplyr`, `tidyr`, `bit64`, `gmp`, `jsonlite` and `nanoparquet`.

### From source

You need OCaml 5.5, opam, a C compiler, `curl` and `tar`.

```sh
scripts/fetch-duckdb          # pinned, checksummed DuckDB static libraries
opam install . --deps-only --with-test --with-dev-setup
opam pin add rupam .
```

## Projects

A project is a directory with a `rupam.toml`:

```toml
[target]
dialect = "r-dplyr"       # or "python-polars"

[check]
strict = true             # the default; false allows target-specific code
```

There is no default dialect. Commands that execute or compile take it from
`--target` or from `rupam.toml`. Previews run the same script that
`rupam compile` writes, so what you see in the REPL is what the script
produces. In strict mode a program is portable, and every dialect must give
the same result or refuse to compile.

If the project directory holds a `pyproject.toml` or `uv.lock`, Python runs
through `uv run`. If it holds a `renv.lock`, R loads that renv library first.
Previews and runs then use exactly the packages the project pins.

## Use

```sh
rupam                                    # empty REPL
rupam repl examples/shop.rp              # REPL with a program loaded
rupam run examples/shop.rp               # compile, validate and execute
rupam compile examples/shop.rp           # print the standalone script
rupam compile examples/shop.rp -o shop.R # script plus manifest and source map
rupam fmt examples/shop.rp
```

Each of these takes `--target r-dplyr` or `--target python-polars` when there
is no `rupam.toml`. `compile -o` also writes a JSON manifest (dependencies,
validation steps, fidelity notes) and a source map back to the `.rp` file.
`rupam --help` lists every command.

## Documentation

- [Language](docs/language.md): values, functions, types, records, variants and pattern matching
- [Tables](docs/tables.md): contracts, the table verbs, missing values and how code is generated
- [REPL](docs/repl.md): commands, completion and inspecting unfinished pipelines
- [B2T2 datasheet](docs/b2t2.md): coverage of the Brown table-types benchmark
- [Specification](docs/spec/README.md): the design rupam is built towards
- [Roadmap](docs/roadmap.md): the order of work from here to the specification

## Development

```sh
dune build @all @runtest      # parser, inference, evaluator, elaboration
dune fmt                      # ocamlformat 0.29.0, Jane Street profile
uv run python -m unittest discover -s test   # runs the generated R and Python
```

CI runs the same three steps on every push and pull request.

## License

Copyright (C) 2026 Visruth Srimath Kandali.

rupam is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version. See [LICENSE](LICENSE).

Scripts that rupam generates are yours. The
[output exception](LICENSE.exception) lets you distribute them, including the
helper code rupam copies into them, under any terms you like.
