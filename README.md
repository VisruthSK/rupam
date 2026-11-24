# rupam

rupam is a typed data-analysis language implemented in OCaml. Programs use the
`.rp` extension. The example in `examples/shop.rp` imports a data dictionary,
derives nullable margins and standardized scores, groups orders by region, and
writes an explicitly ordered Parquet result.

## Build and run

Install OCaml 5.3, Dune, Menhir, Yojson and Uucp. For Python execution install uv.
The R target requires R, dplyr, bit64, jsonlite, nanoparquet and gmp. Both table
targets use the upstream data-dict CLI from the locked uv environment.

```sh
opam install . --deps-only
uv sync --locked
dune build
uv run dune exec rupam -- examples/shop.rp
uv run dune exec rupam -- repl examples/shop.rp
```

The example writes `regional.parquet` in the working directory. North has two
transactions totaling 10.0 and South has one totaling 5.0.

In the REPL, enter `:schema regional` to inspect the inferred fields,
`:explain regional` to inspect its plan, and `:preview regional` to execute it.
Use `:backend r` or `:backend python` to select a preview engine. Type
`:help` to list commands and `:quit` to leave.

## Standalone scripts

Run the following commands from the repository root. The generated scripts
retain the dictionary path relative to the compilation working directory.
They call data-dict and their target libraries directly.

```sh
uv run dune exec rupam -- compile python examples/shop.rp > shop.py
uv run python shop.py
uv run dune exec rupam -- compile r examples/shop.rp > shop.R
uv run Rscript shop.R
```

Use `compile python examples/shop.rp -o shop.py` to write a script with a
JSON dependency manifest and output-statement source map. The editor server
runs with `dune exec rupam -- lsp` and uses standard LSP over stdin/stdout.

## Verification

```sh
dune build @all @runtest
uv run python -m unittest discover -s test
```

The OCaml suite exercises parsing, formatting, inference, elaboration and
reference evaluation. The integration suite invokes the compiler and both
generated scripts, reads their Parquet outputs, and checks failure on invalid
input. Release requirements and acceptance cases are in
[the language brief](typed_polyglot_data_science_language_agent_brief.md).

