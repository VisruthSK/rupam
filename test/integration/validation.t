A contract is validated against its whole source before any value is shown.
A required column with a missing value fails and binds nothing; a value
outside a documented range passes (C20, C21).

  $ python -c "import polars as pl; pl.DataFrame({'id': [1, 2, 3], 'score': [5.0, None, 101.0]}).write_parquet('t.parquet')"
  $ cat > data-dict.yaml <<'YAML'
  > $version: 0.1.0
  > $learn_more: https://data-dict.tidyverse.org/
  > name: c
  > tables:
  >   - name: t
  >     source:
  >       parquet: t.parquet
  >     columns:
  >       - name: id
  >         type: number(id)
  >         examples: [1, 2]
  >         constraints: [primary_key]
  >       - name: score
  >         type: number(quantity)
  >         range: [0, 100]
  >         constraints: [required]
  > YAML
  $ printf 'use contract "data-dict.yaml" as c\nlet t = load(c.t)\n:bindings\n' | rupam repl --target python-polars
  contract-failure
    data-dict.yaml does not match its data: t.score: A required column must not contain nulls; has 1 null value
  c : contract
  $ python -c "import polars as pl; pl.DataFrame({'id': [1, 2, 3], 'score': [5.0, 7.0, 101.0]}).write_parquet('t.parquet')"
  $ printf 'use contract "data-dict.yaml" as c\nload(c.t)\n' | rupam repl --target python-polars
  3 rows × 2 columns, unordered
        id    score
     <Int>  <Float>
  1      1     5.00
  2      2     7.00
  3      3   101.00
