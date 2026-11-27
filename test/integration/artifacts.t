A guarded branch lowers to native Polars, and the script ships a manifest and
a source map that points back to the program.

  $ cp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ cat > checked.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let orders = load(shop.orders)
  > let result = orders |> mutate(x = if (customer_id > 0) customer_id else customer_id + 9223372036854775807) |> select(x)
  > write_parquet(result, "checked.parquet")
  > RP
  $ rupam compile --target python-polars checked.rp -o checked.py
  $ python checked.py && rupam-dev show checked.parquet
  x: Int
  1
  2
  3
  4
  $ grep -c map_elements checked.py
  0
  [1]
  $ python -c "import json; m = json.load(open('checked.py.manifest.json')); print(m['target'], [i['path'] for i in m['inputs']], m['nonportable'])"
  python-polars ['data-dict.yaml', 'orders.parquet'] []
  $ python -c "import json; lines = open('checked.py').read().splitlines(); print(any(m['start']['line'] == 3 and '.with_columns(' in lines[m['generated_line'] - 1] for m in json.load(open('checked.py.map.json'))['mappings']))"
  True

An exported Python script writes UTF-8 even when its output is redirected.

  $ printf 'word\ncafé ∑\n' > words.csv
  $ echo 'print(read_csv("words.csv"))' > text.rp
  $ rupam lock --target python-polars text.rp > /dev/null && rupam compile --target python-polars text.rp -o text.py && python text.py > text.txt && cat text.txt
  shape: (1, 1)
  ┌────────┐
  │ word   │
  │ ---    │
  │ str    │
  ╞════════╡
  │ café ∑ │
  └────────┘
