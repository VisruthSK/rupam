A dialect-specific function needs an import and strict = false, is recorded
as nonportable, and is rejected by other dialects.

  $ cp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ cat > body.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let t = load(shop.orders) |> mutate(root = polars.sqrt(revenue)) |> select(root)
  > write_parquet(t, "sqrt.parquet")
  > RP
  $ cp body.rp sqrt.rp && rupam compile --target python-polars sqrt.rp
  sqrt.rp:2:44  unknown-name
    Unknown name: polars
  [123]
  $ (echo "import polars"; cat body.rp) > sqrt.rp && rupam compile --target python-polars sqrt.rp
  sqrt.rp:1:1  dialect-capability
    import polars is target-specific. Set strict = false under [check] in rupam.toml, or pass --no-strict
  [123]
  $ rupam compile --no-strict --target python-polars sqrt.rp -o sqrt.py && python sqrt.py && rupam-dev show sqrt.parquet
  root: Float
  1.7320508075688772
  2.23606797749979
  3.0
  0.0
  $ python -c "import json; print(json.load(open('sqrt.py.manifest.json'))['nonportable'])"
  ['polars.sqrt']
  $ rupam compile --no-strict --target r-dplyr sqrt.rp
  sqrt.rp:3:44  dialect-capability
    R target cannot execute the Python capability polars.sqrt
  [123]
