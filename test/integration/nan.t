NaN in a source reads as a missing value.

  $ cp ../../examples/data-dict.yaml .
  $ python -c "import polars as pl; pl.DataFrame({'customer_id': [1, 2, 3, 4], 'region': ['North'] * 4, 'revenue': [1.0] * 4, 'cost': [None, float('nan'), None, float('nan')]}).write_parquet('orders.parquet')"
  $ cat > groups.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let t = load(shop.orders) |> summarize(n = n(), by = cost, group_missing = propagate)
  > write_parquet(t, "groups.parquet")
  > RP
  $ rupam compile --target r-dplyr groups.rp > groups.R && Rscript groups.R && python show.py groups.parquet
  cost: Float64, n: Int64
  (None, 4)
  $ rupam compile --target python-polars groups.rp > groups.py && python groups.py && python show.py groups.parquet
  cost: Float64, n: Int64
  (None, 4)
