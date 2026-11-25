A compiled script refuses data changed since the lock, in both dialects.

  $ cp ../../examples/shop.rp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ rupam compile --target python-polars shop.rp > shop.py
  $ rupam compile --target r-dplyr shop.rp > shop.R
  $ python shop.py && python show.py regional.parquet region
  region: String, transactions: Int64, total_margin: Float64
  ('North', 2, 10.0)
  ('South', 1, 5.0)
  $ Rscript shop.R && python show.py regional.parquet region
  region: String, transactions: Int64, total_margin: Float64
  ('North', 2, 10.0)
  ('South', 1, 5.0)
  $ rm regional.parquet
  $ python -c "import polars as pl; t = pl.read_parquet('orders.parquet'); t.with_columns(pl.when(pl.col('revenue') == 0).then(None).otherwise(pl.col('revenue')).alias('revenue')).write_parquet('orders.parquet')"
  $ python shop.py 2>&1 | grep -o 'changed since rupam lock.*'
  changed since rupam lock: run rupam lock, then recompile")
  changed since rupam lock: run rupam lock, then recompile
  $ Rscript shop.R 2>&1 | grep -o 'changed since rupam lock.*'
  changed since rupam lock: run rupam lock, then recompile
  $ ls regional.parquet
  ls: cannot access 'regional.parquet': No such file or directory
  [2]
  $ rupam compile --target python-polars shop.rp
  shop.rp:29:1  stale-lock
    orders.parquet changed since rupam lock. Run rupam lock shop.rp, then compile again.
  [123]
  $ rupam lock --target python-polars shop.rp 2>&1 | head -3
  shop.rp:29:1  contract-failure
    data-dict.yaml does not match its data: has 1 null value
