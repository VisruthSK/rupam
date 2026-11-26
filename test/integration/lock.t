Rupam rejects stale locks. Standalone scripts read their supplied inputs
without checking the lock, in both dialects.

  $ cp ../../examples/shop.rp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ rupam compile --target python-polars shop.rp > shop.py
  $ rupam compile --target r-dplyr shop.rp > shop.R
  $ python shop.py && rupam-dev show regional.parquet region
  region: String, transactions: Int, total_margin: Float
  North | 2 | 10.0
  South | 1 | 5.0
  $ Rscript shop.R && rupam-dev show regional.parquet region
  region: String, transactions: Int, total_margin: Float
  North | 2 | 10.0
  South | 1 | 5.0
  $ rm regional.parquet
  $ python -c "import pyarrow.parquet as pq; table = pq.read_table('orders.parquet'); pq.write_table(table.replace_schema_metadata({b'test': b'changed'}), 'orders.parquet')"
  $ python shop.py && rupam-dev show regional.parquet region
  region: String, transactions: Int, total_margin: Float
  North | 2 | 10.0
  South | 1 | 5.0
  $ rm regional.parquet
  $ Rscript shop.R && rupam-dev show regional.parquet region
  region: String, transactions: Int, total_margin: Float
  North | 2 | 10.0
  South | 1 | 5.0
  $ rupam compile --target python-polars shop.rp
  stale-lock
    orders.parquet changed since rupam lock. Run rupam lock shop.rp, then compile again.
  [123]
  $ python -c "import polars as pl; t = pl.read_parquet('orders.parquet'); t.with_columns(pl.when(pl.col('revenue') == 0).then(None).otherwise(pl.col('revenue')).alias('revenue')).write_parquet('orders.parquet')"
  $ rupam lock --target python-polars shop.rp 2>&1 | head -3
  contract-failure
    data-dict.yaml does not match its data: has 1 null value
