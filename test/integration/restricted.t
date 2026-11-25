Restricted columns and values derived from them never reach an output.

  $ cp ../../examples/orders.parquet .
  $ sed 's/- name: cost/- name: cost\n        display: restricted/' ../../examples/data-dict.yaml > data-dict.yaml
  $ cat > private.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let orders = load(shop.orders) |> mutate(discount = cost * 0.5)
  > write_parquet(orders, "result.parquet")
  > RP
  $ rupam compile --target python-polars private.rp > private.py && python private.py && python show.py result.parquet
  customer_id: Int64, region: String, revenue: Float64
  (1, 'North', 3.0)
  (2, 'South', 5.0)
  (3, 'North', 9.0)
  (4, 'South', 0.0)
  $ rupam compile --target r-dplyr private.rp > private.R && Rscript private.R && python show.py result.parquet
  customer_id: Int64, region: String, revenue: Float64
  (1, 'North', 3.0)
  (2, 'South', 5.0)
  (3, 'North', 9.0)
  (4, 'South', 0.0)
