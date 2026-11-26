Joins use named keys, keep unmatched rows and reject colliding columns.

  $ cp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ cat > join.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let orders = load(shop.orders)
  > let lefts = orders |> select(customer_id, cost)
  > let rights = orders |> select(cost, region)
  > let joined = lefts |> left_join(rights, by = cost, relationship = many_to_one) |> arrange(customer_id)
  > write_parquet(joined, "joined.parquet")
  > RP
  $ rupam compile --target python-polars join.rp > join.py && python join.py && rupam-dev show joined.parquet
  customer_id: Int, cost: Float?, region: String?
  1 | None | None
  2 | 0.0 | South
  3 | 2.0 | North
  4 | 1.0 | South
  $ rupam compile --target r-dplyr join.rp > join.R && Rscript join.R && rupam-dev show joined.parquet
  customer_id: Int, cost: Float?, region: String?
  1 | None | None
  2 | 0.0 | South
  3 | 2.0 | North
  4 | 1.0 | South
  $ sed -i 's/select(cost, region)/select(cost, customer_id)/' join.rp
  $ rupam compile --target python-polars join.rp
  join.rp:5:14  join-collision
    Both tables have a column named customer_id
  [123]
