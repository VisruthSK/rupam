Grouped windows and a pivot round trip agree across dialects.

  $ cp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ cat > pivot.rp <<'RP'
  > use contract "data-dict.yaml" as shop
  > let t = load(shop.orders) |> mutate(center = revenue - mean(revenue), by = region)
  > let tall = t |> select(customer_id, revenue, center) |> pivot_longer(cols = (revenue, center), names_to = metric, values_to = reading)
  > let wide = tall |> pivot_wider(names_from = metric, values_from = reading, levels = [revenue, center]) |> rename(centered = center) |> arrange(customer_id)
  > write_parquet(wide, "result.parquet")
  > RP
  $ rupam compile --target python-polars pivot.rp > pivot.py && python pivot.py && python show.py result.parquet
  customer_id: Int64, revenue: Float64, centered: Float64
  (1, 3.0, -3.0)
  (2, 5.0, 2.5)
  (3, 9.0, 3.0)
  (4, 0.0, -2.5)
  $ rupam compile --target r-dplyr pivot.rp > pivot.R && Rscript pivot.R && python show.py result.parquet
  customer_id: Int64, revenue: Float64, centered: Float64
  (1, 3.0, -3.0)
  (2, 5.0, 2.5)
  (3, 9.0, 3.0)
  (4, 0.0, -2.5)
