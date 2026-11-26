The REPL previews the same table in both persistent workers.

  $ cp ../../examples/shop.rp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ printf ':schema regional\n:preview regional\n:target r-dplyr\n:preview regional\n:quit\n' | rupam repl --target python-polars shop.rp
  region        String
  transactions  Int
  total_margin  Float?
  3 columns from orders in data-dict.yaml
  2 rows × 3 columns, ordered
     region    transactions  total_margin
     <String>         <Int>      <Float?>
  1  North                2         10.00
  2  South                1          5.00
  r-dplyr
  2 rows × 3 columns, ordered
     region    transactions  total_margin
     <String>         <Int>      <Float?>
  1  North                2         10.00
  2  South                1          5.00
