The REPL previews the same table in both persistent workers.

  $ cp ../../examples/shop.rp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ printf ':schema regional\n:preview regional\n:target r-dplyr\n:preview regional\n:quit\n' | rupam repl --target python-polars shop.rp
  2 rows × 3 columns
    region        String
    transactions  Int
    total_margin  Float?
  2 rows × 3 columns, ordered by (total_margin desc, region asc)
     region    transactions  total_margin
     <String>         <Int>      <Float?>
  1  North                2         10.00
  2  South                1          5.00
  r-dplyr
  2 rows × 3 columns, ordered by (total_margin desc, region asc)
     region    transactions  total_margin
     <String>         <Int>      <Float?>
  1  North                2         10.00
  2  South                1          5.00

Both workers keep non-ASCII text intact in both directions.

  $ printf '"café ∑"\n:target r-dplyr\n"café ∑"\n:quit\n' | rupam repl --target python-polars
  "café ∑" : String
  r-dplyr
  "café ∑" : String
