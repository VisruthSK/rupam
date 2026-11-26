Switching target never replays writes, and :save writes the accepted analysis.

  $ cp ../../examples/orders.parquet ../../examples/data-dict.yaml .
  $ printf 'use contract "data-dict.yaml" as shop\nlet small = load(shop.orders) |> select(region) |> arrange(region)\nsmall |> write_parquet("out.parquet")\n' > a.rp
  $ (timeout 120 sh -c "until [ -f out.parquet ]; do sleep 0.1; done"; rm out.parquet; printf ':language python\n:target\nsmall\nlet n = 1 + 1\n:undo\n:bindings\n:save b.rp\n:quit\n') | rupam repl --target r-dplyr a.rp
  python-polars
  python-polars
  4 rows × 1 column, ordered
     region  
     <String>
  1  North   
  2  North   
  3  South   
  4  South   
  n : Int
  shop : contract
  small : table
  Saved b.rp
  $ ls out.parquet
  ls: cannot access 'out.parquet': No such file or directory
  [2]
  $ cat b.rp
  use contract "data-dict.yaml" as shop
  
  let small =
      load(shop.orders)
      |> select(region)
      |> arrange(region)
  
  small |> write_parquet("out.parquet")
  $ rupam --version | cut -d+ -f1
  0.0.0-dev
