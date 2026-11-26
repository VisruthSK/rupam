CSV and Parquet sources without a contract keep exact integers and read NaN
as missing, in both dialects.

  $ printf 'id,site,value\n9007199254740993,North,2.5\n2,North,\n3,South,4.0\n' > visits.csv
  $ python -c "import polars as pl; pl.DataFrame({'id': [9007199254740993, 2], 'score': [float('nan'), 1.5]}).write_parquet('scores.parquet')"
  $ cat > files.rp <<'RP'
  > let visits = read_csv("visits.csv")
  > let scores = read_parquet("scores.parquet")
  > write_parquet(visits |> select(id, site), "ids.parquet")
  > write_parquet(scores, "scores_out.parquet")
  > RP
  $ for target in python-polars r-dplyr; do
  >   rupam lock --target $target files.rp > /dev/null && rupam run --target $target files.rp > /dev/null &&
  >   rupam-dev show ids.parquet id && rupam-dev show scores_out.parquet
  > done
  id: Int, site: String
  2 | North
  3 | South
  9007199254740993 | North
  id: Int, score: Float?
  9007199254740993 | None
  2 | 1.5
  id: Int, site: String
  2 | North
  3 | South
  9007199254740993 | North
  id: Int, score: Float?
  9007199254740993 | None
  2 | 1.5
