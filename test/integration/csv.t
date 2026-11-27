CSV inference reads the whole file under the specified grammar, and both
targets load the inferred schema with the same values.

  $ export TERM=dumb
  $ cat > types.csv <<'CSV'
  > id,big,lead,x,flag,day,bad_day,plus,word,blank
  > 1,9223372036854775807,007,1.5,TRUE,2024-02-29,2023-02-29,+1,"NA",""
  > -42,9223372036854775808,12,1e-3,FALSE,2023-01-01,2023-01-01,2,NA,x
  > 0,1,3,NaN,,NA,2023-01-02,3,text,y
  > CSV
  $ printf 'let t = read_csv("types.csv")\n:schema t\n:preview t\n:target r-dplyr\n:preview t\n:quit\n' | rupam repl --target python-polars

Infinity in an otherwise numeric column is an error with its location; in a
text column it is text.

  $ printf 'x,label\n1.5,a\ninf,Inf\n' > inf.csv
  $ printf 'let t = read_csv("inf.csv")\n:quit\n' | rupam repl --target python-polars
  $ printf 'label\nInf\nword\n' > word.csv
  $ printf 'let t = read_csv("word.csv")\n:schema t\n:quit\n' | rupam repl --target python-polars

Malformed files are errors that name the line.

  $ printf 'a,a\n1,2\n' > duplicate.csv
  $ printf 'a,b\n1,2\n3,4,5\n' > ragged.csv
  $ printf 'a,b\n1,x"y\n' > quote.csv
  $ for f in duplicate ragged quote; do printf 'let t = read_csv("%s.csv")\n:quit\n' $f | rupam repl --target python-polars; done

`missing_tokens` replaces the default missing tokens, so real "NA" text can
be kept; both targets apply the same tokens.

  $ printf 'code,n\nNA,1\nUS,\n' > codes.csv
  $ printf 'let t = read_csv("codes.csv", missing_tokens = [""])\n:schema t\n:preview t\n:target r-dplyr\n:preview t\n:quit\n' | rupam repl --target python-polars
  $ printf 'let t = read_parquet("x.parquet", missing_tokens = [])\n:quit\n' | rupam repl --target python-polars
