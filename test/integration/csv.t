CSV inference reads the whole file under the specified grammar, and both
targets load the inferred schema with the same values.

  $ export TERM=dumb
  $ cat > types.csv <<'CSV'
  > id,big,lead,x,flag,day,bad_day,plus,word,blank
  > 1,9223372036854775807,007,1.5,TRUE,2024-02-29,2023-02-29,+1,"NA",""
  > -42,9223372036854775808,12,1e-3,FALSE,2023-01-01,2023-01-01,2,NA,x
  > 0,1,3,NaN,,NA,2023-01-02,3,text,y
  > CSV
  $ printf 'let t = read_csv("types.csv")\n:schema t\n:preview t\n:preview t |> select(word, blank)\n:target r-dplyr\n:preview t\n:preview t |> select(word, blank)\n:quit\n' | rupam repl --target python-polars
  3 rows × 10 columns
    id       Int
    big      String
    lead     String
    x        Float?
    flag     Bool?
    day      Date?
    bad_day  String
    plus     String
    word     String?
    blank    String?
  3 rows × 10 columns
    id       Int
    big      String
    lead     String
    x        Float?
    flag     Bool?
    day      Date?
    bad_day  String
    plus     String
    word     String?
    blank    String?
  3 rows × 10 columns, unordered
        id  big                  lead             x  flag     day         bad_day     plus    
     <Int>  <String>             <String>  <Float?>  <Bool?>  <Date?>     <String>    <String>
  1      1  9223372036854775807  007           1.50  TRUE     2024-02-29  2023-02-29  +1      
  2    -42  9223372036854775808  12            0.00  FALSE    2023-01-01  2023-01-01  2       
  3      0  1                    3             None  None     None        2023-01-02  3       
  … and word <String?>, blank <String?>
  3 rows × 2 columns, unordered
     word       blank    
     <String?>  <String?>
  1  None       None     
  2  None       x        
  3  text       y        
  r-dplyr
  3 rows × 10 columns, unordered
        id  big                  lead             x  flag     day         bad_day     plus    
     <Int>  <String>             <String>  <Float?>  <Bool?>  <Date?>     <String>    <String>
  1      1  9223372036854775807  007           1.50  TRUE     2024-02-29  2023-02-29  +1      
  2    -42  9223372036854775808  12            0.00  FALSE    2023-01-01  2023-01-01  2       
  3      0  1                    3             None  None     None        2023-01-02  3       
  … and word <String?>, blank <String?>
  3 rows × 2 columns, unordered
     word       blank    
     <String?>  <String?>
  1  None       None     
  2  None       x        
  3  text       y        

Infinity in an otherwise numeric column is an error with its location; in a
text column it is text.

  $ printf 'x,label\n1.5,a\ninf,Inf\n' > inf.csv
  $ printf 'let t = read_csv("inf.csv")\n:quit\n' | rupam repl --target python-polars
  1:9  unreadable-input
    ./inf.csv:3: column x holds an infinite value
  $ printf 'label\nInf\nword\n' > word.csv
  $ printf 'let t = read_csv("word.csv")\n:schema t\n:quit\n' | rupam repl --target python-polars
  2 rows × 1 column
    label  String
  2 rows × 1 column
    label  String

Malformed files are errors that name the line.

  $ printf 'a,a\n1,2\n' > duplicate.csv
  $ printf 'a,b\n1,2\n3,4,5\n' > ragged.csv
  $ printf 'a,b\n1,x"y\n' > quote.csv
  $ printf 'a,b\n1,ok\n2,caf\351\n' > latin1.csv
  $ for f in duplicate ragged quote latin1; do printf 'let t = read_csv("%s.csv")\n:quit\n' $f | rupam repl --target python-polars; done
  1:9  unreadable-input
    ./duplicate.csv:1: the column name a appears twice
  1:9  unreadable-input
    ./ragged.csv:3: expected 2 fields, found 3
  1:9  unreadable-input
    ./quote.csv:2: column 4: a quote in a field that does not start with one
  1:9  unreadable-input
    ./latin1.csv:3: field 2 is not valid UTF-8

`missing_tokens` replaces the default missing tokens, so real "NA" text can
be kept; both targets apply the same tokens.

  $ printf 'code,n\nNA,1\nUS,\n' > codes.csv
  $ printf 'let t = read_csv("codes.csv", missing_tokens = [""])\n:schema t\n:preview t\n:target r-dplyr\n:preview t\n:quit\n' | rupam repl --target python-polars
  2 rows × 2 columns
    code  String
    n     Int?
  2 rows × 2 columns
    code  String
    n     Int?
  2 rows × 2 columns, unordered
     code           n
     <String>  <Int?>
  1  NA             1
  2  US          None
  r-dplyr
  2 rows × 2 columns, unordered
     code           n
     <String>  <Int?>
  1  NA             1
  2  US          None
  $ printf 'let t = read_parquet("x.parquet", missing_tokens = [])\n:quit\n' | rupam repl --target python-polars
  1:52  wrong-arguments
    read_parquet has no missing_tokens
