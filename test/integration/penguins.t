The penguins analysis gives the same result in R and Python.

  $ export TERM=dumb
  $ cp ../../examples/penguins.rp ../../examples/penguins.csv ../../examples/species.csv .
  $ for target in python-polars r-dplyr; do
  >   rupam lock --target $target penguins.rp > /dev/null &&
  >   rupam run --target $target penguins.rp > /dev/null &&
  >   rupam-dev show penguin-bills.parquet species island measure mm > $target.txt
  > done
  $ head -4 python-polars.txt
  species: String, island: String, measure: String, mm: Float
  Adelie | Biscoe | bill_dep | 16.0
  Adelie | Biscoe | bill_dep | 16.2
  Adelie | Biscoe | bill_dep | 16.5
  $ diff python-polars.txt r-dplyr.txt
