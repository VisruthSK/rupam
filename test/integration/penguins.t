The penguins analysis gives the same result in R and Python.

  $ cp ../../examples/penguins.rp ../../examples/penguins.csv ../../examples/species.csv .
  $ for target in python-polars r-dplyr; do
  >   rupam lock --target $target penguins.rp > /dev/null &&
  >   rupam run --target $target penguins.rp > /dev/null &&
  >   rupam-dev show penguins-by-species.parquet species > $target.txt
  > done
  Usage: rupam-dev show [--help] [OPTION]… FILE [KEY]…
  rupam-dev: FILE argument: no penguins-by-species.parquet file or directory
  Usage: rupam-dev show [--help] [OPTION]… FILE [KEY]…
  rupam-dev: FILE argument: no penguins-by-species.parquet file or directory
  [124]
  $ cat python-polars.txt
  $ diff python-polars.txt r-dplyr.txt
