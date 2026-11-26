The penguins analysis gives the same result in R and Python.

  $ cp ../../examples/penguins.rp ../../examples/penguins.csv ../../examples/species.csv .
  $ for target in python-polars r-dplyr; do
  >   rupam lock --target $target penguins.rp > /dev/null &&
  >   rupam run --target $target penguins.rp > /dev/null &&
  >   rupam-dev show penguins-by-species.parquet species > $target.txt
  > done
  $ cat python-polars.txt
  species: String, n: Int, mean_mass: Float, sd_mass: Float, median_bill: Float, islands: Int
  Adelie | 151 | 3.7006622516556296 | 0.45856612591013485 | 38.8 | 3
  Chinstrap | 68 | 3.7330882352941175 | 0.3843350813871913 | 49.55 | 1
  Gentoo | 123 | 5.076016260162602 | 0.5041162366570914 | 47.3 | 1
  $ diff python-polars.txt r-dplyr.txt
  2,4c2,4
  < Adelie | 151 | 3.7006622516556296 | 0.45856612591013485 | 38.8 | 3
  < Chinstrap | 68 | 3.7330882352941175 | 0.3843350813871913 | 49.55 | 1
  < Gentoo | 123 | 5.076016260162602 | 0.5041162366570914 | 47.3 | 1
  ---
  > Adelie | 151 | 3.700662251655629 | 0.4585661259101348 | 38.8 | 3
  > Chinstrap | 68 | 3.7330882352941175 | 0.3843350813871914 | 49.55 | 1
  > Gentoo | 123 | 5.076016260162602 | 0.5041162366570916 | 47.3 | 1
  [1]
