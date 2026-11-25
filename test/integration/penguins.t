The penguins analysis gives the same result in R and Python.

  $ cp ../../examples/penguins.rp ../../examples/penguins.csv ../../examples/species.csv .
  $ for target in python-polars r-dplyr; do
  >   rupam lock --target $target penguins.rp > /dev/null &&
  >   rupam run --target $target penguins.rp > /dev/null &&
  >   python show.py penguins-by-species.parquet species > $target.txt
  > done
  $ cat python-polars.txt
  species: String, n: Int64, mean_mass: Float64, sd_mass: Float64, median_bill: Float64, islands: Int64
  ('Adelie', 151, 3.700662251656, 0.45856612591, 38.8, 3)
  ('Chinstrap', 68, 3.733088235294, 0.384335081387, 49.55, 1)
  ('Gentoo', 123, 5.076016260163, 0.504116236657, 47.3, 1)
  $ diff python-polars.txt r-dplyr.txt
