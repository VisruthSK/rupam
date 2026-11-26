Errors report a code and a span, and the session continues.

  $ cp ../../examples/penguins.csv .
  $ cat > session.txt <<'INPUT'
  > let square(x) = x * x
  > square(7)
  > squar(2)
  > let birds = read_csv("penguins.csv")
  > birds |> select(speciez)
  > birds |> filter(body_mass > 4000)
  > 1:3
  > square(3)
  > INPUT
  $ rupam repl --target r-dplyr < session.txt
  square : (number) -> number
  49 : Int64
  1:1  unknown-name
    Unknown name: squar. Did you mean square?
  1:17  unknown-column
    No column named speciez. Did you mean species?
  1:17  optional-condition
    This condition is missing where its inputs are missing. Say what happens to those rows: missing = skip drops them, missing = keep keeps them, missing = error stops the run
  1:1  r-range
    `a:b` selects columns. For integers write `a..=b`
  9 : Int64
