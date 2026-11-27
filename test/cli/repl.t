Errors report a code and a span, and the session continues.

  $ cp ../../examples/penguins.csv .
  $ cat > session.txt <<'INPUT'
  > let square(x) = x * x
  > square(7)
  > squar(2)
  > let birds = read_csv("penguins.csv")
  > birds |> select(speciez)
  > birds |> filter(body_mass > 4000)
  > birds |> make_key(species)
  > 1:3
  > square(3)
  > match 0, 1 { 0, _ => "A", _, 0 => "B", _, _ => "C" }
  > birds |> mutate(`x` = 1, x = 2)
  > INPUT
  $ rupam repl --target r-dplyr < session.txt
  square : (Number) -> Number
  49 : Int
  1:1  unknown-name
    Unknown name: squar. Did you mean square?
  344 rows × 8 columns
    species      String
    island       String
    bill_len     Float?
    bill_dep     Float?
    flipper_len  Int?
    body_mass    Int?
    sex          String?
    year         Int
  1:17  unknown-column
    No column named speciez. Did you mean species?
  1:17  optional-condition
    This condition is missing where its inputs are missing. Say what happens to those rows: missing = skip drops them, missing = keep keeps them, missing = error stops the run
  1:10  unknown-name
    Unknown name: make_key
  1:1  r-range
    `a:b` selects columns. For integers write `a=..=b`
  9 : Int
  "A" : String
  1:30  duplicate-name
    mutate repeats the column x
