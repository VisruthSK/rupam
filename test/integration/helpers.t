A helper built from other helpers keeps its block-local columns as columns,
so a correlation written in Rupam runs pooled and per group, with the same
values in R and Python.

  $ export TERM=dumb
  $ cp ../../examples/penguins.csv .
  $ cat > corr.rp <<'RP'
  > let penguins = read_csv("penguins.csv")
  > let center(x) = x - mean(x)
  > let pearson(x, y) = {
  >   let dx = center(x)
  >   let dy = center(y)
  >   sum(dx * dy) / sqrt(sum(dx * dx) * sum(dy * dy))
  > }
  > let clean = penguins |> filter(some(bill_len) and some(bill_dep))
  > let pooled = clean |> summarize(r = pearson(bill_len, bill_dep), missing = skip)
  > let grouped = clean |> summarize(r = pearson(bill_len, bill_dep), by = species, missing = skip)
  > RP
  $ for target in python-polars r-dplyr; do printf ':preview pooled\n:preview grouped\n:quit\n' | rupam repl --target $target corr.rp; done
  1 row × 1 column, ordered
            r
     <Float?>
  1     -0.24
  3 rows × 2 columns, unordered
     species           r
     <String>   <Float?>
  1  Adelie         0.39
  2  Gentoo         0.64
  3  Chinstrap      0.65
  1 row × 1 column, ordered
            r
     <Float?>
  1     -0.24
  3 rows × 2 columns, unordered
     species           r
     <String>   <Float?>
  1  Adelie         0.39
  2  Gentoo         0.64
  3  Chinstrap      0.65

The helper's missing-value requirement is enforced where it is used.

  $ printf 'let strict = clean |> summarize(r = pearson(bill_len, bill_dep))\n:quit\n' | rupam repl --target python-polars corr.rp
  corr.rp:6:38  missing-policy-required
    sum reads a column that can be missing. Say what to do with missing values: missing = skip ignores them, missing = keep makes the result missing, missing = error stops the run
