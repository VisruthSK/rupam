Variants and exact integers survive both dialects.

  $ cat > variant.rp <<'RP'
  > type Maybe<a> = | Empty | Just(value: a)
  > let get(x) = match x { Empty => 0, Just(n) => n }
  > get(Just(9007199254740993))
  > RP
  $ rupam compile --target r-dplyr variant.rp > variant.R && Rscript variant.R
  9007199254740993
  $ rupam compile --target python-polars variant.rp > variant.py && python variant.py
  9007199254740993
