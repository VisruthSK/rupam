import sys

import polars as pl

frame = pl.read_parquet(sys.argv[1])
for key in sys.argv[2:]:
    frame = frame.sort(key, nulls_last=True)
print(", ".join(f"{name}: {dtype}" for name, dtype in frame.schema.items()))
for row in frame.rows():
    print(tuple(round(value, 12) if isinstance(value, float) else value for value in row))
