import os
import random
import sys

import polars as pl
import pyarrow as pa

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
n = 2_000_000
rng = random.Random(1)
zones = ["Midtown", "Harlem", "Queens", "Brooklyn", "JFK", "Bronx"]
tips = [rng.gauss(3.0, 1.5) for _ in range(n)]
frame = pl.DataFrame(
    {
        "id": range(n),
        "fare": [rng.gammavariate(2.0, 8.0) for _ in range(n)],
        "paid": [rng.random() < 0.7 for _ in range(n)],
        "zone": [rng.choice(zones) for _ in range(n)],
        "tip": [t if t >= 0 else None for t in tips],
    },
    schema={"id": pl.Int64, "fare": pl.Float64, "paid": pl.Boolean, "zone": pl.String, "tip": pl.Float64},
)
frame.write_ipc(os.path.join(out, "polars_2m.arrow"), compression="uncompressed",
                compat_level=pl.CompatLevel.oldest())
table = frame.to_arrow()
with pa.OSFile(os.path.join(out, "batches_2m.arrow"), "wb") as sink, \
        pa.ipc.new_file(sink, table.schema) as writer:
    for batch in table.to_batches(max_chunksize=65536):
        writer.write_batch(batch)
