import os
import sys

import numpy as np
import polars as pl
import pyarrow as pa

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
n = 2_000_000
rng = np.random.default_rng(1)
frame = pl.DataFrame(
    {
        "id": np.arange(n, dtype=np.int64),
        "fare": rng.gamma(2.0, 8.0, n),
        "paid": rng.random(n) < 0.7,
        "zone": rng.choice(["Midtown", "Harlem", "Queens", "Brooklyn", "JFK", "Bronx"], n),
        "tip": rng.normal(3.0, 1.5, n),
    }
).with_columns(pl.when(pl.col("tip") < 0).then(None).otherwise(pl.col("tip")).alias("tip"))
frame.write_ipc(os.path.join(out, "polars_2m.arrow"), compression="uncompressed",
                compat_level=pl.CompatLevel.oldest())
table = frame.to_arrow()
with pa.OSFile(os.path.join(out, "batches_2m.arrow"), "wb") as sink, \
        pa.ipc.new_file(sink, table.schema) as writer:
    for batch in table.to_batches(max_chunksize=65536):
        writer.write_batch(batch)
