import datetime
import decimal
import itertools
import os
import random
import sys

import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
rng = random.Random(7)
n = 3000


def maybe(value, p):
    return None if rng.random() < p else value


def floats(nan, null):
    return [maybe(float("nan") if rng.random() < nan else rng.uniform(-9, 9), null) for _ in range(n)]


table = pa.table(
    {
        "id": pa.array(range(n), pa.int64()),
        "count": pa.array([maybe(rng.randrange(-9, 9), 0.1) for _ in range(n)], pa.int64()),
        "small": pa.array([rng.randrange(-9, 9) for _ in range(n)], pa.int32()),
        "clean": pa.array(floats(0, 0), pa.float64()),
        "nan": pa.array(floats(0.05, 0), pa.float64()),
        "null": pa.array(floats(0, 0.05), pa.float64()),
        "late_nan": pa.array([1.5] * (n - 1) + [float("nan")], pa.float64()),
        "ok": pa.array([maybe(rng.random() < 0.5, 0.1) for _ in range(n)], pa.bool_()),
        "word": pa.array([maybe(rng.choice(["a", "b", "é", "long text"]), 0.1) for _ in range(n)], pa.string()),
        "full": pa.array([rng.choice(["x", "y"]) for _ in range(n)], pa.string()),
        "day": pa.array([maybe(datetime.date(2000, 1, 1) + datetime.timedelta(days=rng.randrange(9000)), 0.1) for _ in range(n)], pa.date32()),
        "short": pa.array([rng.randrange(-9, 9) for _ in range(n)], pa.int16()),
        "single": pa.array([rng.uniform(-9, 9) for _ in range(n)], pa.float32()),
        "unsigned": pa.array([rng.randrange(0, 9) for _ in range(n)], pa.uint64()),
        "money": pa.array([decimal.Decimal("1.25")] * n, pa.decimal128(10, 2)),
        "stamp": pa.array([datetime.datetime(2020, 1, 1)] * n, pa.timestamp("us")),
        "raw": pa.array([b"\x00\x01"] * n, pa.binary()),
    }
)
required = pa.schema([f.with_nullable(False) if f.name in ("id", "full", "clean") else f for f in table.schema])
table = table.cast(required)

for codec, dictionary, version, stats in itertools.product(
    ["none", "snappy", "gzip", "zstd", "lz4"], [True, False], ["1.0", "2.0"], [True, False]
):
    name = f"{codec}-{'dict' if dictionary else 'plain'}-v{version}-{'stats' if stats else 'nostats'}.parquet"
    pq.write_table(
        table, os.path.join(out, name), compression=codec, use_dictionary=dictionary,
        data_page_version=version, write_statistics=stats, row_group_size=1000, data_page_size=4096,
    )

pq.write_table(table, os.path.join(out, "split.parquet"), use_dictionary=False,
               use_byte_stream_split=["clean", "nan", "null", "late_nan"])
pl.from_arrow(table.select(["id", "count", "clean", "nan", "null", "ok", "word", "day"])).write_parquet(
    os.path.join(out, "polars.parquet"))
pl.from_arrow(table.select(["id", "count", "clean", "nan", "null", "ok", "word", "day"])).write_parquet(
    os.path.join(out, "polars-nostats.parquet"), statistics=False, row_group_size=500)
pq.write_table(pa.table({"nested": pa.array([{"a": 1}] * 10), "after": pa.array(range(10), pa.int64())}),
               os.path.join(out, "nested.parquet"))

