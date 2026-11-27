import datetime
import json
import math
import os
import random
import sys

import polars as pl
import pyarrow as pa

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
rng = random.Random(20261127)


def write(name, table, batch_sizes=None):
    path = os.path.join(out, name + ".arrow")
    with pa.OSFile(path, "wb") as sink, pa.ipc.new_file(sink, table.schema) as writer:
        if batch_sizes is None:
            writer.write_table(table)
        else:
            whole = table.combine_chunks().to_batches()[0]
            start = 0
            for size in batch_sizes:
                writer.write_batch(whole.slice(start, size))
                start += size


def maybe(value, p=0.2):
    return None if rng.random() < p else value


def text():
    alphabet = "abcXYZ019 ,\"'\n\té∑🙂"
    return "".join(rng.choice(alphabet) for _ in range(rng.randrange(0, 12)))


n = 1000
base = pa.table(
    {
        "i64": pa.array([maybe(rng.randrange(-(2**63) + 1, 2**63)) for _ in range(n)], pa.int64()),
        "f64": pa.array(
            [maybe(rng.choice([rng.uniform(-1e9, 1e9), 0.0, -0.0, 1e-310, math.inf, -math.inf, 5e-324]))
             for _ in range(n)],
            pa.float64(),
        ),
        "flag": pa.array([maybe(rng.random() < 0.5) for _ in range(n)], pa.bool_()),
        "name": pa.array([maybe(text()) for _ in range(n)], pa.string()),
        "day": pa.array([maybe(datetime.date(1900, 1, 1) + datetime.timedelta(days=rng.randrange(0, 80000)))
                         for _ in range(n)], pa.date32()),
    }
)
write("single_batch", base)
sizes = []
left = n
while left:
    size = min(left, rng.choice([0, 1, 7, 64, 200]))
    sizes.append(size)
    left -= size
write("many_batches", base, sizes + [0])
write("no_nulls", base.drop_null())
write("empty", base.slice(0, 0))

ints = pa.table(
    {
        "i8": pa.array([maybe(rng.randrange(-128, 128)) for _ in range(n)], pa.int8()),
        "i16": pa.array([maybe(rng.randrange(-(2**15), 2**15)) for _ in range(n)], pa.int16()),
        "i32": pa.array([maybe(rng.randrange(-(2**31), 2**31)) for _ in range(n)], pa.int32()),
        "u8": pa.array([maybe(rng.randrange(0, 2**8)) for _ in range(n)], pa.uint8()),
        "u16": pa.array([maybe(rng.randrange(0, 2**16)) for _ in range(n)], pa.uint16()),
        "u32": pa.array([maybe(rng.randrange(0, 2**32)) for _ in range(n)], pa.uint32()),
        "f32": pa.array([maybe(rng.uniform(-1e30, 1e30)) for _ in range(n)], pa.float32()),
        "large": pa.array([maybe(text()) for _ in range(n)], pa.large_string()),
    }
)
write("widths", ints, [300, 0, 700])

frame = pl.DataFrame(
    {
        "id": [maybe(rng.randrange(-(2**40), 2**40)) for _ in range(n)],
        "score": [maybe(rng.uniform(-5, 5)) for _ in range(n)],
        "ok": [maybe(rng.random() < 0.5) for _ in range(n)],
        "label": [maybe(text()) for _ in range(n)],
        "when": [maybe(datetime.date(2000, 1, 1) + datetime.timedelta(days=rng.randrange(0, 9000)))
                 for _ in range(n)],
    },
    schema={"id": pl.Int64, "score": pl.Float64, "ok": pl.Boolean, "label": pl.String, "when": pl.Date},
)
frame.write_ipc(os.path.join(out, "polars.arrow"), compression="uncompressed",
                compat_level=pl.CompatLevel.oldest())
pl.concat([frame] * 5, rechunk=False).write_ipc(
    os.path.join(out, "polars_chunks.arrow"), compression="uncompressed",
    compat_level=pl.CompatLevel.oldest())

levels = [maybe(rng.choice(["low", "mid", "high", "é"])) for _ in range(n)]
dictionary = pa.table({"level": pa.array(levels, pa.string()).dictionary_encode()})
write("dictionary", dictionary, [400, 0, 600])
epoch = datetime.date(1970, 1, 1)


def plain(v):
    if isinstance(v, float) and (math.isnan(v) or math.isinf(v)):
        return "nan" if math.isnan(v) else ("inf" if v > 0 else "-inf")
    if isinstance(v, datetime.date):
        return (v - epoch).days
    return v


for name in sorted(os.listdir(out)):
    if name.endswith(".arrow"):
        table = pa.ipc.open_file(os.path.join(out, name)).read_all()
        columns = [{"name": f.name, "values": [plain(v) for v in c.to_pylist()]}
                   for f, c in zip(table.schema, table.columns)]
        with open(os.path.join(out, name[: -len(".arrow")] + ".json"), "w", encoding="utf-8") as f:
            json.dump(columns, f)
