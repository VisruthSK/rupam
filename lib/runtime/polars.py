import hashlib
import io
from pathlib import Path

import polars as pl
import pyarrow as pa
import pyarrow.csv as pacsv
import pyarrow.parquet as pq

ARROW_TYPES = {"Int64": pa.int64(), "Float64": pa.float64(), "Bool": pa.bool_(), "String": pa.string()}


def read_locked(path, digest, format, columns):
    data = Path(path).read_bytes()
    if hashlib.sha256(data).hexdigest() != digest:
        raise ValueError(f"{path} changed since rupam lock: run rupam lock, then recompile")
    if format == "parquet":
        table = pq.read_table(io.BytesIO(data), columns=list(columns))
    else:
        table = pacsv.read_csv(
            io.BytesIO(data),
            parse_options=pacsv.ParseOptions(delimiter="\t" if format == "tsv" else ","),
            convert_options=pacsv.ConvertOptions(
                column_types={name: ARROW_TYPES[kind] for name, kind in columns.items()},
                null_values=[""],
                strings_can_be_null=True,
            ),
        )
    return pl.from_arrow(table).with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()


RESULTS = {}


def rp_result(frame, id):
    RESULTS[id] = frame.collect()
    return RESULTS[id].height


def rp_page(id, offset, limit, path):
    RESULTS[id].slice(offset, limit).write_parquet(path)


def rp_release(id):
    RESULTS.pop(id, None)


def rp_finite(series):
    if (series.is_infinite() | series.is_nan()).any():
        raise ArithmeticError("arithmetic overflow: a result is not finite")
    return series


def rp_covered(left, right, key, kind):
    sides = {"left": [(left, right)], "right": [(right, left)]}.get(kind, [(left, right), (right, left)])
    for rows, other in sides:
        if rows.join(other, on=key, how="anti", nulls_equal=False).select(pl.len()).collect().item():
            raise ValueError(f"{kind}_join with unmatched = error found rows with no match on {key}")
    return left


def rp_assert_key(frame, columns):
    if frame.select(pl.any_horizontal(pl.col(columns).is_null().any())).collect().item():
        raise ValueError(f"{', '.join(columns)} is not a key: some rows are missing it")
    if frame.select(pl.struct(columns).is_duplicated().any()).collect().item():
        raise ValueError(f"{', '.join(columns)} is not a key: some rows repeat it")
    return frame


def rp_show_float(series):
    return pl.Series([None if v is None else repr(v) for v in series], dtype=pl.String)


def rp_formatter(spec):
    return lambda series: pl.Series([None if v is None else format(v, spec) for v in series], dtype=pl.String)


def rp_present(series):
    if series.null_count() > 0:
        raise ValueError("filter condition is missing and missing = error")
    return series


def rp_wider(data, ids, key, value, labels):
    frame = data.collect()
    if frame.filter(pl.col(key).is_null() | ~pl.col(key).is_in(labels)).height:
        raise ValueError("Unexpected pivot key")
    return frame.lazy().pivot(key, labels, index=ids, values=value)
