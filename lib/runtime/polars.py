import polars as pl
import pyarrow as pa
import pyarrow.csv as pacsv
import pyarrow.parquet as pq

ARROW_TYPES = {"Int64": pa.int64(), "Float64": pa.float64(), "Bool": pa.bool_(), "String": pa.string(), "Date": pa.date32()}


def rp_read(path, format, columns):
    if format == "parquet":
        table = pq.read_table(path, columns=list(columns))
    else:
        table = pacsv.read_csv(
            path,
            parse_options=pacsv.ParseOptions(ignore_empty_lines=False, delimiter="\t" if format == "tsv" else ","),
            convert_options=pacsv.ConvertOptions(
                column_types={name: ARROW_TYPES[kind] for name, kind in columns.items()},
                null_values=[""],
                strings_can_be_null=True,
            ),
        )
    return pl.from_arrow(table).with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()


def rp_result(frame, path):
    data = frame.collect()
    data.write_ipc(path, compression="uncompressed", compat_level=pl.CompatLevel.oldest())
    return data.height


def rp_show_float(series):
    return pl.Series([None if v is None else repr(v) for v in series], dtype=pl.String)


def rp_formatter(spec):
    return lambda series: pl.Series([None if v is None else format(v, spec) for v in series], dtype=pl.String)


def rp_wider(data, ids, key, value, labels):
    frame = data.collect()
    if frame.filter(pl.col(key).is_null() | ~pl.col(key).is_in(labels)).height:
        raise ValueError("Unexpected pivot key")
    return frame.lazy().pivot(key, labels, index=ids, values=value)
