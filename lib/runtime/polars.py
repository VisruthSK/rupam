import json
import shutil
import subprocess
import tempfile
from pathlib import Path

import polars as pl
import pyarrow as pa
import pyarrow.csv as pacsv
import pyarrow.parquet as pq





def rp_present(series):
    if series.null_count() > 0:
        raise ValueError("filter condition is missing and missing = error")
    return series


def rp_wider(data, ids, key, value, labels):
    frame = data.collect()
    if frame.filter(pl.col(key).is_null() | ~pl.col(key).is_in(labels)).height:
        raise ValueError("Unexpected pivot key")
    return frame.lazy().pivot(key, labels, index=ids, values=value)


def dictionary_json(command, path):
    result = subprocess.run(["data-dict", *command, str(path)], capture_output=True)
    if result.returncode:
        try:
            report = json.loads(result.stdout)
            codes = sorted({p["code"] for p in report.get("problems", []) if "code" in p})
        except (ValueError, TypeError):
            codes = []
        raise ValueError(f"data-dict {command[0]} failed: " + ", ".join(codes))
    return json.loads(result.stdout)


def validated_source(dictionary, expected, name, physical_types):
    original = Path(dictionary).resolve()
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        snapshot = root / original.name
        shutil.copyfile(original, snapshot)
        model = dictionary_json(["export-spec"], snapshot)
        if model != json.loads(expected):
            raise ValueError("Contract changed since compilation")
        paths = {}
        for table in model["tables"]:
            relative = Path(table["source"]["parquet"])
            if relative.is_absolute() or ".." in relative.parts:
                raise ValueError("Snapshot requires a dictionary-relative source")
            destination = root / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(original.parent / relative, destination)
            paths[table["name"]] = destination
        for level in ["validate-meta", "validate-data"]:
            report = dictionary_json([level, "--json"], snapshot)
            if report["run"]["tool"]["version"] != "0.0.3":
                raise ValueError("Unsupported data-dict validator version")
            if report["status"] != "ok" or any(s["outcome"] != "pass" for s in report["steps"]):
                raise ValueError(f"Incomplete {level} validation")
        schema = pl.read_parquet_schema(paths[name])
        if {k: str(v) for k, v in schema.items()} != physical_types:
            raise ValueError("Physical schema changed since compilation")
        fields = next(t["columns"] for t in model["tables"] if t["name"] == name)
        table = pq.read_table(paths[name], columns=[c["name"] for c in fields])
        return pl.from_arrow(table).with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()


def validate_contract(dictionary):
    for level in ["validate-meta", "validate-data"]:
        report = dictionary_json([level, "--json"], dictionary)
        if report["status"] != "ok" or any(s["outcome"] != "pass" for s in report["steps"]):
            raise ValueError(f"Incomplete {level} validation")


ARROW_TYPES = {"Int64": pa.int64(), "Float64": pa.float64(), "Boolean": pa.bool_(), "String": pa.string()}
ARROW_KINDS = {"int64": "Int64", "double": "Float64", "bool": "Boolean", "string": "String", "large_string": "String"}


def read_file(path, format, schema):
    if format == "parquet":
        table = pq.read_table(path)
    else:
        table = pacsv.read_csv(
            path,
            parse_options=pacsv.ParseOptions(delimiter="	" if format == "tsv" else ","),
            convert_options=pacsv.ConvertOptions(
                column_types={name: ARROW_TYPES[kind] for name, kind in schema.items()},
                null_values=[""],
                strings_can_be_null=True,
            ),
        )
    found = {field.name: ARROW_KINDS.get(str(field.type), str(field.type)) for field in table.schema}
    if list(found) != list(schema) or found != schema:
        raise ValueError(f"{path} no longer matches the schema it was compiled against")
    return pl.from_arrow(table).with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()
