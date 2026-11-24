import json
import shutil
import subprocess
import tempfile
from pathlib import Path

import polars as pl





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
        frame = pl.read_parquet(paths[name], columns=[c["name"] for c in fields])
        return frame.with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()


def validate_contract(dictionary):
    for level in ["validate-meta", "validate-data"]:
        report = dictionary_json([level, "--json"], dictionary)
        if report["status"] != "ok" or any(s["outcome"] != "pass" for s in report["steps"]):
            raise ValueError(f"Incomplete {level} validation")


def read_file(path, format, schema):
    dtypes = {name: getattr(pl, kind) for name, kind in schema.items()}
    if format == "parquet":
        frame = pl.read_parquet(path)
    else:
        frame = pl.read_csv(
            path,
            separator="\t" if format == "tsv" else ",",
            schema_overrides=dtypes,
            null_values=[""],
        )
    if {k: str(v) for k, v in frame.schema.items()} != schema:
        raise ValueError(f"{path} no longer matches the schema it was compiled against")
    return frame.with_columns(pl.col(pl.Float64).fill_nan(None)).lazy()
