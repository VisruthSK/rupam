"""Run with uv run python -m unittest discover -s test."""
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

import polars as pl

ROOT = Path(__file__).resolve().parents[1]


class ContractIntegration(unittest.TestCase):
    def test_compiled_script_revalidates_before_filtering(self):
        subprocess.run(["dune", "build", "bin/rupam_cli.exe"], cwd=ROOT, check=True)
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for name in ("shop.rp", "orders.parquet", "data-dict.yaml"):
                shutil.copy(ROOT / "examples" / name, work / name)
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            script = work / "shop.py"
            script.write_bytes(subprocess.check_output(
                [compiler, "compile", "python", "shop.rp"], cwd=work))
            subprocess.run(["python", script], cwd=work, check=True)
            output = work / "regional.parquet"
            result = pl.read_parquet(output)
            self.assertEqual(result.rows(), [("North", 2, 10.), ("South", 1, 5.)])
            self.assertEqual(result.dtypes, [pl.String, pl.Int64, pl.Float64])
            output.unlink()
            r_script = work / "shop.R"
            r_script.write_bytes(subprocess.check_output(
                [compiler, "compile", "r", "shop.rp"], cwd=work))
            subprocess.run(["Rscript", r_script], cwd=work, check=True)
            r_output = pl.read_parquet(output)
            self.assertEqual(r_output.schema, result.schema)
            self.assertEqual(r_output.rows(), result.rows())
            output.unlink()
            source = work / "orders.parquet"
            data = pl.read_parquet(source).with_columns(
                pl.when(pl.col("revenue") == 0).then(None).otherwise(pl.col("revenue")).alias("revenue"))
            data.write_parquet(source)
            failure = subprocess.run(["python", script], cwd=work, capture_output=True)
            self.assertNotEqual(failure.returncode, 0)
            self.assertIn(b"validate-data", failure.stderr)
            self.assertFalse(output.exists())

