"""Run with uv run python -m unittest discover -s test."""
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

import polars as pl

ROOT = Path(__file__).resolve().parents[1]
DIALECT = {"r": "r-dplyr", "python": "python-polars"}


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
                [compiler, "compile", "--target", "python-polars", "shop.rp"], cwd=work))
            subprocess.run(["python", script], cwd=work, check=True)
            output = work / "regional.parquet"
            result = pl.read_parquet(output)
            self.assertEqual(result.rows(), [("North", 2, 10.), ("South", 1, 5.)])
            self.assertEqual(result.dtypes, [pl.String, pl.Int64, pl.Float64])
            output.unlink()
            r_script = work / "shop.R"
            r_script.write_bytes(subprocess.check_output(
                [compiler, "compile", "--target", "r-dplyr", "shop.rp"], cwd=work))
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


    def test_join_uses_named_keys_and_preserves_unmatched_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for name in ("orders.parquet", "data-dict.yaml"):
                shutil.copy(ROOT / "examples" / name, work / name)
            program = work / "join.rp"
            program.write_text("""use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
let lefts = orders |> select(customer_id, cost)
let rights = orders |> select(cost, region)
let joined = lefts |> join(rights, on = cost, how = left) |> arrange(customer_id)
write_parquet(joined, "joined.parquet")
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            for target, runner in [("python", "python"), ("r", "Rscript")]:
                with self.subTest(target=target):
                    script = work / ("join." + target)
                    script.write_bytes(subprocess.check_output(
                        [compiler, "compile", "--target", DIALECT[target], program], cwd=work))
                    subprocess.run([runner, script], cwd=work, check=True)
                    result = pl.read_parquet(work / "joined.parquet")
                    self.assertEqual(result["region"].to_list(), [None, "South", "North", "South"])
                    (work / "joined.parquet").unlink()
            program.write_text(program.read_text().replace("select(cost, region)", "select(cost, customer_id)"))
            error = subprocess.run([compiler, "compile", "--target", "python-polars", program], cwd=work, capture_output=True)
            self.assertNotEqual(error.returncode, 0)
            self.assertIn(b"collision", error.stderr)

    def test_private_fields_and_derivatives_are_excluded_from_exports(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            shutil.copy(ROOT / "examples/orders.parquet", work)
            dictionary = (ROOT / "examples/data-dict.yaml").read_text()
            (work / "data-dict.yaml").write_text(dictionary.replace(
                "- name: cost", "- name: cost\n        display: restricted"))
            (work / "private.rp").write_text("""use contract "data-dict.yaml" as shop
let orders = load(shop.orders) |> mutate(discount = cost * 0.5)
write_parquet(orders, "result.parquet")
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            for target, runner in [("python", "python"), ("r", "Rscript")]:
                with self.subTest(target=target):
                    script = work / ("private." + target)
                    script.write_bytes(subprocess.check_output(
                        [compiler, "compile", "--target", DIALECT[target], "private.rp"], cwd=work))
                    subprocess.run([runner, script], cwd=work, check=True)
                    result = pl.read_parquet(work / "result.parquet")
                    self.assertEqual(result.columns, ["customer_id", "region", "revenue"])
                    (work / "result.parquet").unlink()

    def test_repl_previews_in_both_persistent_workers(self):
        compiler = ROOT / "_build/default/bin/rupam_cli.exe"
        commands = ":schema regional\n:preview regional\n:target r-dplyr\n:preview regional\n:preview regional\n:quit\n"
        completed = subprocess.run([compiler, "repl", "--target", "python-polars", ROOT / "examples/shop.rp"],
                                   input=commands, text=True, capture_output=True, check=True)
        self.assertEqual(completed.stdout.count("North"), 3, completed.stderr)
        self.assertIn("total_margin  Float64?", completed.stdout)

    def test_polars_checked_branch_and_artifacts(self):
        import json

        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for name in ("orders.parquet", "data-dict.yaml"):
                shutil.copy(ROOT / "examples" / name, work)
            (work / "checked.rp").write_text("""use contract "data-dict.yaml" as shop
let orders = load(shop.orders)
let result = orders |> mutate(x = if (customer_id > 0) customer_id else customer_id + 9223372036854775807) |> select(x)
write_parquet(result, "checked.parquet")
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            subprocess.run([compiler, "compile", "--target", "python-polars", "checked.rp", "-o", "checked.py"], cwd=work, check=True)
            subprocess.run(["python", "checked.py"], cwd=work, check=True)
            self.assertEqual(pl.read_parquet(work / "checked.parquet")["x"].to_list(), [1, 2, 3, 4])
            manifest = json.loads((work / "checked.py.manifest.json").read_text())
            self.assertNotIn("fallback", manifest["fidelity"])
            generated = (work / "checked.py").read_text()
            self.assertNotIn("map_elements", generated)
            mappings = json.loads((work / "checked.py.map.json").read_text())["mappings"]
            lines = generated.splitlines()
            self.assertTrue(any(m["start"]["line"] == 3 and
                                ".with_columns(" in lines[m["generated_line"] - 1]
                                for m in mappings))

    def test_target_capability_requires_import_and_appears_in_manifest(self):
        import json

        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for name in ("orders.parquet", "data-dict.yaml"):
                shutil.copy(ROOT / "examples" / name, work)
            source = work / "sqrt.rp"
            body = """use contract "data-dict.yaml" as shop
let t = load(shop.orders) |> mutate(root = polars.sqrt(revenue)) |> select(root)
write_parquet(t, "sqrt.parquet")
"""
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            source.write_text(body)
            denied = subprocess.run([compiler, "compile", "--target", "python-polars", source], cwd=work, capture_output=True)
            self.assertNotEqual(denied.returncode, 0)
            self.assertIn(b"Unknown name: polars", denied.stderr)
            source.write_text("import polars\n" + body)
            strict = subprocess.run([compiler, "compile", "--target", "python-polars", source], cwd=work, capture_output=True)
            self.assertNotEqual(strict.returncode, 0)
            self.assertIn(b"strict", strict.stderr)
            subprocess.run([compiler, "compile", "--no-strict", "--target", "python-polars", source, "-o", "sqrt.py"], cwd=work, check=True)
            subprocess.run(["python", "sqrt.py"], cwd=work, check=True)
            self.assertEqual(pl.read_parquet(work / "sqrt.parquet")["root"][2], 3.)
            manifest = json.loads((work / "sqrt.py.manifest.json").read_text())
            self.assertEqual(manifest["nonportable"], ["polars.sqrt"])
            denied = subprocess.run([compiler, "compile", "--no-strict", "--target", "r-dplyr", source], cwd=work, capture_output=True)
            self.assertNotEqual(denied.returncode, 0)
            self.assertIn(b"polars.sqrt", denied.stderr)

    def test_scalar_variant_on_both_targets(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            source = work / "variant.rp"
            source.write_text("""type Maybe<a> = | Empty | Just(value: a)
let get(x) = match x { Empty => 0, Just(n) => n }
get(Just(9007199254740993))
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            for target,runner in [("r","Rscript"),("python","python")]:
                script = work / ("variant." + target)
                script.write_bytes(subprocess.check_output([compiler,"compile","--target",DIALECT[target],source]))
                self.assertIn(b"9007199254740993",subprocess.check_output([runner,script]))

    def test_editor_diagnostics_and_completion(self):
        import json

        uri = "file:///sample.rp"
        messages = [
            {"id": 1, "method": "initialize", "params": {}},
            {"method": "textDocument/didOpen", "params": {"textDocument": {
                "uri": uri, "version": 1, "text": "let square(x)=x*x"}}},
            {"id": 2, "method": "textDocument/completion", "params": {"textDocument": {"uri": uri}}},
            {"method": "textDocument/didChange", "params": {"textDocument": {"uri": uri, "version": 2},
                "contentChanges": [{"text": "let x = TRUE + 1"}]}},
            {"id": 3, "method": "shutdown", "params": {}}, {"method": "exit"}]
        body = bytearray()
        for item in messages:
            data = json.dumps(dict(jsonrpc="2.0", **item)).encode()
            body.extend(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
        compiler = ROOT / "_build/default/bin/rupam_cli.exe"
        output = subprocess.check_output([compiler, "lsp"], input=bytes(body))
        replies = []
        while output:
            header, data = output.split(b"\r\n\r\n", 1)
            size = int(header.split(b":")[1])
            replies.append(json.loads(data[:size]))
            output = data[size:]
        self.assertEqual(replies[1]["params"]["diagnostics"], [])
        self.assertEqual(replies[2]["result"][0]["label"], "square")
        self.assertEqual(replies[3]["params"]["version"], 2)
        self.assertEqual(len(replies[3]["params"]["diagnostics"]), 1)

    def test_grouped_windows_and_pivot_round_trip(self):
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            for name in ("orders.parquet", "data-dict.yaml"):
                shutil.copy(ROOT / "examples" / name, work)
            source = work / "pivot.rp"
            source.write_text("""use contract "data-dict.yaml" as shop
let t = load(shop.orders) |> mutate(center = revenue - mean(revenue), by = region)
let tall = t |> select(customer_id, revenue, center) |> pivot_longer(cols = (revenue, center), names_to = metric, values_to = reading)
let wide = tall |> pivot_wider(names_from = metric, values_from = reading, levels = [revenue, center]) |> rename(center = centered) |> arrange(customer_id)
write_parquet(wide, "result.parquet")
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            for target,runner in [("python","python"),("r","Rscript")]:
                script=work / ("pivot." + target)
                script.write_bytes(subprocess.check_output([compiler,"compile","--target",DIALECT[target],source],cwd=work))
                subprocess.run([runner,script],cwd=work,check=True)
                result=pl.read_parquet(work / "result.parquet")
                self.assertEqual(result.rows(),[(1,3.,-3.),(2,5.,2.5),(3,9.,3.),(4,0.,-2.5)])

    def test_nan_in_sources_reads_as_missing(self):

        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            shutil.copy(ROOT / "examples/data-dict.yaml", work)
            pl.DataFrame({
                "customer_id": [1, 2, 3, 4], "region": ["North"] * 4,
                "revenue": [1.] * 4, "cost": [None, float("nan"), None, float("nan")]
            }).write_parquet(work / "orders.parquet")
            source = work / "groups.rp"
            source.write_text("""use contract "data-dict.yaml" as shop
let t = load(shop.orders) |> summarize(n=count(), by=cost)
write_parquet(t, "groups.parquet")
""")
            compiler = ROOT / "_build/default/bin/rupam_cli.exe"
            for target, runner in [("r", "Rscript"), ("python", "python")]:
                script = work / ("groups." + target)
                script.write_bytes(subprocess.check_output([compiler,"compile","--target",DIALECT[target],source],cwd=work))
                subprocess.run([runner, script],cwd=work,check=True)
                rows = pl.read_parquet(work / "groups.parquet").rows()
                self.assertEqual(rows, [(None, 4)])



class FileSources(unittest.TestCase):
    def test_csv_and_parquet_without_contract_agree_across_dialects(self):
        compiler = ROOT / "_build/default/bin/rupam_cli.exe"
        big = 9007199254740993
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            (work / "visits.csv").write_text(f"id,site,value\n{big},North,2.5\n2,North,\n3,South,4.0\n")
            pl.DataFrame({"id": [big, 2], "score": [float("nan"), 1.5]}).write_parquet(work / "scores.parquet")
            (work / "files.rp").write_text("""let visits = read_csv("visits.csv")
let scores = read_parquet("scores.parquet")
write_parquet(visits |> select(id, site), "ids.parquet")
write_parquet(scores, "scores_out.parquet")
""")
            for target in ("python-polars", "r-dplyr"):
                with self.subTest(target=target):
                    subprocess.run([compiler, "run", "--target", target, "files.rp"], cwd=work,
                                   check=True, capture_output=True)
                    ids = pl.read_parquet(work / "ids.parquet").sort("site")
                    self.assertEqual(ids["id"].to_list()[0], big)
                    scores = pl.read_parquet(work / "scores_out.parquet")
                    self.assertEqual(scores["id"].to_list(), [big, 2])
                    self.assertEqual(scores["score"].to_list(), [None, 1.5])
