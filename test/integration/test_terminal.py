"""Real terminal interaction and accepted-source regressions for Rupam."""
import errno
import json
import os
from pathlib import Path
import select
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import time
import unittest

if os.name == "posix":
    import fcntl
    import pty
    import termios

    import pyte


ROOT = Path(__file__).resolve().parents[2]
EXAMPLES = ROOT / "examples"
RUPAM = Path(os.environ.get("RUPAM_TEST_BIN", ROOT / "_build/default/bin/rupam_cli.exe")).resolve()
DEV = Path(os.environ.get("RUPAM_DEV_TEST_BIN", ROOT / "_build/default/dev/rupam_dev.exe")).resolve()
RIGHT = b"\x1b[C"
LEFT = b"\x1b[D"
ENTER = b"\r"
TAB = b"\t"


class Terminal:
    def __init__(self, directory, *args, width=100, height=30, env=None):
        self.screen = pyte.Screen(width, height)
        self.stream = pyte.ByteStream(self.screen)
        self.pid, self.fd = pty.fork()
        if not self.pid:
            os.chdir(directory)
            os.environ.update(
                TERM="xterm-256color",
                XDG_CONFIG_HOME=str(directory / "config"),
                XDG_STATE_HOME=str(directory / "state"),
            )
            os.environ.update(env or {})
            os.execv(RUPAM, ["rupam", *args])
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", height, width, 0, 0))
        self.screen.write_process_input = lambda reply: self.send(reply.encode())

    def __enter__(self):
        return self

    def __exit__(self, *_):
        try:
            os.kill(self.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        os.waitpid(self.pid, 0)
        os.close(self.fd)

    @property
    def lines(self):
        return [line.rstrip() for line in self.screen.display]

    def until(self, condition):
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            if callable(condition):
                if condition(self.lines):
                    return
            elif any(condition in line for line in self.lines):
                return
            if select.select([self.fd], [], [], 0.05)[0]:
                try:
                    data = os.read(self.fd, 65536)
                except OSError as e:
                    if e.errno != errno.EIO:
                        raise
                    break
                if not data:
                    break
                self.stream.feed(data)
        raise AssertionError(f"Missing {condition!r} on terminal:\n" + "\n".join(self.lines))

    def send(self, data):
        os.write(self.fd, data)

    def submit(self, command):
        self.send(command.encode() + ENTER)

    def paste(self, source):
        self.send(b"\x1b[200~" + source.encode() + b"\x1b[201~")

    def point(self, text):
        return next((line.index(text) + 1, y + 1) for y, line in enumerate(self.lines) if text in line)

    def painted_cell(self, text):
        x, y = self.point(text)
        return self.screen.buffer[y - 1][x - 1]

    def click(self, text):
        x, y = self.point(text)
        self.send(f"\x1b[<0;{x};{y}M\x1b[<0;{x};{y}m".encode())

    def wheel_down(self, text):
        x, y = self.point(text)
        self.send(f"\x1b[<65;{x};{y}M".encode())


@unittest.skipUnless(os.name == "posix", "uses a Unix pseudo-terminal")
class InteractiveRepl(unittest.TestCase):
    def test_completion_menu_filters_while_typing(self):
        with tempfile.TemporaryDirectory() as tmp, Terminal(
            Path(tmp), "--target", "python-polars"
        ) as terminal:
            terminal.until("rupam ❯")
            terminal.send(b":h" + TAB)
            terminal.until("show REPL commands")
            terminal.send(b"\x15")
            terminal.until(lambda lines: not any("show REPL commands" in line for line in lines))
            terminal.send(b":" + TAB)
            terminal.until("first rows")
            terminal.send(b"t")
            terminal.until(lambda lines: any("show or set the target" in line for line in lines)
                           and any("inferred type" in line for line in lines)
                           and not any("first rows" in line for line in lines))

    def test_grid_details_pipeline_and_saved_source(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            for name in ("shop.rp", "orders.parquet", "data-dict.yaml"):
                shutil.copyfile(EXAMPLES / name, directory / name)
            with Terminal(directory, "repl", "--target", "python-polars", "shop.rp") as terminal:
                terminal.until("rupam ❯")
                terminal.submit(":view orders")
                terminal.until("Rupam pipeline")
                terminal.until("customer_id = 1")
                terminal.send(RIGHT)
                terminal.until('region = "North"')
                terminal.send(b"SSS")
                terminal.until("Preview 1")
                terminal.send(LEFT)
                terminal.send(b"S")
                terminal.until("Preview 2")
                terminal.until("South")
                terminal.click("South")
                terminal.until('region = "South"')
                terminal.send(RIGHT * 2)
                terminal.until("cost = 1.0")
                header = next(line for line in terminal.lines if "customer_id" in line and "revenue" in line)
                header_line = terminal.lines.index(header)
                self.assertIn("│", header)
                self.assertIn("cost", header)
                terminal.click("d Details")
                terminal.until("Skewness")
                terminal.until("Q1 0.5   Median 1   Q3 1.5")
                terminal.until("Mean 1   SD 1")
                self.assertEqual(terminal.lines[header_line], header)
                self.assertTrue(any("South" in line and "1.00" in line for line in terminal.lines))
                self.assertNotEqual(terminal.painted_cell("None").fg,
                                    terminal.painted_cell("1.00").fg)
                terminal.send(b"fj")
                terminal.until("cost = 0.0")
                self.assertFalse(any("1 Skip" in line for line in terminal.lines))
                terminal.send(b"k")
                terminal.until("cost = 1.0")
                terminal.send(b"=")
                terminal.until("1 Skip")
                terminal.send(b"1" + ENTER)
                terminal.until("let view_result")
                terminal.submit(":save saved.rp")
                terminal.until("saved.rp")
                saved = directory / "saved.rp"
                self.assertTrue(saved.exists(), "\n".join(terminal.lines))
                terminal.submit(":quit")
            source = saved.read_text(encoding="utf-8")
            viewer = source.split("let view_result", 1)[1]
            self.assertEqual(viewer.count("arrange("), 1, viewer)
            self.assertIn("desc($region), desc($customer_id)", viewer)
            self.assertIn("filter($cost == 1.0, missing = skip)", viewer)
            checked = subprocess.run([RUPAM, "check", saved],
                                     cwd=directory, capture_output=True, text=True, timeout=60)
            self.assertEqual(checked.returncode, 0, checked.stderr)
            ir = subprocess.run([DEV, "transform-ir", "--binding", "view_result", saved],
                                cwd=directory, capture_output=True, text=True, timeout=60)
            self.assertEqual(ir.returncode, 0, ir.stderr)
            self.assertEqual(ir.stdout.count("(Arrange"), 1, ir.stdout)
            self.assertEqual(ir.stdout.count("(Filter"), 1, ir.stdout)
            with saved.open("a", encoding="utf-8") as file:
                file.write('\nwrite_parquet(view_result, "viewer-result.parquet")\n')
            for target in ("python-polars", "r-dplyr"):
                with self.subTest(target=target):
                    for command in ("lock", "run"):
                        execution = subprocess.run([RUPAM, command, "--target", target, saved],
                                                   cwd=directory, capture_output=True, text=True, timeout=60)
                        self.assertEqual(execution.returncode, 0, execution.stderr)
                    output = subprocess.check_output(
                        [sys.executable, "-c",
                         "import json, pyarrow.parquet as pq; "
                         "print(json.dumps(pq.read_table('viewer-result.parquet').to_pylist()))"],
                        cwd=directory, text=True, timeout=60,
                    )
                    result = json.loads(output)
                    self.assertEqual(result, [{"customer_id": 4, "region": "South",
                                               "revenue": 0.0, "cost": 1.0}])

    def test_multiline_draft_retains_grid_and_defers_writes(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            for name in ("viewer-penguins.rp", "penguins.csv"):
                shutil.copyfile(EXAMPLES / name, directory / name)
            with Terminal(directory, "repl", "--target", "python-polars",
                          "viewer-penguins.rp") as terminal:
                terminal.until("rupam ❯")
                terminal.submit(":view penguins")
                terminal.until("344 rows")
                terminal.until("39.10")
                terminal.wheel_down("39.10")
                terminal.until(lambda lines: any(line.startswith("│  4 ") for line in lines))
                terminal.send(b"e")
                terminal.until("Tab Complete")
                terminal.paste("\n    |> ")
                terminal.until("incomplete-input")
                self.assertTrue(any("344 rows" in line for line in terminal.lines))
                terminal.paste("filter(body_mass > 4000, missing = skip)")
                terminal.until("Preview 2")
                self.assertFalse(any("344 rows" in line for line in terminal.lines))
                terminal.paste('\n    |> write_parquet("unapproved.parquet")')
                terminal.until("not-a-table")
                self.assertFalse((directory / "unapproved.parquet").exists())
                terminal.send(b"\x1b")
                terminal.until("Enter Save")
                terminal.send(b"\x1b")
                terminal.until(lambda lines: not any("Rupam pipeline" in line for line in lines))
                terminal.submit(":bindings")
                terminal.until("penguins :")
                self.assertFalse(any("view_result" in line for line in terminal.lines))

    def test_value_filter_uses_exact_typed_cell(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / "exact.csv").write_text(
                "measurement\n0.30000000000000004\n0.3\n", encoding="utf-8"
            )
            with Terminal(directory, "--target", "python-polars") as terminal:
                terminal.until("rupam ❯")
                terminal.submit('let exact = read_csv("exact.csv")')
                terminal.submit(":view exact")
                terminal.until("measurement = 0.30000000000000004")
                self.assertEqual(sum("0.30" in line and "│" in line for line in terminal.lines), 2)
                terminal.send(b"=")
                terminal.until("1 row × 1 column")
                terminal.until("$measurement == 0.30000000000000004")

    def test_escape_cancels_preview_without_killing_repl_worker(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            for name in ("viewer-penguins.rp", "penguins.csv"):
                shutil.copyfile(EXAMPLES / name, directory / name)
            slow, started = directory / "slow", directory / "started"
            python = directory / "python"
            python.write_text(
                "#!/bin/sh\n"
                'if [ "$1" = -u ] && [ -f "$TEST_SLOW" ]; then\n'
                '  printf "%s\\n" "$$" > "$TEST_STARTED"\n'
                '  exec "$TEST_PYTHON" -c "import time; time.sleep(60)"\n'
                "fi\n"
                'exec "$TEST_PYTHON" "$@"\n',
                encoding="utf-8",
            )
            python.chmod(0o755)
            env = {
                "PATH": str(directory) + os.pathsep + os.environ["PATH"],
                "TEST_SLOW": str(slow),
                "TEST_STARTED": str(started),
                "TEST_PYTHON": sys.executable,
            }
            with Terminal(directory, "repl", "--target", "python-polars",
                          "viewer-penguins.rp", env=env) as terminal:
                terminal.until("rupam ❯")
                terminal.submit(":view penguins")
                terminal.until("Rupam pipeline")
                slow.touch()
                terminal.send(b"s")
                terminal.until(lambda _: started.exists() and started.stat().st_size > 0)
                pid = int(started.read_text(encoding="utf-8"))
                try:
                    slow.unlink()
                    terminal.send(b"S")
                    terminal.until("Preview 2")
                    with self.assertRaises(ProcessLookupError):
                        os.kill(pid, 0)
                    terminal.send(b"q")
                    terminal.until(lambda lines: not any("Rupam pipeline" in line for line in lines))
                    terminal.submit(":view penguins")
                    terminal.until("Rupam pipeline")
                    started.unlink()
                    slow.touch()
                    terminal.send(b"s")
                    terminal.until(lambda _: started.exists() and started.stat().st_size > 0)
                    pid = int(started.read_text(encoding="utf-8"))
                    terminal.send(b"\x1b")
                    terminal.until(lambda lines: not any("Rupam pipeline" in line for line in lines))
                    with self.assertRaises(ProcessLookupError):
                        os.kill(pid, 0)
                    terminal.submit(":preview penguins")
                    terminal.until("344 rows")
                finally:
                    try:
                        os.kill(pid, signal.SIGTERM)
                    except ProcessLookupError:
                        pass


if __name__ == "__main__":
    unittest.main()
