import re
import sys
from pathlib import Path

root = Path(sys.argv[1])
internal = {"unix", "str", "threads"}
project = (root / "dune-project").read_text()
declared = set(re.findall(r"^\s*\(?([a-z][a-z0-9_-]*)", project, re.M))
used = set()
for dune in root.rglob("dune"):
    if any(part in {"_build", "_opam", "node_modules", ".worktrees"} for part in dune.parts):
        continue
    internal.update(re.findall(r"\(library\s+\(name\s+([a-z0-9_]+)\)", dune.read_text()))
    if "vendor" in dune.parts:
        continue
    for libraries in re.findall(r"\((?:libraries|pps)([^)]*)\)", dune.read_text()):
        used.update(name.split(".")[0] for name in libraries.split())
missing = sorted(name for name in used - internal if name not in declared and name.replace("_", "-") not in declared)
if missing:
    sys.exit("dune-project does not declare: " + ", ".join(missing))
