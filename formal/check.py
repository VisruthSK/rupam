"""Check executable Lean cases against Rupam's shared conformance expectations."""

import json
import subprocess
import tomllib
from pathlib import Path


def main() -> None:
    formal = Path(__file__).resolve().parent
    fixtures = formal.parent / "test" / "conformance"
    subprocess.run(["lake", "build"], cwd=formal, check=True)
    run = subprocess.run(
        ["lake", "env", "lean", "-DwarningAsError=true", "--run", "Fixtures.lean"],
        cwd=formal,
        capture_output=True,
        text=True,
    )
    if run.returncode:
        raise SystemExit(run.stdout + run.stderr)
    cases: dict[str, dict[str, str]] = {}
    for line in run.stdout.splitlines():
        case, field, value = json.loads(line)
        observed = cases.setdefault(case, {})
        if field in observed:
            raise SystemExit(f"{case}: duplicate outcome field {field}")
        observed[field] = value
    if set(cases) != {
        "C110", "C119", "C123", "C124", "C138", "C139", "C37", "C52", "C53", "C55", "C56"
    }:
        raise SystemExit(f"Unexpected Lean fixtures: {cases}")
    for case, observed in cases.items():
        with (fixtures / f"{case}.toml").open("rb") as file:
            expected = tomllib.load(file)["expect"]
        if observed != expected:
            raise SystemExit(f"{case}: expected {expected!r}, got {observed!r}")
    print(f"Lean conformance: {len(cases)} fixtures passed")


if __name__ == "__main__":
    main()
