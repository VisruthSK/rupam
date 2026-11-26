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
    cases = [json.loads(line) for line in run.stdout.splitlines()]
    if sorted(case for case, _ in cases) != ["C119", "C123", "C124", "C52", "C56"]:
        raise SystemExit(f"Unexpected Lean fixtures: {cases}")
    for case, value in cases:
        with (fixtures / f"{case}.toml").open("rb") as file:
            expected = tomllib.load(file)["expect"]["value"]
        if value != expected:
            raise SystemExit(f"{case}: expected {expected!r}, got {value!r}")
    print(f"Lean conformance: {len(cases)} fixtures passed")


if __name__ == "__main__":
    main()
