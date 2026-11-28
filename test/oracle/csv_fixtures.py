import csv
import os
import random
import sys

out = sys.argv[1]
os.makedirs(out, exist_ok=True)
rng = random.Random(11)
pieces = ["a", "b", "zone 9", ",", '"', '""', "\n", "\r\n", "é", "∑", "x" * 40, "12", "-3.5", "TRUE", " "]


def field():
    return "".join(rng.choice(pieces) for _ in range(rng.randrange(1, 6)))


for name, rows, columns, terminator in [("lf", 4000, 5, "\n"), ("crlf", 3000, 3, "\r\n"), ("wide", 500, 60, "\n")]:
    with open(os.path.join(out, name + ".csv"), "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, lineterminator=terminator, quoting=csv.QUOTE_MINIMAL)
        writer.writerow([f"c{i}" for i in range(columns)])
        for _ in range(rows):
            writer.writerow([field() for _ in range(columns)])
