import re
import sys

text = sys.stdin.read()
edits = {}
for block in re.split(r"(?m)^@@@ FILE ", text)[1:]:
    header, _, body = block.partition("\n")
    path = header.strip()
    pairs = edits.setdefault(path, [])
    for chunk in re.split(r"(?m)^<<<<\n", body)[1:]:
        old, _, rest = chunk.partition("\n====\n")
        new, _, _ = rest.partition("\n>>>>")
        pairs.append((old, new))

contents = {}
for path, pairs in edits.items():
    s = open(path, encoding="utf-8").read()
    for old, new in pairs:
        if s.count(old) != 1:
            sys.exit(f"{path}: expected one match, found {s.count(old)}:\n{old[:200]}")
        s = s.replace(old, new, 1)
    contents[path] = s
for path, s in contents.items():
    open(path, "w", encoding="utf-8", newline="\n").write(s)
print("edited " + ", ".join(contents))
