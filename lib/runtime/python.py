import calendar
import datetime
import functools
import io
import json
import math
import re
import sys

for stream in (sys.stdout, sys.stderr):
    if isinstance(stream, io.TextIOWrapper):
        stream.reconfigure(encoding="utf-8")


class RupamError(Exception):
    def __init__(self, code, message):
        super().__init__(message)
        self.code = code


def rp_int(x):
    if not -(2**63) < x < 2**63:
        raise RupamError("arithmetic-overflow", "arithmetic overflow: Int out of range")
    return x


def rp_real(x):
    if math.isinf(x) or math.isnan(x):
        raise RupamError("arithmetic-overflow", "arithmetic overflow: a result is not finite")
    return x


def rp_dynamic(v):
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, float):
        return repr(v)
    if isinstance(v, str):
        return json.dumps(v, ensure_ascii=False)
    if v is None:
        return "None"
    if isinstance(v, tuple) and v and isinstance(v[0], str) and v[0][:1].isupper():
        tag, args = v[0], v[1:]
        return tag if not args else tag + "(" + ", ".join(map(rp_dynamic, args)) + ")"
    if isinstance(v, tuple):
        return "(" + ", ".join(map(rp_dynamic, v)) + ")"
    if isinstance(v, list):
        return "[" + ", ".join(map(rp_dynamic, v)) + "]"
    if isinstance(v, dict):
        return "{" + ", ".join(f"{k} = {rp_dynamic(x)}" for k, x in v.items()) + "}"
    if callable(v):
        return "<function>"
    return str(v)


def rp_format(v, shape):
    if shape == "int":
        return str(v)
    if shape == "float":
        return repr(v)
    if shape == "bool":
        return "TRUE" if v else "FALSE"
    if shape == "string":
        return json.dumps(v, ensure_ascii=False)
    if shape == "date":
        return 'd"' + v.isoformat() + '"'
    if shape == "duration":
        return f"days({v.days})"
    if shape == "period":
        return f"months({v})"
    if shape == "function":
        return "<function>"
    if isinstance(shape, str):
        return rp_dynamic(v)
    kind = shape[0]
    if kind == "list":
        return "[" + ", ".join(rp_format(x, shape[1]) for x in v) + "]"
    if kind == "option":
        return "None" if v is None else rp_format(v, shape[1])
    if kind == "tuple":
        return "(" + ", ".join(rp_format(x, s) for x, s in zip(v, shape[1:])) + ")"
    if kind == "record":
        return "{" + ", ".join(f"{name} = {rp_format(v[name], s)}" for name, s in shape[1]) + "}"
    return rp_dynamic(v)


def rp_show(value, shape):
    return rp_format(value, json.loads(shape))


def rp_exact_float(i):
    f = float(i)
    if int(f) != i:
        raise RupamError("inexact-float", f"{i} has no exact Float value")
    return f


def rp_seq(first, last, step=1):
    if step == 0:
        raise ValueError("seq needs a nonzero step")
    out = []
    v = first
    while (step > 0 and v <= last) or (step < 0 and v >= last):
        out.append(v)
        v = first + len(out) * step
    return out


def rp_element(x, i):
    n = len(x)
    i = n if i is None else i
    if i < 1 or i > n:
        raise IndexError(f"index {i} is outside 1..{n}")
    return x[i - 1]


def rp_slice(x, a, b):
    n = len(x)
    a = 1 if a is None else a
    b = n if b is None else b
    if a < 1 or b > n:
        raise IndexError(f"slice {a}..={b} is outside 1..{n}")
    return x[a - 1 : b]



def rp_add_months(d, n):
    total = d.year * 12 + d.month - 1 + n
    year, month = divmod(total, 12)
    month += 1
    return datetime.date(year, month, min(d.day, calendar.monthrange(year, month)[1]))


def rp_parse_date(text, fmt):
    try:
        return datetime.datetime.strptime(text, fmt).date()
    except ValueError:
        return None


def rp_and(a, b):
    if a is False or b is False:
        return False
    return None if a is None or b is None else True


def rp_or(a, b):
    if a is True or b is True:
        return True
    return None if a is None or b is None else False


def rp_vreduce(kind, policy, xs):
    present = [v for v in xs if v is not None]
    if policy == "error" and len(present) < len(xs):
        raise RupamError("missing-under-error", "A reduction met a missing value and missing = error")
    if policy == "keep" and kind not in ("n_distinct", "first", "last") and len(present) < len(xs):
        return None
    kept = xs if policy == "keep" else present
    if kind in ("first", "last"):
        return (kept[0] if kind == "first" else kept[-1]) if kept else None
    if kind == "n_distinct":
        return len(dict.fromkeys(kept))
    if not present or (kind in ("var", "sd") and len(present) < 2):
        return None
    if kind == "sum":
        total = sum(present)
        return total
    if kind in ("min", "max"):
        return min(present) if kind == "min" else max(present)
    floats = [float(v) for v in present]
    mean = sum(floats) / len(floats)
    if kind == "mean":
        return mean
    if kind == "median":
        s = sorted(floats)
        n = len(s)
        return s[n // 2] if n % 2 else (s[n // 2 - 1] + s[n // 2]) / 2
    variance = sum((v - mean) ** 2 for v in floats) / (len(floats) - 1)
    return variance if kind == "var" else math.sqrt(variance)
