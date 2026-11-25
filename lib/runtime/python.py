import json
import math
import re


def rp_float(x):
    for digits in (15, 16, 17):
        text = "%.*g" % (digits, x)
        if float(text) == x:
            break
    return text if any(c in text for c in ".eni") else text + ".0"


def rp_dynamic(v):
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, float):
        return rp_float(v)
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
        return rp_float(v)
    if shape == "bool":
        return "TRUE" if v else "FALSE"
    if shape == "string":
        return json.dumps(v, ensure_ascii=False)
    if shape == "function":
        return "<function>"
    if isinstance(shape, str):
        return rp_dynamic(v)
    kind = shape[0]
    if kind == "list":
        return "[" + ", ".join(rp_format(x, shape[1]) for x in v) + "]"
    if kind == "option":
        return "None" if v is None else "Some(" + rp_format(v, shape[1]) + ")"
    if kind == "tuple":
        return "(" + ", ".join(rp_format(x, s) for x, s in zip(v, shape[1:])) + ")"
    if kind == "record":
        return "{" + ", ".join(f"{name} = {rp_format(v[name], s)}" for name, s in shape[1]) + "}"
    return rp_dynamic(v)


def rp_show(value, shape):
    return rp_format(value, json.loads(shape))
