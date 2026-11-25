import json
import subprocess
import sys

uri = "file:///sample.rp"
messages = [
    {"id": 1, "method": "initialize", "params": {}},
    {"method": "textDocument/didOpen", "params": {"textDocument": {"uri": uri, "version": 1, "text": "let square(x)=x*x"}}},
    {"id": 2, "method": "textDocument/completion", "params": {"textDocument": {"uri": uri}}},
    {"method": "textDocument/didChange", "params": {"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": "let x = TRUE + 1"}]}},
    {"id": 3, "method": "shutdown", "params": {}},
    {"method": "exit"},
]
request = b""
for message in messages:
    data = json.dumps(dict(jsonrpc="2.0", **message)).encode()
    request += f"Content-Length: {len(data)}\r\n\r\n".encode() + data
output = subprocess.run(sys.argv[1:], input=request, capture_output=True, check=True).stdout
while output:
    header, rest = output.split(b"\r\n\r\n", 1)
    size = int(header.split(b":")[1])
    print(json.dumps(json.loads(rest[:size]), sort_keys=True))
    output = rest[size:]
