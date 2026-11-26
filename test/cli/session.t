The session protocol checks, submits against an expected revision and rejects stale requests.

  $ cat > requests <<'JSON'
  > {"jsonrpc":"2.0","id":1,"method":"check","params":{"source":"let y = 1.5 * 2"}}
  > {"jsonrpc":"2.0","id":2,"method":"submit","params":{"revision":0,"source":"let f(x) = x + 1"}}
  > {"jsonrpc":"2.0","id":3,"method":"submit","params":{"revision":0,"source":"let g = 1"}}
  > {"jsonrpc":"2.0","id":4,"method":"check","params":{"source":"f(\"a\")"}}
  > {"jsonrpc":"2.0","id":5,"method":"bindings"}
  > {"jsonrpc":"2.0","id":6,"method":"nonsense"}
  > {"jsonrpc":"2.0","id":7,"method":"undo","params":{"revision":1}}
  > {"jsonrpc":"2.0","id":8,"method":"bindings"}
  > {"jsonrpc":"2.0","id":9,"method":"target","params":{"target":"python-polars"}}
  > {"jsonrpc":"2.0","id":10,"method":"code","params":{"source":"read_csv(\"penguins.csv\") |> filter(0.0 < bill_len <= 40.0)"}}
  > {"jsonrpc":"2.0","id":11,"method":"complete","params":{"source":"read_csv(\"penguins.csv\") |> filter(bill"}}
  > JSON
  $ cp ../../examples/penguins.csv .
  $ rupam session < requests
  {"jsonrpc":"2.0","id":1,"result":{"revision":0,"value":[{"name":"y","type":"Float"}]}}
  {"jsonrpc":"2.0","id":2,"result":{"revision":1,"value":[{"kind":"bound","name":"f","type":"(Number) -> Number"}]}}
  {"jsonrpc":"2.0","id":3,"error":{"code":-32000,"message":"Expected revision 0, the session is at 1","data":{"category":"superseded","revision":1}}}
  {"jsonrpc":"2.0","id":4,"error":{"code":-32000,"message":"Expected Number, found String","data":{"category":"diagnostic","code":"type-mismatch","message":"Expected Number, found String","span":{"start":{"line":1,"column":3},"end":{"line":1,"column":6}}}}}
  {"jsonrpc":"2.0","id":5,"result":{"revision":1,"value":[{"name":"f","type":"(Number) -> Number"}]}}
  {"jsonrpc":"2.0","id":6,"error":{"code":-32602,"message":"Unknown method nonsense","data":{"category":"protocol","revision":1}}}
  {"jsonrpc":"2.0","id":7,"result":{"revision":0,"value":true}}
  {"jsonrpc":"2.0","id":8,"result":{"revision":0,"value":[]}}
  {"jsonrpc":"2.0","id":9,"result":{"revision":0,"value":"python-polars"}}
  {"jsonrpc":"2.0","id":10,"error":{"code":-32000,"message":"This condition is missing where its inputs are missing. Say what happens to those rows: missing = skip drops them, missing = keep keeps them, missing = error stops the run","data":{"category":"diagnostic","code":"optional-condition","message":"This condition is missing where its inputs are missing. Say what happens to those rows: missing = skip drops them, missing = keep keeps them, missing = error stops the run","span":{"start":{"line":1,"column":36},"end":{"line":1,"column":58}}}}}
  {"jsonrpc":"2.0","id":11,"result":{"revision":0,"value":{"start":35,"suggestions":[{"text":"bill_len","detail":"Float?"},{"text":"bill_dep","detail":"Float?"}]}}}
