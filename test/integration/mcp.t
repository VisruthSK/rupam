The developer MCP negotiates its single protocol revision per request and serves compiler tools.

  $ cat > requests <<'JSON'
  > {"jsonrpc":"2.0","id":"discovery","method":"server/discover","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc": "2.0", "method": "notifications/example"}
  > {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2025-11-25","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":3,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {invalid
  > {"jsonrpc":"2.0","id":4,"method":"ping","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"evaluate_reference","arguments":{"source":"let square(x) = x * x\nsquare(3)"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"transform_ir","arguments":{"source":"let x = 3"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"check","arguments":{"source":"unbound_name"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"code","arguments":{"source":"3","target":"other"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > {"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"conformance","arguments":{"id":"C01"},"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}}}
  > JSON
  $ (cd "$INSIDE_DUNE" && rupam-dev mcp) < requests | cut -c1-300
  {"jsonrpc":"2.0","id":"discovery","result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"supportedVersions":["2026-07-28"],"capabilities":{"tools":{}}}}
  {"jsonrpc":"2.0","id":2,"error":{"code":-32022,"message":"Unsupported protocol version. Supported: 2026-07-28","data":{"supported":["2026-07-28"],"requested":"2025-11-25"}}}
  {"jsonrpc":"2.0","id":3,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"tools":[{"name":"check","description":"Check a Rupam program.","annotations":{"readOnlyHint":true},"inputSchema":{"type":"object","properties":{"source
  {"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error"}}
  {"jsonrpc":"2.0","id":4,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}}}}
  {"jsonrpc":"2.0","id":5,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"content":[{"type":"text","text":"{\n  \"bindings\": {\n    \"square\": { \"type\": \"(Number) -> Number\", \"value\": \"<function>\" }\n  },\n  \"shown
  {"jsonrpc":"2.0","id":6,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"content":[{"type":"text","text":"(Define ((name x) (stamp 1))\n ((node (Constant (Int 3))) (typ Int) (shape Scalar) (span 1:9-1:10)))"}],"structuredCon
  {"jsonrpc":"2.0","id":7,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"content":[{"type":"text","text":"{\n  \"category\": \"diagnostic\",\n  \"code\": \"unknown-name\",\n  \"message\": \"Unknown name: unbound_name\",\n  \
  {"jsonrpc":"2.0","id":8,"error":{"code":-32602,"message":"Invalid tool name or arguments"}}
  {"jsonrpc":"2.0","id":9,"result":{"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"rupam-dev","version":"0.0.0-dev"}},"content":[{"type":"text","text":"{ \"id\": \"C01\", \"status\": \"passed\", \"problems\": [] }"}],"structuredContent":{"value":{"id":"C01","status":"pa
