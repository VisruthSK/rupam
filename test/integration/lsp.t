The language server reports diagnostics and completes top-level names.

  $ python lsp.py rupam lsp
  {"id": 1, "jsonrpc": "2.0", "result": {"capabilities": {"completionProvider": {}, "textDocumentSync": 1}}}
  {"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics", "params": {"diagnostics": [], "uri": "file:///sample.rp", "version": 1}}
  {"id": 2, "jsonrpc": "2.0", "result": [{"detail": "(Num) -> Num", "label": "square"}]}
  {"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics", "params": {"diagnostics": [{"message": "Expected Bool, found Num", "range": {"end": {"character": 16, "line": 0}, "start": {"character": 8, "line": 0}}, "severity": 1, "source": "rupam"}], "uri": "file:///sample.rp", "version": 2}}
  {"id": 3, "jsonrpc": "2.0", "result": null}
