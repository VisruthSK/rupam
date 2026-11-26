The language server reports diagnostics and completes top-level names.

  $ cat > requests <<'JSON'
  > {"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
  > {"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///sample.rp","version":1,"text":"let square(x)=x*x"}}}
  > {"jsonrpc":"2.0","id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///sample.rp"}}}
  > {"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///sample.rp","version":2},"contentChanges":[{"text":"let x = TRUE + 1"}]}}
  > {"jsonrpc":"2.0","id":3,"method":"shutdown","params":{}}
  > {"jsonrpc":"2.0","method":"exit"}
  > JSON
  $ rupam-dev frame < requests | rupam lsp | rupam-dev unframe
  {"id":1,"jsonrpc":"2.0","result":{"capabilities":{"completionProvider":{},"textDocumentSync":1}}}
  {"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"diagnostics":[],"uri":"file:///sample.rp","version":1}}
  {"id":2,"jsonrpc":"2.0","result":[{"detail":"(number) -> number","label":"square"}]}
  {"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"diagnostics":[{"message":"Expected Bool, found number","range":{"end":{"character":16,"line":0},"start":{"character":8,"line":0}},"severity":1,"source":"rupam"}],"uri":"file:///sample.rp","version":2}}
  {"id":3,"jsonrpc":"2.0","result":null}
