val write
  :  entry:Lock.entry
  -> program_sha256:string
  -> locations:Document.location list
  -> output:string
  -> code:string
  -> Syntax.declaration list
  -> unit
