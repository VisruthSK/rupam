val write
  :  entry:Lock.entry
  -> program_blake3:string
  -> locations:Document.location list
  -> output:string
  -> code:string
  -> TransformIR.item list
  -> unit
