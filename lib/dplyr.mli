module R = R_syntax

val preview
  :  runtime:Runtime.t
  -> Analysis_program.dataset
  -> offset:int
  -> limit:int
  -> Page.t

val program
  :  strict:bool
  -> directory:string
  -> Syntax.declaration list
  -> string * Document.location list

val table : Analysis_program.dataset -> string
