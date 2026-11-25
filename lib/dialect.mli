val program
  :  Project.dialect
  -> strict:bool
  -> directory:string
  -> Syntax.declaration list
  -> string * Document.location list

val value : Project.dialect -> Syntax.declaration list -> string
val table : Project.dialect -> Analysis_program.dataset -> string

val preview
  :  Project.dialect
  -> runtime:Runtime.t
  -> Analysis_program.dataset
  -> offset:int
  -> limit:int
  -> Page.t
