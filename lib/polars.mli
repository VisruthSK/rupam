val table
  :  scalar:(Core.expr -> Python_syntax.expr)
  -> name:(Core.id -> string)
  -> Core.table
  -> Python_syntax.expr
