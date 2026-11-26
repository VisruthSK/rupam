val table
  :  scalar:(TransformIR.expr -> Python_syntax.expr)
  -> name:(TransformIR.id -> string)
  -> TransformIR.table
  -> Python_syntax.expr
