type row =
  | Abs
  | Sign
  | Sqrt
  | Exp
  | Log
  | Log2
  | Log10
  | Floor
  | Ceiling
  | Trunc
  | Round
  | Pmin
  | Pmax
  | Present
  | Absent
  | Length
  | Lower
  | Upper
  | Trim
  | Starts
  | Ends
  | Detect

type reduction =
  | Sum
  | Mean
  | Median
  | Var
  | Sd
  | Min
  | Max
  | Distinct
  | Count

type builtin =
  | Row of row
  | Reduction of reduction
  | Coalesce

val reductions : (string * reduction) list
val scalar : (string * builtin) list
val all : (string * builtin) list
val name : builtin -> string
val find : string -> builtin option
