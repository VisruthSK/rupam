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
  | Round_digits
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
  | Size
  | Reverse
  | Sorted
  | Deduplicate
  | Map_each
  | Retain
  | Fold
  | Sequence
[@@deriving sexp_of]

type reduction =
  | Sum
  | Mean
  | Median
  | Var
  | Sd
  | Min
  | Max
  | Distinct
  | First
  | Last
  | Count
[@@deriving sexp_of]

type builtin =
  | Row of row
  | Reduction of reduction
  | Coalesce

val reductions : (string * reduction) list
val missing_is_value : reduction -> bool
val positional : reduction -> bool
val scalar : (string * builtin) list
val all : (string * builtin) list
val name : builtin -> string
val find : string -> builtin option
