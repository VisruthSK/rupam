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

let rows =
  [ "abs", Abs
  ; "sign", Sign
  ; "sqrt", Sqrt
  ; "exp", Exp
  ; "log", Log
  ; "log2", Log2
  ; "log10", Log10
  ; "floor", Floor
  ; "ceiling", Ceiling
  ; "trunc", Trunc
  ; "round", Round
  ; "round", Round_digits
  ; "pmin", Pmin
  ; "pmax", Pmax
  ; "some", Present
  ; "none", Absent
  ; "str_length", Length
  ; "str_to_lower", Lower
  ; "str_to_upper", Upper
  ; "str_trim", Trim
  ; "str_starts", Starts
  ; "str_ends", Ends
  ; "str_detect", Detect
  ; "length", Size
  ; "rev", Reverse
  ; "sort", Sorted
  ; "unique", Deduplicate
  ; "map", Map_each
  ; "keep", Retain
  ; "reduce", Fold
  ; "seq", Sequence
  ]
;;

let reductions =
  [ "sum", Sum
  ; "mean", Mean
  ; "median", Median
  ; "var", Var
  ; "sd", Sd
  ; "min", Min
  ; "max", Max
  ; "n_distinct", Distinct
  ; "first", First
  ; "last", Last
  ; "n", Count
  ]
;;

let missing_is_value = function
  | Distinct | First | Last -> true
  | Sum | Mean | Median | Var | Sd | Min | Max | Count -> false
;;

let positional = function
  | First | Last -> true
  | Sum | Mean | Median | Var | Sd | Min | Max | Distinct | Count -> false
;;

let scalar = ("coalesce", Coalesce) :: List.map (fun (n, r) -> n, Row r) rows
let all = scalar @ List.map (fun (n, r) -> n, Reduction r) reductions
let name builtin = fst (List.find (fun (_, b) -> b = builtin) all)
let find n = List.assoc_opt n all
