type id =
  { name : string
  ; stamp : int
  }

type shape =
  | Scalar
  | Columnar

type policy =
  | Skip
  | Keep
  | Fail

type constant =
  | Int of int64
  | Float of float
  | Number of string
  | Text of string
  | Bool of bool

type direction =
  | Ascending
  | Descending

type join_kind =
  | Inner
  | Left
  | Right
  | Full
  | Semi
  | Anti

type relationship =
  | One_to_one
  | Many_to_one
  | One_to_many
  | Many_to_many

type expr =
  { node : node
  ; typ : Types.t
  ; shape : shape
  ; span : Syntax.span
  }

and node =
  | Constant of constant
  | Absent
  | Variable of id
  | Column of string
  | Builtin of Prelude.row * expr list
  | Reduce of Prelude.reduction * policy * expr
  | Count
  | Unary of Syntax.unary * expr
  | Binary of Syntax.binary * expr * expr
  | Convert of expr
  | Lift of (id * expr) list * expr
  | If of expr * expr * expr
  | Coalesce of expr * expr
  | Require of expr
  | Match of expr * arm list
  | Lambda of id list * expr
  | Apply of expr * expr list
  | Construct of string * expr list
  | Let of id * expr * expr
  | Tuple of expr list
  | List of expr list
  | Record of (string * expr) list
  | Field of expr * string
  | Member of expr * expr
  | Foreign of string * string * expr list
  | Text of piece list
  | Table of table

and arm =
  { pattern : pattern
  ; guard : expr option
  ; body : expr
  }

and pattern =
  | Wildcard
  | Bind of id
  | Equals of constant
  | Missing
  | Present of pattern
  | Between of constant option * constant option * bool
  | Tag of string * pattern list
  | Elements of pattern list
  | Fields of (string * id) list
  | Either of pattern list
  | Named of id * pattern

and piece =
  | Fixed of string
  | Shown of expr * string option

and table =
  { source : source
  ; stages : stage list
  ; schema : (string * Types.t) list
  ; rest : Types.t option
  ; keys : string list list
  ; order : string list option
  ; restricted : string list
  }

and source =
  | Read of file
  | Load of
      { contract : Contract.t
      ; table : Contract.table
      ; file : file
      }
  | Bound of id
  | Join of join

and file =
  { path : string
  ; digest : string
  ; format : Data_file.format
  ; columns : (string * Types.t) list
  }

and join =
  { left : table
  ; right : table
  ; key : string
  ; kind : join_kind
  ; relationship : relationship option
  ; covered : bool
  }

and stage =
  { step : step
  ; at : Syntax.span
  }

and step =
  | Filter of expr
  | Mutate of
      { name : string
      ; value : expr
      ; by : string list
      }
  | Select of string list
  | Rename of (string * string) list
  | Summarize of
      { by : string list
      ; measures : (string * expr) list
      }
  | Arrange of (expr * direction) list
  | Take of int
  | Distinct of string list
  | Pivot_longer of
      { ids : string list
      ; columns : string list
      ; names_to : string
      ; values_to : string
      }
  | Pivot_wider of
      { ids : string list
      ; names_from : string
      ; values_from : string
      ; levels : string list
      }
  | Assert_key of string list

type item =
  | Define of id * expr
  | Variant of string * (string * int) list
  | Show of expr
  | Write of expr * string

val join_name : join_kind -> string
val join_kinds : (string * join_kind) list
val relationship_name : relationship -> string
val relationships : (string * relationship) list
val policies : (string * policy) list
val names : table -> string list
val row : table -> Types.row
val exists : (expr -> bool) -> expr -> bool
val columns : expr -> string list
val reduces : expr -> bool
val sources : table -> string list
val files : table -> file list
val contracts : table -> string list
val substitute : id -> expr -> expr -> expr
val tabular : item list -> bool
