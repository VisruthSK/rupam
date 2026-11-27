open Sexplib0.Sexp_conv

type id =
  { name : string
  ; stamp : int
  }
[@@deriving sexp_of]

type shape =
  | Scalar
  | Columnar
[@@deriving sexp_of]

type policy =
  | Skip
  | Keep
  | Fail
[@@deriving sexp_of]

type constant =
  | Int of int64
  | Float of float
  | Number of string
  | Text of string
  | Bool of bool
  | Date of int
[@@deriving sexp_of]

type direction =
  | Ascending
  | Descending
[@@deriving sexp_of]

type join_kind =
  | Inner
  | Left
  | Right
  | Full
  | Semi
  | Anti
[@@deriving sexp_of]

type relationship =
  | One_to_one
  | Many_to_one
  | One_to_many
  | Many_to_many
[@@deriving sexp_of]

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
  | Guarded of expr * expr
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
  | Sequence of expr * expr
  | Element of expr * expr option
  | Slice of expr * expr option * expr option
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
  | Between of (constant * bool) option * (constant * bool) option
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
      { contract : (Contract.t[@sexp.opaque])
      ; table : (Contract.table[@sexp.opaque])
      ; file : file
      }
  | Bound of id
  | Join of join

and file =
  { path : string
  ; digest : string
  ; format : Input.format
  ; columns : (string * Types.t) list
  ; missing : string list
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
[@@deriving sexp_of]

type item =
  | Define of id * expr
  | Variant of string * (string * int) list
  | Show of expr
  | Write of expr * string
[@@deriving sexp_of]

let join_name = function
  | Inner -> "inner"
  | Left -> "left"
  | Right -> "right"
  | Full -> "full"
  | Semi -> "semi"
  | Anti -> "anti"
;;

let join_kinds =
  List.map (fun k -> join_name k ^ "_join", k) [ Inner; Left; Right; Full; Semi; Anti ]
;;

let relationship_name = function
  | One_to_one -> "one_to_one"
  | Many_to_one -> "many_to_one"
  | One_to_many -> "one_to_many"
  | Many_to_many -> "many_to_many"
;;

let relationships =
  List.map
    (fun r -> relationship_name r, r)
    [ One_to_one; Many_to_one; One_to_many; Many_to_many ]
;;

let policy_name = function
  | Skip -> "skip"
  | Keep -> "keep"
  | Fail -> "error"
;;

let policies = List.map (fun p -> policy_name p, p) [ Skip; Keep; Fail ]
let names table = List.map fst table.schema
let row table = { Types.fields = table.schema; rest = table.rest }

let rec exists f e =
  f e
  ||
  let any = List.exists (exists f) in
  match e.node with
  | Constant _ | Absent | Variable _ | Column _ | Count -> false
  | Table t -> exists_table f t
  | Builtin (_, xs) | Construct (_, xs) | Foreign (_, _, xs) | Tuple xs | List xs ->
    any xs
  | Reduce (_, _, x) | Unary (_, x) | Convert x | Require x | Lambda (_, x) | Field (x, _)
    -> exists f x
  | Binary (_, a, b)
  | Coalesce (a, b)
  | Let (_, a, b)
  | Member (a, b)
  | Sequence (a, b)
  | Guarded (a, b) -> any [ a; b ]
  | Element (x, i) -> any (x :: Option.to_list i)
  | Slice (x, a, b) -> any ((x :: Option.to_list a) @ Option.to_list b)
  | If (a, b, c) -> any [ a; b; c ]
  | Lift (bound, body) -> any (body :: List.map snd bound)
  | Match (x, arms) ->
    any (x :: List.concat_map (fun a -> a.body :: Option.to_list a.guard) arms)
  | Apply (g, xs) -> any (g :: xs)
  | Record fs -> any (List.map snd fs)
  | Text pieces ->
    any
      (List.filter_map
         (function
           | Shown (x, _) -> Some x
           | Fixed _ -> None)
         pieces)

and exists_table f t =
  (match t.source with
   | Join j -> exists_table f j.left || exists_table f j.right
   | Read _ | Load _ | Bound _ -> false)
  || List.exists
       (fun s ->
          match s.step with
          | Filter x | Mutate { value = x; _ } -> exists f x
          | Summarize { measures; _ } -> List.exists (fun (_, x) -> exists f x) measures
          | Arrange keys -> List.exists (fun (x, _) -> exists f x) keys
          | Select _
          | Rename _
          | Take _
          | Distinct _
          | Pivot_longer _
          | Pivot_wider _
          | Assert_key _ -> false)
       t.stages
;;

let columns e =
  let found = ref [] in
  ignore
    (exists
       (fun e ->
          (match e.node with
           | Column n when not (List.mem n !found) -> found := n :: !found
           | _ -> ());
          false)
       e);
  List.rev !found
;;

let reduces =
  exists (fun e ->
    match e.node with
    | Reduce _ | Count -> true
    | _ -> false)
;;

let rec sources table =
  match table.source with
  | Read file -> [ file.path ]
  | Load { contract; file; _ } -> [ contract.path; file.path ]
  | Bound _ -> []
  | Join { left; right; _ } -> sources left @ sources right
;;

let rec files table =
  match table.source with
  | Read file -> [ file ]
  | Load _ | Bound _ -> []
  | Join { left; right; _ } -> files left @ files right
;;

let rec loaded_by table =
  match table.source with
  | Load { contract; _ } -> [ contract ]
  | Read _ | Bound _ -> []
  | Join { left; right; _ } -> loaded_by left @ loaded_by right
;;

let contracts table = List.map (fun (c : Contract.t) -> c.path) (loaded_by table)

let loaded items =
  let found = ref [] in
  let visit e =
    ignore
      (exists
         (fun e ->
            (match e.node with
             | Table t -> found := loaded_by t @ !found
             | _ -> ());
            false)
         e)
  in
  List.iter
    (function
      | Define (_, e) | Show e | Write (e, _) -> visit e
      | Variant _ -> ())
    items;
  List.sort_uniq (fun (a : Contract.t) b -> compare a.path b.path) !found
;;

let rec substitute (id : id) (value : expr) (e : expr) : expr =
  let go = substitute id value in
  let node : node =
    match e.node with
    | Variable v when v.stamp = id.stamp -> value.node
    | Builtin (r, xs) -> Builtin (r, List.map go xs)
    | Unary (op, x) -> Unary (op, go x)
    | Binary (op, a, b) -> Binary (op, go a, go b)
    | Convert x -> Convert (go x)
    | If (a, b, c) -> If (go a, go b, go c)
    | Coalesce (a, b) -> Coalesce (go a, go b)
    | Apply (f, xs) -> Apply (go f, List.map go xs)
    | Text pieces ->
      Text
        (List.map
           (function
             | Shown (x, f) -> Shown (go x, f)
             | p -> p)
           pieces)
    | node -> node
  in
  { e with node }
;;

let tabular items =
  List.exists
    (function
      | Define (_, e) | Show e | Write (e, _) ->
        exists
          (fun e ->
             match e.node with
             | Table _ -> true
             | _ -> false)
          e
      | Variant _ -> false)
    items
;;
