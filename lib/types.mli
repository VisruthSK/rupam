type kind =
  | Any
  | Number

type t =
  | Int64
  | Float64
  | Bool
  | String
  | Option of t
  | List of t
  | Tuple of t list
  | Record of row
  | Table of row
  | Variant of string * t list
  | Arrow of t list * t
  | Var of var ref

and row =
  { fields : (string * t) list
  ; rest : t option
  }

and var =
  | Unbound of
      { level : int
      ; kind : kind
      }
  | Link of t

type scheme =
  { quantified : var ref list
  ; body : t
  }

exception Mismatch

val fresh : ?kind:kind -> int -> t
val mono : t -> scheme
val closed : (string * t) list -> row
val repr : t -> t
val optional : t -> t
val present : t -> t
val is_optional : t -> bool
val resolve : t -> t
val resolve_row : row -> row
val default_numbers : t -> unit
val display : t -> string
val numeric : t -> bool
val unify : t -> t -> unit
val generalize : int -> t -> scheme
val instantiate : int -> scheme -> t
val field : int -> row -> string -> t option
val known : row -> (string * t) list
