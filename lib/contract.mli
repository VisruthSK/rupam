type column =
  { name : string
  ; logical_type : string
  ; required : bool
  ; primary : bool
  ; unique : bool
  ; restricted : bool
  ; metadata : Yojson.Basic.t
  }

type table =
  { name : string
  ; parquet : string option
  ; columns : column list
  ; metadata : Yojson.Basic.t
  }

type t =
  { path : string
  ; tables : table list
  ; metadata : Yojson.Basic.t
  }

val decode : string -> Yojson.Basic.t -> t
val describe_table : table -> unit
val describe : t -> unit
