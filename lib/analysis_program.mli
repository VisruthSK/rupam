type join_kind =
  | Inner
  | Left
  | Right
  | Full
  | Semi
  | Anti

val join_name : join_kind -> string

type relationship =
  | One_to_one
  | Many_to_one
  | One_to_many
  | Many_to_many

type dataset =
  { origin : origin
  ; fields : string list
  ; plan : Table.t
  }

and origin =
  | Contracted of contracted
  | File of source
  | Joined of join

and contracted =
  { dictionary : Contract.t
  ; table : Contract.table
  ; source : source
  }

and source =
  { path : string
  ; digest : string
  ; format : Data_file.format
  ; columns : (string * Shape.primitive * bool) list
  }

and join =
  { left : dataset
  ; right : dataset
  ; key : string
  ; kind : join_kind
  ; relationship : relationship option
  ; covered : bool
  }

type binding =
  | Dataset of dataset
  | Dictionary of Contract.t
  | Function of string list * Syntax.expr * environment * Check.env

and environment = (string * binding) list

val uses : (string -> bool) -> Syntax.expr -> bool
val tabular : Syntax.declaration list -> bool
val kind : Shape.primitive -> string

val dataset
  :  directory:string
  -> (string * binding) list
  -> Check.env
  -> Syntax.expr
  -> dataset

type output =
  | Write of string
  | Print

val printed : Syntax.declaration -> Syntax.expr option

val extend
  :  ?strict:bool
  -> directory:string
  -> environment
  -> Check.env
  -> Syntax.declaration list
  -> environment * Check.env * (dataset * output) list

val analyze
  :  ?strict:bool
  -> directory:string
  -> Syntax.declaration list
  -> environment * (dataset * output) list

val compile
  :  ?strict:bool
  -> directory:string
  -> Syntax.declaration list
  -> (dataset * output) list

val public_view : dataset -> dataset
val origin : dataset -> string
val schema : dataset -> unit
val page : dataset -> path:string -> total:string -> offset:int -> Page.t
val explain : dataset -> unit
val sources : dataset -> string list
val files : dataset -> source list
val contracts : dataset -> string list
val validate : dataset -> unit
