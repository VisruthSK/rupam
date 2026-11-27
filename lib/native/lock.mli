type input =
  { path : string
  ; blake3 : string
  }
[@@deriving yojson_of]

type column =
  { name : string
  ; type_ : string
  }

type schema =
  { path : string
  ; columns : column list
  }

type entry =
  { source : string
  ; target : Target.t
  ; compiler : string
  ; strict : bool
  ; inputs : input list
  ; schemas : schema list
  }

val yojson_of_entry : entry -> Yojson.Safe.t
val establish : project:Project.t -> source:string -> TransformIR.item list -> entry
val require : project:Project.t -> source:string -> TransformIR.item list -> entry
val print_schemas : entry -> unit
