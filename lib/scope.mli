type template =
  { params : string list
  ; body : Syntax.expr
  ; closure : env
  }

and binding =
  | Value of
      { id : TransformIR.id
      ; scheme : Types.scheme
      }
  | Table_value of
      { id : TransformIR.id
      ; table : TransformIR.table
      }
  | Function of
      { id : TransformIR.id option
      ; scheme : Types.scheme option
      ; template : template
      }
  | Constructor of
      { scheme : Types.scheme
      ; arity : int
      }
  | Inline of TransformIR.expr
  | Contract of Contract.t
  | Module of string

and env =
  { names : (string * binding) list
  ; inputs : inputs
  ; types : (string * Types.scheme) list
  ; variants : (string * (string * Types.scheme) list) list
  ; missing : TransformIR.policy option
  ; group_missing : TransformIR.policy option
  ; directory : string
  ; strict : bool
  ; supply : int ref
  }

and inputs =
  { columns : missing:string list -> string -> Input.column list
  ; digest : string -> string
  ; contract : string -> Contract.t
  }

val offline : inputs
val empty : ?inputs:inputs -> directory:string -> strict:bool -> unit -> env
val fresh_id : env -> string -> TransformIR.id
val fail : Diagnostic.span -> Diagnostic.code -> string -> 'a
val bind : env -> string -> binding -> env
val lookup : env -> string -> binding option
val bindings : env -> (string * binding * string) list
val table_parameters : template -> string list
val describe : binding -> string option
val origin : env -> TransformIR.table -> string
