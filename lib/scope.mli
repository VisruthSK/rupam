type template =
  { params : string list
  ; body : Syntax.expr
  ; closure : env
  }

and binding =
  | Value of
      { id : Core.id
      ; scheme : Types.scheme
      }
  | Table_value of
      { id : Core.id
      ; table : Core.table
      }
  | Function of
      { id : Core.id option
      ; scheme : Types.scheme option
      ; template : template
      }
  | Constructor of
      { scheme : Types.scheme
      ; arity : int
      }
  | Inline of Core.expr
  | Contract of Contract.t
  | Module of string

and env =
  { names : (string * binding) list
  ; types : (string * Types.scheme) list
  ; variants : (string * (string * Types.scheme) list) list
  ; missing : Core.policy option
  ; group_missing : Core.policy option
  ; directory : string
  ; strict : bool
  }

val empty : directory:string -> strict:bool -> env
val fresh_id : string -> Core.id
val fail : Diagnostic.span -> Diagnostic.code -> string -> 'a
val bind : env -> string -> binding -> env
val lookup : env -> string -> binding option
val bindings : env -> (string * binding * string) list
val describe : binding -> string option
val origin : env -> Core.table -> string
