type kind =
  | Function
  | File
  | Directory
  | Option
  | Plain

type suggestion =
  { text : string
  ; detail : string
  ; kind : kind
  }

val suggestion : ?kind:kind -> string -> string -> suggestion
val insert : suggestion -> string

val verb : string -> string option
val word_start : string -> int
val complete : Session.t -> string -> int * suggestion list
val tables : Session.t -> suggestion list
val bindings : Session.t -> (string * Scope.binding * suggestion) list
