type suggestion =
  { text : string
  ; detail : string
  }

val verb : string -> string option
val word_start : string -> int
val complete : Session.t -> string -> int * suggestion list
val tables : Session.t -> suggestion list
val bindings : Session.t -> (string * Scope.binding * suggestion) list
