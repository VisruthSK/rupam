type view =
  | Schema
  | Grid
  | Preview
  | Explain
  | Contract
  | Validate

type command =
  | Quit
  | Help of string option
  | Options
  | Version
  | Lock
  | Save of string option
  | Undo
  | Redo
  | Bindings
  | Target of Rupam.Target.t option
  | Language of Rupam.Target.language
  | Dialect of Rupam.Target.dialect
  | Type of string
  | View of view * string
  | Code of Rupam.Target.t option * Rupam_native.Session.code * string
  | Source of string

type t =
  { session : Rupam_native.Session.t
  ; directory : string
  ; show : digits:int -> Rupam_native.Page.t -> unit
  ; fail : Rupam.Diagnostic.t -> unit
  ; view : Rupam.Syntax.expr -> Rupam_native.Session.result -> unit
  }

type suggestion = Rupam_native.Completion.suggestion =
  { text : string
  ; detail : string
  ; kind : Rupam_native.Completion.kind
  }

val parse : string -> (command, string) result
val report : string -> unit
val report_diagnostic : Rupam.Diagnostic.t -> unit
val diagnostic : ?fail:(Rupam.Diagnostic.t -> unit) -> (unit -> 'a) -> ('a, unit) result
val digits : t -> int
val complete : t -> string -> int * suggestion list
val hint : string -> string option
val missing_argument : string -> bool
val incomplete : string -> bool
val run : t -> command -> bool
val open_session : project:Rupam_native.Project.t -> ?path:string -> unit -> t option
val start : project:Rupam_native.Project.t -> ?path:string -> unit -> unit
