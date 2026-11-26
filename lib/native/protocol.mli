type request =
  | Capabilities
  | Options
  | Version
  | Bindings
  | Check of { source : string }
  | Complete of { source : string }
  | Submit of
      { revision : int
      ; source : string
      }
  | Preview of
      { source : string
      ; offset : int
      ; limit : int
      }
  | Code of
      { source : string
      ; target : Target.t option
      }
  | Retarget of { target : Target.t }
  | Save of { path : string option }
  | Undo of { revision : int }
  | Redo of { revision : int }
  | Lock

type category =
  | Protocol
  | Diagnostic
  | Execution
  | Cancelled
  | Superseded

exception Rejected of category * string

val methods : (string * (Yojson.Safe.t -> request)) list
val decode : string -> Yojson.Safe.t -> request
val category_name : category -> string
val diagnostic : Diagnostic.t -> category * Yojson.Safe.t
val page : Page.t -> Yojson.Safe.t
val event : Session.event -> Yojson.Safe.t
val item : TransformIR.item -> Yojson.Safe.t option
