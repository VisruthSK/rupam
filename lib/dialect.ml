let names : Target.language -> Host.names = function
  | R -> R_host.names ()
  | Python -> Python_host.names ()
;;

let script (target : Target.t) items =
  match target.dialect with
  | Dplyr -> R_host.script items
  | Polars -> Python_host.script items
;;

let preamble (target : Target.t) =
  match target.dialect with
  | Dplyr -> Adapters.r ^ Adapters.dplyr
  | Polars -> Adapters.python ^ Adapters.polars
;;

let statements (target : Target.t) names items =
  match target.dialect with
  | Dplyr -> fst (R_syntax.program (R_host.statements names items))
  | Polars -> fst (Python_syntax.program (Python_host.statements names items))
;;

let r statement = fst (R_syntax.program [ statement ])
let python statement = fst (Python_syntax.program [ statement ])

let result (target : Target.t) names table ~id =
  match target.dialect with
  | Dplyr -> r (R_host.result names table ~id)
  | Polars -> python (Python_host.result names table ~id)
;;

let page (target : Target.t) ~id ~offset ~limit ~path =
  match target.language with
  | R -> r (R_host.page ~id ~offset ~limit ~path)
  | Python -> python (Python_host.page ~id ~offset ~limit ~path)
;;

let release (target : Target.t) ~id =
  match target.language with
  | R -> r (R_host.release ~id)
  | Python -> python (Python_host.release ~id)
;;

let table (target : Target.t) names table =
  match target.dialect with
  | Dplyr -> R_syntax.render (Dplyr.table names table)
  | Polars -> Python_syntax.render (Python_host.table names table)
;;
