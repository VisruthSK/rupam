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

let preview (target : Target.t) names table ~offset ~limit ~path =
  match target.dialect with
  | Dplyr -> fst (R_syntax.program [ R_host.preview names table ~offset ~limit ~path ])
  | Polars ->
    fst (Python_syntax.program [ Python_host.preview names table ~offset ~limit ~path ])
;;

let table (target : Target.t) names table =
  match target.dialect with
  | Dplyr -> R_syntax.render (Dplyr.table names table)
  | Polars -> Python_syntax.render (Python_host.table names table)
;;
