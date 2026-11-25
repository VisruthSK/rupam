type session =
  { dialect : Project.dialect
  ; names : Host.names
  }

let session (dialect : Project.dialect) =
  { dialect
  ; names =
      (match dialect with
       | R_dplyr -> R_host.names ()
       | Python_polars -> Python_host.names ())
  }
;;

let script (dialect : Project.dialect) items =
  match dialect with
  | R_dplyr -> R_host.script items
  | Python_polars -> Python_host.script items
;;

let preamble (dialect : Project.dialect) =
  match dialect with
  | R_dplyr -> Adapters.r ^ Adapters.dplyr
  | Python_polars -> Adapters.python ^ Adapters.polars
;;

let statements session items =
  match session.dialect with
  | R_dplyr -> fst (R_syntax.program (R_host.statements session.names items))
  | Python_polars ->
    fst (Python_syntax.program (Python_host.statements session.names items))
;;

let preview session table ~offset ~limit ~path =
  match session.dialect with
  | R_dplyr ->
    fst (R_syntax.program [ R_host.preview session.names table ~offset ~limit ~path ])
  | Python_polars ->
    fst
      (Python_syntax.program
         [ Python_host.preview session.names table ~offset ~limit ~path ])
;;

let table session table =
  match session.dialect with
  | R_dplyr -> R_syntax.render (Dplyr.table session.names table)
  | Python_polars -> Python_syntax.render (Python_host.table session.names table)
;;
