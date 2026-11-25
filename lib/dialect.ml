let program (dialect : Project.dialect) =
  match dialect with
  | R_dplyr -> Dplyr.program
  | Python_polars -> Polars.program
;;

let value (dialect : Project.dialect) =
  match Runtime.of_dialect dialect with
  | R -> R_values.render
  | Python -> Polars.value
;;

let table (dialect : Project.dialect) =
  match dialect with
  | R_dplyr -> Dplyr.table
  | Python_polars -> Polars.table
;;

let preview (dialect : Project.dialect) =
  match dialect with
  | R_dplyr -> Dplyr.preview
  | Python_polars -> Polars.preview
;;
