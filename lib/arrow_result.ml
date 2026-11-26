type t

external open_ipc : string -> t = "rupam_arrow_open"
external rows : t -> int = "rupam_arrow_rows"
external names : t -> string array = "rupam_arrow_names"
external cells_ : t -> int -> int -> string option array array = "rupam_arrow_cells"

type histogram =
  { low : float
  ; high : float
  ; counts : int array
  ; missing : int
  }

external histogram_
  :  t
  -> int
  -> int
  -> float * float * int array * int
  = "rupam_arrow_histogram"

let histogram t ~column ~bins =
  let low, high, counts, missing = histogram_ t column bins in
  { low; high; counts; missing }
;;

external close : t -> unit = "rupam_arrow_close"

let cells t ~offset ~limit = cells_ t offset limit
