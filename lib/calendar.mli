val of_iso : string -> int option
val to_iso : int -> string
val ymd : int -> int * int * int
val of_ymd : int * int * int -> int option
val add_months : int -> int -> int
val weekday : int -> int
val year_day : int -> int
val parse : format:string -> string -> int option
