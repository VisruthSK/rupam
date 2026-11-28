type t

val run : ?workers:int -> size:int64 -> (t -> 'a) -> 'a
val each : t -> int -> (int -> unit) -> unit
val ordered : t -> next:(unit -> 'a option) -> work:('a -> 'b) -> ('b -> unit) -> unit
