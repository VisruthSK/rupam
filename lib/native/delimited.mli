val columns
  :  on_read:(Bytes.t -> int -> int -> unit)
  -> missing:string list
  -> path:string
  -> Input.format
  -> Input.column list
