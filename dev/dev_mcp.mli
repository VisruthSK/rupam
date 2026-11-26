type parameter =
  | Text of string
  | Choice of string * string list

type tool =
  { name : string
  ; description : string
  ; parameters : parameter list
  ; call : (string * string) list -> Yojson.Safe.t
  }

val run : tool list -> in_channel -> out_channel -> unit
