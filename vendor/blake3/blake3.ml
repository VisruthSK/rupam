type buffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external state_size : unit -> int = "rupam_blake3_state_size"
external init : buffer -> unit = "rupam_blake3_init"
external update : buffer -> buffer -> int -> unit = "rupam_blake3_update"
external finish : buffer -> string = "rupam_blake3_finish"

let chunk = 1 lsl 20
let create n = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout n

let file path =
  let state = create (state_size ()) in
  init state;
  let buffer = create chunk in
  let fd = Unix.openfile path [ O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
       let rec feed () =
         match Unix.read_bigarray fd buffer 0 chunk with
         | 0 -> ()
         | n ->
           update state buffer n;
           feed ()
       in
       feed ();
       let digest = finish state in
       String.concat
         ""
         (List.init (String.length digest) (fun i ->
            Printf.sprintf "%02x" (Char.code digest.[i]))))
;;
