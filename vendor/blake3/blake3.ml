type buffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external state_size : unit -> int = "rupam_blake3_state_size"
external init : buffer -> unit = "rupam_blake3_init"
external update : buffer -> buffer -> int -> unit = "rupam_blake3_update"
external subtree : buffer -> int -> int -> buffer -> int -> unit = "rupam_blake3_subtree"
external push : buffer -> buffer -> int -> int -> unit = "rupam_blake3_push"
external finish : buffer -> string = "rupam_blake3_finish"

external update_bytes
  :  buffer
  -> Bytes.t
  -> int
  -> int
  -> unit
  = "rupam_blake3_update_bytes"
[@@noalloc]

external read_at : string -> int -> buffer -> int -> bool -> unit = "rupam_blake3_read"
external misalignment : buffer -> int = "rupam_blake3_misalignment"

let chunk = 1024
let span = 1 lsl 24
let create n = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout n

let aligned n =
  let raw = create (n + 4096) in
  Bigarray.Array1.sub raw (misalignment raw) n
;;

let scratch = Domain.DLS.new_key (fun () -> aligned span)

let hex digest =
  String.concat
    ""
    (List.init (String.length digest) (fun i ->
       Printf.sprintf "%02x" (Char.code digest.[i])))
;;

let sequential count f =
  for i = 0 to count - 1 do
    f i
  done
;;

let file ?(parallel = sequential) path =
  let size = In_channel.with_open_bin path In_channel.length |> Int64.to_int in
  let count = if size = 0 then 0 else (size - 1) / span in
  let pairs = create (max 1 (count * 64)) in
  parallel count (fun i ->
    let buffer = Domain.DLS.get scratch in
    read_at path (i * span) buffer span true;
    subtree buffer span (i * (span / chunk)) pairs i);
  let state = create (state_size ()) in
  init state;
  for i = 0 to count - 1 do
    push state pairs i (span / chunk)
  done;
  let rest = size - (count * span) in
  let buffer = Domain.DLS.get scratch in
  if rest > 0
  then (
    read_at path (count * span) buffer rest false;
    update state buffer rest);
  hex (finish state)
;;

type hasher = buffer

let hasher () =
  let state = create (state_size ()) in
  init state;
  state
;;

let feed hasher bytes offset length =
  if offset < 0 || length < 0 || offset > Bytes.length bytes - length
  then invalid_arg "Blake3.feed";
  update_bytes hasher bytes offset length
;;

let digest hasher = hex (finish hasher)

let string text =
  let h = hasher () in
  feed h (Bytes.unsafe_of_string text) 0 (String.length text);
  digest h
;;
