type buffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external state_size : unit -> int = "rupam_blake3_state_size"
external init : buffer -> unit = "rupam_blake3_init"
external update : buffer -> buffer -> int -> unit = "rupam_blake3_update"
external subtree : buffer -> int -> int -> buffer -> int -> unit = "rupam_blake3_subtree"
external push : buffer -> buffer -> int -> int -> unit = "rupam_blake3_push"
external finish : buffer -> string = "rupam_blake3_finish"

let chunk = 1024
let span = 1 lsl 22
let create n = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout n

let hex digest =
  String.concat
    ""
    (List.init (String.length digest) (fun i ->
       Printf.sprintf "%02x" (Char.code digest.[i])))
;;

let rec fill fd buffer offset length =
  if offset < length
  then (
    match Unix.read_bigarray fd buffer offset (length - offset) with
    | 0 -> failwith "the file shrank while it was being hashed"
    | n -> fill fd buffer (offset + n) length)
;;

let with_file path f =
  let fd = Unix.openfile path [ O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)
;;

let subtrees path pairs ~count ~workers =
  let work w () =
    with_file path (fun fd ->
      let buffer = create span in
      let rec go i =
        if i < count
        then (
          ignore (Unix.LargeFile.lseek fd (Int64.of_int (i * span)) SEEK_SET);
          fill fd buffer 0 span;
          subtree buffer span (i * (span / chunk)) pairs i;
          go (i + workers))
      in
      go w)
  in
  let others = List.init (workers - 1) (fun w -> Domain.spawn (work (w + 1))) in
  work 0 ();
  List.iter Domain.join others
;;

let file path =
  let size = (Unix.LargeFile.stat path).st_size |> Int64.to_int in
  let count = if size = 0 then 0 else (size - 1) / span in
  let workers = max 1 (min count (min 8 (Domain.recommended_domain_count ()))) in
  let pairs = create (max 1 (count * 64)) in
  if count > 0 then subtrees path pairs ~count ~workers;
  let state = create (state_size ()) in
  init state;
  for i = 0 to count - 1 do
    push state pairs i (span / chunk)
  done;
  with_file path (fun fd ->
    ignore (Unix.LargeFile.lseek fd (Int64.of_int (count * span)) SEEK_SET);
    let buffer = create span in
    let rec feed () =
      match Unix.read_bigarray fd buffer 0 span with
      | 0 -> ()
      | n ->
        update state buffer n;
        feed ()
    in
    feed ());
  hex (finish state)
;;

let string text =
  let state = create (state_size ()) in
  init state;
  let n = String.length text in
  let data = create (max 1 n) in
  String.iteri (fun i c -> data.{i} <- Char.code c) text;
  update state data n;
  hex (finish state)
;;
