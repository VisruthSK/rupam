open Talon_ipc

type bigbytes = Ipc.bigbytes

external get16 : bigbytes -> int -> int = "%caml_bigstring_get16"
external get32 : bigbytes -> int -> int32 = "%caml_bigstring_get32"
external get64 : bigbytes -> int -> int64 = "%caml_bigstring_get64"

type storage =
  | Boolean
  | Integer of
      { width : int
      ; signed : bool
      }
  | Real of int
  | Text of int
  | Dictionary of
      { width : int
      ; signed : bool
      ; id : int
      }
  | Opaque

type buffer =
  { position : int
  ; length : int
  }

type column =
  { nulls : int
  ; buffers : buffer array
  }

type batch =
  { start : int
  ; rows : int
  ; columns : column array
  }

type t =
  { mutable channel : In_channel.t option
  ; names : string array
  ; storages : storage array
  ; batches : batch array
  ; total : int
  ; dictionaries : (int, string option array Lazy.t) Hashtbl.t
  }

type value =
  | Missing
  | Unsupported
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Date of int

let live t =
  match t.channel with
  | Some channel -> channel
  | None -> failwith "The result was released"
;;

let bigbytes channel ~position ~length =
  let b = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout length in
  In_channel.seek channel (Int64.of_int position);
  match In_channel.really_input_bigarray channel b 0 length with
  | Some () -> b
  | None -> failwith "The result file is truncated"
;;

let bytes channel ~position ~length =
  let b = Bytes.create length in
  In_channel.seek channel (Int64.of_int position);
  match In_channel.really_input channel b 0 length with
  | Some () -> b
  | None -> failwith "The result file is truncated"
;;

let storage (field : Types.field) =
  match field.encoding, field.arrow with
  | Some e, (Utf8 | Large_utf8) ->
    Dictionary { width = e.index_bits / 8; signed = e.index_signed; id = e.id }
  | Some _, _ -> Opaque
  | None, Bool -> Boolean
  | None, Int { bits = 64; signed = false } -> Opaque
  | None, Int { bits; signed } -> Integer { width = bits / 8; signed }
  | None, Float 32 -> Real 4
  | None, Float 64 -> Real 8
  | None, (Date_day | Time (S | Ms)) -> Integer { width = 4; signed = true }
  | None, (Date_ms | Time (Us | Ns) | Timestamp _ | Duration _) ->
    Integer { width = 8; signed = true }
  | None, Utf8 -> Text 4
  | None, Large_utf8 -> Text 8
  | None, _ -> Opaque
;;

let rec span (field : Types.field) =
  let own =
    match field.encoding, field.arrow with
    | Some _, _ -> 2
    | ( None
      , ( Bool
        | Int _
        | Float _
        | Date_day
        | Date_ms
        | Time _
        | Timestamp _
        | Duration _
        | Decimal _
        | Fixed_binary _ ) ) -> 2
    | None, (Utf8 | Large_utf8 | Binary | Large_binary) -> 3
    | None, (List | Large_list | Map) -> 2
    | None, (Fixed_list _ | Struct) -> 1
    | None, (Utf8_view | Binary_view | Unsupported _) ->
      failwith
        ("Unsupported result schema: column " ^ field.name ^ " has an unsupported type")
  in
  List.fold_left
    (fun (nodes, buffers) child ->
       let n, b = span child in
       nodes + n, buffers + b)
    (1, own)
    field.children
;;

let metadata channel (block : Types.block) =
  if block.metadata < 8 then failwith "The result file has an unreadable batch";
  let b = bigbytes channel ~position:block.offset ~length:block.metadata in
  let size = Ipc.word b 4 land 0xFFFF_FFFF in
  if Ipc.word b 0 <> -1 || 8 + size > block.metadata
  then failwith "The result file has an unreadable batch";
  Ipc.parse b ~first:8 ~last:(8 + size)
;;

let columns (spans : (int * int) array) (block : Types.block) (batch : Types.batch) =
  let body = block.offset + block.metadata in
  let buffer (offset, length) =
    if offset < 0 || length < 0 || offset + length > block.body
    then failwith "The result file has an unreadable batch";
    { position = body + offset; length }
  in
  let node = ref 0
  and next = ref 0 in
  Array.map
    (fun (nodes, buffers) ->
       if
         !node + nodes > Array.length batch.nodes
         || !next + buffers > Array.length batch.buffers
       then failwith "The result file has an unreadable batch";
       let length, nulls = batch.nodes.(!node) in
       if length <> batch.length then failwith "The result file has an unreadable batch";
       let column =
         { nulls
         ; buffers = Array.init buffers (fun i -> buffer batch.buffers.(!next + i))
         }
       in
       node := !node + nodes;
       next := !next + buffers;
       column)
    spans
;;

let validity channel column ~lo ~hi =
  if column.nulls = 0 || column.buffers.(0).length = 0 || hi <= lo
  then fun _ -> true
  else (
    let first = lo / 8
    and last = (hi - 1) / 8 in
    let buffer = column.buffers.(0) in
    if last >= buffer.length then failwith "The result file has an unreadable batch";
    let b =
      bigbytes channel ~position:(buffer.position + first) ~length:(last - first + 1)
    in
    fun i ->
      let k = lo + i - (8 * first) in
      b.{k lsr 3} land (1 lsl (k land 7)) <> 0)
;;

let fixed channel buffer ~width ~lo ~hi =
  if hi * width > buffer.length then failwith "The result file has an unreadable batch";
  bigbytes channel ~position:(buffer.position + (lo * width)) ~length:((hi - lo) * width)
;;

let integer b ~width ~signed i =
  match width with
  | 1 ->
    let v = b.{i} in
    Int64.of_int (if signed && v > 127 then v - 256 else v)
  | 2 ->
    let v = get16 b (2 * i) in
    Int64.of_int (if signed && v > 32767 then v - 65536 else v)
  | 4 ->
    let v = Int64.of_int32 (get32 b (4 * i)) in
    if signed then v else Int64.logand v 0xFFFF_FFFFL
  | _ -> get64 b (8 * i)
;;

let offset b ~width i =
  if width = 4 then Int32.to_int (get32 b (4 * i)) else Int64.to_int (get64 b (8 * i))
;;

let texts channel column ~width ~lo ~hi =
  let offsets = fixed channel column.buffers.(1) ~width ~lo ~hi:(hi + 1) in
  let first = offset offsets ~width 0
  and last = offset offsets ~width (hi - lo) in
  if first < 0 || last < first || last > column.buffers.(2).length
  then failwith "The result file has an unreadable batch";
  let data =
    bytes channel ~position:(column.buffers.(2).position + first) ~length:(last - first)
  in
  fun i ->
    let start = offset offsets ~width i
    and stop = offset offsets ~width (i + 1) in
    if start < first || stop < start || stop > last
    then failwith "The result file has an unreadable batch";
    Bytes.sub_string data (start - first) (stop - start)
;;

let dictionary t id =
  match Hashtbl.find_opt t.dictionaries id with
  | Some values -> Lazy.force values
  | None -> failwith "The result file has an unreadable batch"
;;

let decoder t batch j ~lo ~hi =
  let channel = live t in
  let column = batch.columns.(j) in
  let present = validity channel column ~lo ~hi in
  let read =
    match t.storages.(j) with
    | Boolean ->
      let bits =
        validity channel { nulls = 1; buffers = [| column.buffers.(1) |] } ~lo ~hi
      in
      fun i -> Bool (bits i)
    | Integer { width; signed } ->
      let b = fixed channel column.buffers.(1) ~width ~lo ~hi in
      fun i -> Int (integer b ~width ~signed i)
    | Real 4 ->
      let b = fixed channel column.buffers.(1) ~width:4 ~lo ~hi in
      fun i -> Float (Int32.float_of_bits (get32 b (4 * i)))
    | Real _ ->
      let b = fixed channel column.buffers.(1) ~width:8 ~lo ~hi in
      fun i -> Float (Int64.float_of_bits (get64 b (8 * i)))
    | Text width ->
      let text = texts channel column ~width ~lo ~hi in
      fun i -> Text (text i)
    | Dictionary { width; signed; id } ->
      let values = dictionary t id in
      let b = fixed channel column.buffers.(1) ~width ~lo ~hi in
      fun i ->
        let k = Int64.to_int (integer b ~width ~signed i) in
        if k < 0 || k >= Array.length values
        then failwith "The result file has an unreadable batch";
        (match values.(k) with
         | Some s -> Text s
         | None -> Missing)
    | Opaque -> fun _ -> Unsupported
  in
  fun i -> if present i then read i else Missing
;;

let decode_dictionary channel (field : Types.field) blocks id =
  let width =
    match field.arrow with
    | Large_utf8 -> 8
    | _ -> 4
  in
  Array.fold_left
    (fun values (block : Types.block) ->
       match (metadata channel block).header with
       | Some (Dictionary d) when d.id = id ->
         let column =
           (columns [| span { field with encoding = None } |] block d.batch).(0)
         in
         let rows = d.batch.length in
         let present = validity channel column ~lo:0 ~hi:rows in
         let text = texts channel column ~width ~lo:0 ~hi:rows in
         let batch =
           Array.init rows (fun i -> if present i then Some (text i) else None)
         in
         if d.delta then Array.append values batch else batch
       | _ -> values)
    [||]
    blocks
;;

let footer channel =
  let size = In_channel.length channel |> Int64.to_int in
  if size < 18 then failwith "The result file has no valid Arrow footer";
  let tail = bigbytes channel ~position:(size - 10) ~length:10 in
  let magic = String.init 6 (fun i -> Char.chr tail.{4 + i}) in
  let length = Ipc.word tail 0 in
  if magic <> "ARROW1" || length <= 0 || length > size - 18
  then failwith "The result file has no valid Arrow footer";
  let b = bigbytes channel ~position:(size - 10 - length) ~length in
  Ipc.footer b ~first:0 ~last:length
;;

let load channel =
  let footer =
    try footer channel with
    | Ipc.Malformed _ -> failwith "The result file has no valid Arrow footer"
  in
  if not footer.schema.little then failwith "Unsupported result schema: big-endian data";
  let fields = Array.of_list footer.schema.fields in
  let spans = Array.map span fields in
  let start = ref 0 in
  let batches =
    Array.map
      (fun block ->
         match (metadata channel block).header with
         | Some (Batch b) ->
           if b.compression <> None
           then failwith "Unsupported result schema: compressed batches";
           let batch =
             { start = !start; rows = b.length; columns = columns spans block b }
           in
           start := !start + b.length;
           batch
         | _ -> failwith "The result file has an unreadable batch"
         | exception Ipc.Malformed _ -> failwith "The result file has an unreadable batch")
      footer.batches
  in
  let dictionaries = Hashtbl.create 4 in
  Array.iter
    (fun (field : Types.field) ->
       Option.iter
         (fun (e : Types.encoding) ->
            Hashtbl.replace
              dictionaries
              e.id
              (lazy (decode_dictionary channel field footer.dictionaries e.id)))
         field.encoding)
    fields;
  { channel = Some channel
  ; names = Array.map (fun (f : Types.field) -> f.name) fields
  ; storages = Array.map storage fields
  ; batches
  ; total = !start
  ; dictionaries
  }
;;

let close t =
  Option.iter In_channel.close t.channel;
  t.channel <- None
;;

let open_ipc path =
  let channel =
    try In_channel.open_bin path with
    | Sys_error _ -> failwith "Cannot map the result file"
  in
  match load channel with
  | t ->
    Gc.finalise close t;
    t
  | exception e ->
    In_channel.close channel;
    raise e
;;

let rows t =
  ignore (live t);
  t.total
;;

let names t =
  ignore (live t);
  Array.copy t.names
;;

let locate t row =
  let rec search low high =
    if low >= high
    then low
    else (
      let mid = low + ((high - low) / 2) in
      let b = t.batches.(mid) in
      if b.start + b.rows <= row then search (mid + 1) high else search low mid)
  in
  search 0 (Array.length t.batches)
;;

let value t row column =
  if row < 0 || row >= rows t || column < 0 || column >= Array.length t.names
  then invalid_arg "cell index";
  let batch = t.batches.(locate t row) in
  let local = row - batch.start in
  decoder t batch column ~lo:local ~hi:(local + 1) 0
;;

let text = function
  | Missing -> None
  | Unsupported -> Some "?"
  | Bool b -> Some (if b then "true" else "false")
  | Int n -> Some (Int64.to_string n)
  | Float x -> Some (Printf.sprintf "%.17g" x)
  | Text s -> Some s
  | Date d -> Some (string_of_int d)
;;

let cells t ~columns ~offset ~limit =
  let total = rows t in
  let offset = max 0 (min offset total) in
  let limit = max 0 (min limit (total - offset)) in
  Array.iter
    (fun j -> if j < 0 || j >= Array.length t.names then invalid_arg "column index")
    columns;
  let out = Array.map (fun _ -> Array.make limit None) columns in
  let rec fill b =
    if b < Array.length t.batches && t.batches.(b).start < offset + limit
    then (
      let batch = t.batches.(b) in
      let lo = max offset batch.start - batch.start
      and hi = min (offset + limit) (batch.start + batch.rows) - batch.start in
      if lo < hi
      then
        Array.iteri
          (fun k j ->
             let at = decoder t batch j ~lo ~hi in
             for i = 0 to hi - lo - 1 do
               out.(k).(batch.start + lo + i - offset) <- text (at i)
             done)
          columns;
      fill (b + 1))
  in
  if limit > 0 then fill (locate t offset);
  out
;;

let numeric t column =
  match t.storages.(column) with
  | Integer _ | Real _ -> true
  | Boolean | Text _ | Dictionary _ | Opaque -> false
;;

let scan t column f =
  Array.iter
    (fun batch ->
       let at = decoder t batch column ~lo:0 ~hi:batch.rows in
       for i = 0 to batch.rows - 1 do
         match at i with
         | Missing -> f None
         | Int n -> f (Some (Int64.to_float n))
         | Float x -> f (Some x)
         | Bool _ | Text _ | Date _ | Unsupported -> ()
       done)
    t.batches
;;

let numbers t column size =
  if not (numeric t column) then failwith "Expected a numeric column";
  let values = Array.make size 0. in
  let count = ref 0 in
  scan t column (function
    | Some x when !count < size ->
      values.(!count) <- x;
      incr count
    | _ -> ());
  values
;;

let distinct_floats values =
  let n = Array.length values in
  let capacity =
    let rec grow c = if c < 2 * n then grow (2 * c) else c in
    grow 16
  in
  let slots = Array.make capacity 0L
  and used = Bytes.make capacity '\000' in
  let distinct = ref 0 in
  Array.iter
    (fun x ->
       let bits = Int64.bits_of_float (if x = 0. then 0. else x) in
       let h = Int64.mul bits 0x9E3779B97F4A7C15L in
       let rec probe s =
         if Bytes.get used s = '\000'
         then (
           Bytes.set used s '\001';
           slots.(s) <- bits;
           incr distinct)
         else if not (Int64.equal slots.(s) bits)
         then probe ((s + 1) land (capacity - 1))
       in
       probe
         (Int64.to_int (Int64.logxor h (Int64.shift_right_logical h 32))
          land (capacity - 1)))
    values;
  !distinct
;;

type histogram =
  { low : float
  ; high : float
  ; counts : int array
  ; missing : int
  }

let histogram t ~column ~bins =
  if not (numeric t column) then failwith "!hist needs a numeric column";
  let low = ref 0.
  and high = ref 0.
  and present = ref 0
  and missing = ref 0 in
  scan t column (function
    | None -> incr missing
    | Some x ->
      if !present = 0 || x < !low then low := x;
      if !present = 0 || x > !high then high := x;
      incr present);
  let counts = Array.make bins 0 in
  let span = (!high /. 2.) -. (!low /. 2.) in
  scan t column (function
    | None -> ()
    | Some x ->
      let position =
        if span > 0. then ((x /. 2.) -. (!low /. 2.)) /. span *. float_of_int bins else 0.
      in
      let k =
        if position >= float_of_int bins
        then bins - 1
        else if position > 0.
        then int_of_float position
        else 0
      in
      counts.(k) <- counts.(k) + 1);
  { low = !low; high = !high; counts; missing = !missing }
;;

type summary =
  { histogram : histogram
  ; q1 : float option
  ; median : float option
  ; q3 : float option
  ; mean : float option
  ; sd : float option
  ; skewness : float option
  ; distinct : int
  }

let swap (a : float array) i j =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x
;;

let rec select random (a : float array) lo hi k =
  if lo < hi
  then (
    let pivot = a.(lo + Random.State.int random (hi - lo + 1)) in
    let i = ref lo
    and j = ref hi in
    while !i <= !j do
      while a.(!i) < pivot do
        incr i
      done;
      while a.(!j) > pivot do
        decr j
      done;
      if !i <= !j
      then (
        swap a !i !j;
        incr i;
        decr j)
    done;
    if k <= !j then select random a lo !j k else if k >= !i then select random a !i hi k)
;;

let quantile (values : float array) p =
  let n = Array.length values in
  if n = 0
  then None
  else (
    let k = float_of_int (n - 1) *. p in
    let i = int_of_float k in
    let weight = k -. float_of_int i in
    select (Random.State.make [| n |]) values 0 (n - 1) i;
    let next = ref values.(min (i + 1) (n - 1)) in
    for j = i + 2 to n - 1 do
      if values.(j) < !next then next := values.(j)
    done;
    Some (((1. -. weight) *. values.(i)) +. (weight *. !next)))
;;

let summary t ~column =
  let histogram = histogram t ~column ~bins:20 in
  let n = rows t - histogram.missing in
  let values = numbers t column n in
  let distinct = ref (distinct_floats values) in
  let quantile = quantile values in
  let scaled_sum f = Array.fold_left (fun sum value -> sum +. f value) 0. values in
  let mean, sd, skewness =
    if n = 0
    then None, None, None
    else (
      let midpoint = (histogram.high /. 2.) +. (histogram.low /. 2.) in
      let scale = max (histogram.high -. midpoint) (midpoint -. histogram.low) in
      let observations = float_of_int n in
      if scale = 0.
      then Some midpoint, (if n > 1 then Some 0. else None), None
      else (
        let scaled x = (x -. midpoint) /. scale in
        let center = scaled_sum scaled /. observations in
        let mean = midpoint +. (scale *. center) in
        let deviation x = scaled x -. center in
        let second =
          scaled_sum (fun x ->
            let d = deviation x in
            d *. d)
        in
        let sd = scale *. sqrt (second /. (observations -. 1.)) in
        let skewness =
          if n < 3 || second = 0.
          then None
          else (
            let third =
              scaled_sum (fun x ->
                let d = deviation x in
                d *. d *. d)
            in
            let variance = second /. observations in
            let correction =
              sqrt (observations *. (observations -. 1.)) /. (observations -. 2.)
            in
            Some (correction *. (third /. observations) /. (variance *. sqrt variance)))
        in
        Some mean, (if n > 1 && Float.is_finite sd then Some sd else None), skewness))
  in
  { histogram
  ; q1 = quantile 0.25
  ; median = quantile 0.5
  ; q3 = quantile 0.75
  ; mean
  ; sd
  ; skewness
  ; distinct = !distinct
  }
;;

type frequencies =
  { missing : int
  ; distinct : int
  ; top : (string * int) list
  }

type profile =
  | Numeric of summary
  | Frequencies of frequencies

let frequencies t ~column =
  let counts = Hashtbl.create 32 in
  let missing = ref 0 in
  let total = rows t in
  let rec scan offset =
    if offset < total
    then (
      let limit = min (total - offset) 4096 in
      Array.iter
        (function
          | None -> incr missing
          | Some value ->
            let n = Option.value (Hashtbl.find_opt counts value) ~default:0 in
            Hashtbl.replace counts value (n + 1))
        (cells t ~columns:[| column |] ~offset ~limit).(0);
      scan (offset + limit))
  in
  scan 0;
  let compare (left, n) (right, m) =
    match Int.compare m n with
    | 0 -> String.compare left right
    | order -> order
  in
  let top =
    Hashtbl.fold
      (fun value count top ->
         List.filteri (fun i _ -> i < 3) (List.merge compare [ value, count ] top))
      counts
      []
  in
  { missing = !missing; distinct = Hashtbl.length counts; top }
;;
