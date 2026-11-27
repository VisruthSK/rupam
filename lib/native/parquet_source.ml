module Meta = Talon.Meta

type bigbytes = Meta.bigbytes

external get64 : bigbytes -> int -> int64 = "%caml_bigstring_get64"

let fail path message =
  Diagnostic.fail Unreadable_input (Printf.sprintf "%s: %s" path message)
;;

let read channel ~path ~position ~length =
  let b = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout length in
  In_channel.seek channel (Int64.of_int position);
  match In_channel.really_input_bigarray channel b 0 length with
  | Some () -> b
  | None -> fail path "the file is shorter than its metadata says"
;;

let magic (b : bigbytes) at text =
  let rec go i =
    i = String.length text || (b.{at + i} = Char.code text.[i] && go (i + 1))
  in
  go 0
;;

let footer channel ~path =
  let size = Int64.to_int (In_channel.length channel) in
  if size < 12 then fail path "not a Parquet file: it is too short";
  let head = read channel ~path ~position:0 ~length:4 in
  let tail = read channel ~path ~position:(size - 8) ~length:8 in
  if magic tail 4 "PARE" then fail path "the file is encrypted";
  if not (magic head 0 "PAR1" && magic tail 4 "PAR1") then fail path "not a Parquet file";
  let length =
    tail.{0} lor (tail.{1} lsl 8) lor (tail.{2} lsl 16) lor (tail.{3} lsl 24)
  in
  if length > size - 12 then fail path "the footer's length does not fit the file";
  let footer = size - 8 - length in
  try Meta.metadata (read channel ~path ~position:footer ~length) ~footer with
  | Meta.Error { msg; _ } -> fail path msg
;;

let rec leaves (schema : Meta.element array) i =
  if schema.(i).children = 0
  then 1, i + 1
  else (
    let rec go count next k =
      if k = schema.(i).children
      then count, next
      else (
        let n, next = leaves schema next in
        go (count + n) next (k + 1))
    in
    go 0 (i + 1) 0)
;;

let typ (e : Meta.element) : Input.physical =
  let unsupported name = Input.Unsupported name in
  if e.children > 0
  then unsupported "STRUCT"
  else (
    match e.physical, e.logical, e.converted with
    | Some Boolean, None, None -> Supported Bool
    | ( Some (Int32 | Int64)
      , (None | Some (Integer { signed = true; bits = 32 | 64 }))
      , (None | Some (17 | 18)) ) -> Supported Int
    | Some Int32, Some Date, _ | Some Int32, None, Some 6 -> Supported Date
    | Some Double, None, None -> Supported Float
    | Some Byte_array, Some (String | Enum), _ | Some Byte_array, None, Some (0 | 4) ->
      Supported String
    | Some Float, _, _ -> unsupported "FLOAT"
    | _, Some (Integer { bits; signed }), _ ->
      unsupported (Printf.sprintf "%sINT%d" (if signed then "" else "U") bits)
    | _, Some (Decimal { precision; scale }), _ ->
      unsupported (Printf.sprintf "DECIMAL(%d,%d)" precision scale)
    | Some Int96, _, _ | _, Some (Timestamp _), _ -> unsupported "TIMESTAMP"
    | _, Some (Time _), _ -> unsupported "TIME"
    | Some Byte_array, None, None -> unsupported "BLOB"
    | _ -> unsupported "an unsupported Parquet type")
;;

let decompress ~path (codec : Meta.codec) (source : bigbytes) size =
  let decoded decode =
    let target = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
    match decode source target with
    | Ok (p : Lz.progress) when p.produced = size -> target
    | Ok _ -> fail path "a page decompressed to the wrong size"
    | Error e ->
      fail path (Stdlib.Format.asprintf "a page does not decompress: %a" Lz.pp_error e)
  in
  match codec with
  | Uncompressed -> source
  | Snappy -> decoded Lz.Snappy.decode
  | Gzip -> decoded Lz.Gzip.decode
  | Zstd -> decoded Lz.Zstd.decode
  | Lz4_raw -> decoded Lz.Lz4.Block.decode
  | Lzo | Brotli | Lz4 | Unknown_codec _ ->
    fail path "a column uses an unsupported compression codec"
;;

let varint (b : bigbytes) p =
  let rec go p shift acc =
    let byte = b.{p} in
    let acc = acc lor ((byte land 0x7f) lsl shift) in
    if byte land 0x80 = 0 then acc, p + 1 else go (p + 1) (shift + 7) acc
  in
  go p 0 0
;;

let absent_levels (b : bigbytes) ~pos ~stop ~count =
  let rec go p left zeros =
    if left <= 0 || p >= stop
    then zeros
    else (
      let header, p = varint b p in
      if header land 1 = 0
      then (
        let run = min left (header lsr 1) in
        go (p + 1) (left - run) (if b.{p} = 0 then zeros + run else zeros))
      else (
        let groups = header lsr 1 in
        let n = min left (groups * 8) in
        let zeros = ref zeros in
        for i = 0 to n - 1 do
          if b.{p + (i lsr 3)} land (1 lsl (i land 7)) = 0 then incr zeros
        done;
        go (p + groups) (left - n) !zeros))
  in
  go pos count 0
;;

let plain_nan (b : bigbytes) ~pos ~count =
  let rec go i =
    i < count
    && (Float.is_nan (Int64.float_of_bits (get64 b (pos + (8 * i)))) || go (i + 1))
  in
  go 0
;;

let split_nan (b : bigbytes) ~pos ~count =
  let rec go i =
    i < count
    &&
    let bits = ref 0L in
    for k = 7 downto 0 do
      bits
      := Int64.logor (Int64.shift_left !bits 8) (Int64.of_int b.{pos + (k * count) + i})
    done;
    Float.is_nan (Int64.float_of_bits !bits) || go (i + 1)
  in
  go 0
;;

let values_nan ~path (encoding : Meta.encoding) b ~pos ~count =
  match encoding with
  | Plain -> plain_nan b ~pos ~count
  | Plain_dictionary | Rle_dictionary -> false
  | Byte_stream_split -> split_nan b ~pos ~count
  | Rle
  | Bit_packed
  | Delta_binary_packed
  | Delta_length_byte_array
  | Delta_byte_array
  | Unknown_encoding _ -> fail path "a Float column uses an unsupported encoding"
;;

let first_page (m : Meta.column_meta) =
  match m.dictionary_page with
  | Some d when d < m.data_page -> d
  | _ -> m.data_page
;;

let scan_chunk channel ~path (m : Meta.column_meta) ~optional ~nulls ~nan =
  let start = first_page m in
  let b = read channel ~path ~position:start ~length:m.compressed_size in
  let limit = m.compressed_size in
  let rec page pos seen =
    if pos >= limit || seen >= m.values
    then false
    else (
      let (h : Meta.header), body =
        try Meta.page_header b ~pos ~limit with
        | Meta.Error { msg; _ } -> fail path msg
      in
      if body + h.compressed_size > limit
      then fail path "a page runs past its column chunk";
      let raw = Bigarray.Array1.sub b body h.compressed_size in
      let next = body + h.compressed_size in
      match h.page with
      | Dictionary { values; encoding = _ } ->
        (nan
         && plain_nan
              (decompress ~path m.codec raw h.uncompressed_size)
              ~pos:0
              ~count:values)
        || page next seen
      | Data { values; encoding; _ } ->
        let data = decompress ~path m.codec raw h.uncompressed_size in
        let absent, start =
          if optional
          then (
            let length =
              data.{0} lor (data.{1} lsl 8) lor (data.{2} lsl 16) lor (data.{3} lsl 24)
            in
            absent_levels data ~pos:4 ~stop:(4 + length) ~count:values, 4 + length)
          else 0, 0
        in
        (nulls && absent > 0)
        || (nan && values_nan ~path encoding data ~pos:start ~count:(values - absent))
        || page next (seen + values)
      | Data_v2
          { values
          ; nulls = absent
          ; encoding
          ; definition_bytes
          ; repetition_bytes
          ; compressed
          ; _
          } ->
        let levels = definition_bytes + repetition_bytes in
        (nulls && absent > 0)
        || (nan
            &&
            let section = Bigarray.Array1.sub raw levels (h.compressed_size - levels) in
            let data =
              if compressed
              then decompress ~path m.codec section (h.uncompressed_size - levels)
              else section
            in
            values_nan ~path encoding data ~pos:0 ~count:(values - absent))
        || page next (seen + values)
      | Other -> page next seen)
  in
  page 0 0
;;

let nullable channel ~path (file : Meta.file) ~leaf (e : Meta.element) typ =
  let metas =
    Array.map
      (fun (g : Meta.row_group) ->
         match g.chunks.(leaf).meta with
         | Some m when not g.chunks.(leaf).encrypted -> m
         | _ -> fail path "a column chunk is stored outside the file or encrypted")
      file.row_groups
  in
  let optional = e.repetition <> Some Required in
  let floating = typ = Input.Supported Types.Float in
  let some_positive f =
    Array.exists
      (fun (m : Meta.column_meta) ->
         match f m.stats with
         | Some n -> n > 0
         | None -> false)
      metas
  in
  let all_known f =
    Array.for_all (fun (m : Meta.column_meta) -> f m.stats <> None) metas
  in
  let nulls (s : Meta.stats) = s.nulls
  and nans (s : Meta.stats) = s.nans in
  (optional && some_positive nulls)
  || (floating && some_positive nans)
  ||
  let need_nulls = optional && not (all_known nulls) in
  let need_nan = floating && not (all_known nans) in
  (need_nulls || need_nan)
  && Array.exists
       (fun m -> scan_chunk channel ~path m ~optional ~nulls:need_nulls ~nan:need_nan)
       metas
;;

let columns path : Input.column list =
  In_channel.with_open_bin path (fun channel ->
    let file = footer channel ~path in
    let schema = file.schema in
    if Array.length schema = 0 then fail path "the file has no schema";
    let rec go i k leaf acc =
      if k = schema.(0).children
      then List.rev acc
      else (
        let e = schema.(i) in
        let count, next = leaves schema i in
        let typ = typ e in
        let nullable =
          match typ with
          | Supported _ -> nullable channel ~path file ~leaf e typ
          | Unsupported _ -> false
        in
        go next (k + 1) (leaf + count) ({ Input.name = e.name; typ; nullable } :: acc))
    in
    go 1 0 0 [])
;;

type cell =
  | Missing
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Date of int

let bit_value (b : bigbytes) ~pos ~width i =
  let start = i * width in
  let rec go k acc =
    if k = width
    then acc
    else (
      let at = start + k in
      let bit = (b.{pos + (at lsr 3)} lsr (at land 7)) land 1 in
      go (k + 1) (Int64.logor acc (Int64.shift_left (Int64.of_int bit) k)))
  in
  go 0 0L
;;

let little (b : bigbytes) ~pos ~bytes =
  let rec go k acc =
    if k = bytes then acc else go (k + 1) (acc lor (b.{pos + k} lsl (8 * k)))
  in
  go 0 0
;;

let signed32 v = if v >= 0x8000_0000 then v - 0x1_0000_0000 else v

let hybrid (b : bigbytes) ~pos ~stop ~width ~count =
  let out = Array.make count 0 in
  let rec go p filled =
    if filled < count && p < stop
    then (
      let header, p = varint b p in
      if header land 1 = 0
      then (
        let run = min (count - filled) (header lsr 1) in
        let bytes = (width + 7) / 8 in
        Array.fill out filled run (little b ~pos:p ~bytes);
        go (p + bytes) (filled + run))
      else (
        let groups = header lsr 1 in
        let n = min (count - filled) (groups * 8) in
        for i = 0 to n - 1 do
          out.(filled + i) <- Int64.to_int (bit_value b ~pos:p ~width i)
        done;
        go (p + (groups * width)) (filled + n)))
  in
  go pos 0;
  out
;;

let zigzag u =
  Int64.logxor (Int64.shift_right_logical u 1) (Int64.neg (Int64.logand u 1L))
;;

let uleb (b : bigbytes) p =
  let rec go p shift acc =
    let byte = b.{p} in
    let acc = Int64.logor acc (Int64.shift_left (Int64.of_int (byte land 0x7f)) shift) in
    if byte land 0x80 = 0 then acc, p + 1 else go (p + 1) (shift + 7) acc
  in
  go p 0 0L
;;

let delta_binary (b : bigbytes) ~pos ~count =
  let block, p = uleb b pos in
  let miniblocks, p = uleb b p in
  let _, p = uleb b p in
  let first, p = uleb b p in
  let miniblocks = Int64.to_int miniblocks in
  let per = Int64.to_int block / miniblocks in
  let out = Array.make count 0L in
  if count > 0 then out.(0) <- zigzag first;
  let rec blocks p filled =
    if filled < count
    then (
      let min_delta, p = uleb b p in
      let min_delta = zigzag min_delta in
      let at = ref (p + miniblocks)
      and filled = ref filled in
      for k = 0 to miniblocks - 1 do
        let width = b.{p + k} in
        if !filled < count
        then (
          for i = 0 to per - 1 do
            if !filled < count
            then (
              out.(!filled)
              <- Int64.add
                   out.(!filled - 1)
                   (Int64.add min_delta (bit_value b ~pos:!at ~width i));
              incr filled)
          done;
          at := !at + (per * width / 8))
      done;
      blocks !at !filled)
  in
  blocks p 1;
  out
;;

let plain ~path (e : Meta.element) (typ : Types.t) (b : bigbytes) ~pos ~count =
  match e.physical, typ with
  | Some Boolean, _ ->
    Array.init count (fun i -> Bool (b.{pos + (i lsr 3)} land (1 lsl (i land 7)) <> 0))
  | Some Int32, Date ->
    Array.init count (fun i -> Date (signed32 (little b ~pos:(pos + (4 * i)) ~bytes:4)))
  | Some Int32, _ ->
    Array.init count (fun i ->
      Int (Int64.of_int (signed32 (little b ~pos:(pos + (4 * i)) ~bytes:4))))
  | Some Int64, _ -> Array.init count (fun i -> Int (get64 b (pos + (8 * i))))
  | Some Double, _ ->
    Array.init count (fun i -> Float (Int64.float_of_bits (get64 b (pos + (8 * i)))))
  | Some Byte_array, _ ->
    let p = ref pos in
    Array.init count (fun _ ->
      let n = little b ~pos:!p ~bytes:4 in
      let s = String.init n (fun k -> Char.chr b.{!p + 4 + k}) in
      p := !p + 4 + n;
      Text s)
  | _ -> fail path "a column has a physical type rupam does not read"
;;

let decode_values ~path e typ (encoding : Meta.encoding) dictionary b ~pos ~stop ~count =
  match encoding with
  | Plain -> plain ~path e typ b ~pos ~count
  | Plain_dictionary | Rle_dictionary ->
    let values =
      match dictionary with
      | Some d -> d
      | None -> fail path "a dictionary-encoded page has no dictionary"
    in
    Array.map
      (fun k ->
         if k < 0 || k >= Array.length values
         then fail path "a dictionary index is out of range"
         else values.(k))
      (hybrid b ~pos:(pos + 1) ~stop ~width:b.{pos} ~count)
  | Rle ->
    let length = little b ~pos ~bytes:4 in
    Array.map
      (fun v -> Bool (v <> 0))
      (hybrid b ~pos:(pos + 4) ~stop:(pos + 4 + length) ~width:1 ~count)
  | Delta_binary_packed ->
    Array.map
      (fun v ->
         match (typ : Types.t) with
         | Date -> Date (Int64.to_int v)
         | _ -> Int v)
      (delta_binary b ~pos ~count)
  | Byte_stream_split ->
    Array.init count (fun i ->
      let bits = ref 0L in
      for k = 7 downto 0 do
        bits
        := Int64.logor (Int64.shift_left !bits 8) (Int64.of_int b.{pos + (k * count) + i})
      done;
      Float (Int64.float_of_bits !bits))
  | Bit_packed | Delta_length_byte_array | Delta_byte_array | Unknown_encoding _ ->
    fail path "a column uses an encoding rupam does not read"
;;

let read_chunk
      channel
      ~path
      (m : Meta.column_meta)
      e
      typ
      ~optional
      (out : cell array)
      ~row
  =
  let b = read channel ~path ~position:(first_page m) ~length:m.compressed_size in
  let limit = m.compressed_size in
  let dictionary = ref None in
  let place levels values row =
    let next = ref 0 in
    Array.iteri
      (fun i level ->
         out.(row + i)
         <- (if level = 0
             then Missing
             else (
               let v = values.(!next) in
               incr next;
               v)))
      levels
  in
  let levels_of data ~pos ~stop ~count =
    if optional then hybrid data ~pos ~stop ~width:1 ~count else Array.make count 1
  in
  let present levels = Array.fold_left ( + ) 0 levels in
  let rec page pos row =
    if pos < limit
    then (
      let (h : Meta.header), body =
        try Meta.page_header b ~pos ~limit with
        | Meta.Error { msg; _ } -> fail path msg
      in
      let raw = Bigarray.Array1.sub b body h.compressed_size in
      let next = body + h.compressed_size in
      match h.page with
      | Dictionary { values; _ } ->
        let data = decompress ~path m.codec raw h.uncompressed_size in
        dictionary := Some (plain ~path e typ data ~pos:0 ~count:values);
        page next row
      | Data { values; encoding; _ } ->
        let data = decompress ~path m.codec raw h.uncompressed_size in
        let length = if optional then little data ~pos:0 ~bytes:4 else 0 in
        let levels = levels_of data ~pos:4 ~stop:(4 + length) ~count:values in
        let start = if optional then 4 + length else 0 in
        place
          levels
          (decode_values
             ~path
             e
             typ
             encoding
             !dictionary
             data
             ~pos:start
             ~stop:h.uncompressed_size
             ~count:(present levels))
          row;
        page next (row + values)
      | Data_v2 { values; encoding; definition_bytes; repetition_bytes; compressed; _ } ->
        let levels =
          levels_of
            raw
            ~pos:repetition_bytes
            ~stop:(repetition_bytes + definition_bytes)
            ~count:values
        in
        let skip = definition_bytes + repetition_bytes in
        let section = Bigarray.Array1.sub raw skip (h.compressed_size - skip) in
        let size = h.uncompressed_size - skip in
        let data =
          if compressed then decompress ~path m.codec section size else section
        in
        place
          levels
          (decode_values
             ~path
             e
             typ
             encoding
             !dictionary
             data
             ~pos:0
             ~stop:size
             ~count:(present levels))
          row;
        page next (row + values)
      | Other -> page next row)
  in
  page 0 row
;;

let rows path =
  In_channel.with_open_bin path (fun channel ->
    let file = footer channel ~path in
    let schema = file.schema in
    let rec go i k leaf acc =
      if k = schema.(0).children
      then List.rev acc
      else (
        let e = schema.(i) in
        let count, next = leaves schema i in
        match typ e with
        | Unsupported _ -> go next (k + 1) (leaf + count) acc
        | Supported t ->
          let out = Array.make file.rows Missing in
          let row = ref 0 in
          Array.iter
            (fun (g : Meta.row_group) ->
               match g.chunks.(leaf).meta with
               | Some m ->
                 read_chunk
                   channel
                   ~path
                   m
                   e
                   t
                   ~optional:(e.repetition <> Some Required)
                   out
                   ~row:!row;
                 row := !row + g.rows
               | None -> fail path "a column chunk is stored outside the file")
            file.row_groups;
          go next (k + 1) (leaf + count) ((e.name, out) :: acc))
    in
    go 1 0 0 [])
;;
