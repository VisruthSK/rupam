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
