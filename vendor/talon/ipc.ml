(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)
open Types

type bigbytes = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Malformed of string

let fail fmt = Printf.ksprintf (fun m -> raise (Malformed m)) fmt

external get16 : bigbytes -> int -> int = "%caml_bigstring_get16"
external get32 : bigbytes -> int -> int32 = "%caml_bigstring_get32"
external get64 : bigbytes -> int -> int64 = "%caml_bigstring_get64"

let word b p = Int32.to_int (get32 b p)

let long b p =
  let v = get64 b p in
  if Int64.of_int (Int64.to_int v) <> v then fail "a 64-bit value overflows";
  Int64.to_int v
;;

type t =
  { b : bigbytes
  ; first : int
  ; last : int
  ; pos : int
  }

let check t p n =
  if p < t.first || p + n > t.last then fail "a flatbuffer offset is outside it"
;;

let long_at t p =
  check t p 8;
  long t.b p
;;

let u32 t p =
  check t p 4;
  word t.b p land 0xFFFF_FFFF
;;

let target t p =
  let v = u32 t p in
  if v = 0 then fail "a flatbuffer offset is zero";
  p + v
;;

let root b ~first ~last =
  let t = { b; first; last; pos = first } in
  { t with pos = target t first }
;;

let slot t id =
  check t t.pos 4;
  let vtable = t.pos - word t.b t.pos in
  let entry = 4 + (2 * id) in
  check t vtable 2;
  if entry + 2 > get16 t.b vtable
  then -1
  else (
    let off =
      check t (vtable + entry) 2;
      get16 t.b (vtable + entry)
    in
    if off = 0 then -1 else t.pos + off)
;;

let scalar get t id default =
  let p = slot t id in
  if p < 0 then default else get t p
;;

let int8 =
  scalar (fun t p ->
    check t p 1;
    t.b.{p})
;;

let int16 =
  scalar (fun t p ->
    check t p 2;
    get16 t.b p)
;;

let int32 =
  scalar (fun t p ->
    check t p 4;
    word t.b p)
;;

let int64 = scalar long_at
let bool t id d = int8 t id (if d then 1 else 0) <> 0

let field t id =
  let p = slot t id in
  if p < 0 then None else Some (target t p)
;;

let table t id = Option.map (fun pos -> { t with pos }) (field t id)

let vector t p width =
  let n = u32 t p in
  check t (p + 4) (n * width);
  p + 4, n
;;

let string t id =
  Option.map
    (fun p ->
       let first, n = vector t p 1 in
       String.init n (fun i -> Char.chr t.b.{first + i}))
    (field t id)
;;

let tables t id =
  match field t id with
  | None -> []
  | Some p ->
    let first, n = vector t p 4 in
    List.init n (fun i -> { t with pos = target t (first + (4 * i)) })
;;

let structs t id ~size =
  match field t id with
  | None -> 0, 0
  | Some p -> vector t p size
;;

let nullary =
  [ 1, Unsupported "null", "null"
  ; 11, Unsupported "interval", "interval"
  ; 14, Unsupported "union", "union"
  ; 22, Unsupported "run-end encoded", "run-end encoded"
  ; 25, Unsupported "list view", "list view"
  ; 26, Unsupported "large list view", "large list view"
  ; 4, Binary, "binary"
  ; 5, Utf8, "utf8"
  ; 6, Bool, "bool"
  ; 12, List, "list"
  ; 13, Struct, "struct"
  ; 19, Large_binary, "large_binary"
  ; 20, Large_utf8, "large_utf8"
  ; 21, Large_list, "large_list"
  ; 23, Binary_view, "binary_view"
  ; 24, Utf8_view, "utf8_view"
  ]
;;

let str kv id = Option.value (string kv id) ~default:""
let units = [ S; Ms; Us; Ns ]

let unit_of c =
  match List.nth_opt units c with
  | Some u -> u
  | None -> fail "the time unit %d is not one of Arrow's" c
  | exception Invalid_argument _ -> fail "a negative time unit"
;;

let decimal_of t =
  let precision = int32 t 0 0
  and scale = int32 t 1 0
  and bits = int32 t 2 128 in
  let most =
    match bits with
    | 32 -> 9
    | 64 -> 18
    | 128 -> 38
    | 256 -> 76
    | b -> fail "a decimal type has %d bits" b
  in
  if precision < 1 || precision > most
  then fail "a decimal of %d bits has precision %d" bits precision;
  Decimal { precision; scale; bits }
;;

let type_of tag t : arrow =
  let t =
    match t with
    | Some t -> t
    | None -> fail "a type is absent"
  in
  match tag with
  | 2 ->
    let bits = int32 t 0 0 in
    if not (List.mem bits [ 8; 16; 32; 64 ]) then fail "an integer type has %d bits" bits;
    Int { bits; signed = bool t 1 false }
  | 3 ->
    (match int16 t 0 0 with
     | (0 | 1 | 2) as p -> Float (16 lsl p)
     | p -> fail "the float precision %d is not one of Arrow's" p)
  | 8 -> if int16 t 0 1 = 0 then Date_day else Date_ms
  | 9 ->
    let u = unit_of (int16 t 0 1)
    and bits = int32 t 1 32 in
    let want =
      match u with
      | S | Ms -> 32
      | Us | Ns -> 64
    in
    if bits <> want then fail "a time type has %d bits, not %d" bits want;
    Time u
  | 10 -> Timestamp (unit_of (int16 t 0 0), str t 1)
  | 7 -> decimal_of t
  | 15 -> Fixed_binary (int32 t 0 0)
  | 16 -> Fixed_list (int32 t 0 0)
  | 17 -> Map
  | 18 -> Duration (unit_of (int16 t 0 1))
  | _ ->
    (match List.find_opt (fun (n, _, _) -> n = tag) nullary with
     | Some (_, a, _) -> a
     | None -> Unsupported (Printf.sprintf "of tag %d" tag))
;;

let extension_name = "ARROW:extension:name"
let extension_metadata = "ARROW:extension:metadata"
let kv_pairs t id = List.map (fun kv -> str kv 0, str kv 1) (tables t id)

let encoding_of d =
  let index_bits, index_signed =
    match table d 1 with
    | None -> 32, true
    | Some i -> int32 i 0 0, bool i 1 false
  in
  { id = int64 d 0 0; index_bits; index_signed }
;;

let rec field budget parent t =
  if !budget = 0 then fail "the schema has more fields than bytes";
  decr budget;
  let raw = kv_pairs t 6 in
  let meta name = List.assoc_opt name raw in
  let reserved (k, _) = k = extension_name || k = extension_metadata in
  let name = str t 0 in
  let path = if parent = "" then name else parent ^ "." ^ name in
  let arrow =
    try type_of (int8 t 2 0) (table t 3) with
    | Malformed m -> fail "column %S: %s" path m
  in
  { name
  ; arrow
  ; encoding = Option.map encoding_of (table t 4)
  ; children = List.map (field budget path) (tables t 5)
  ; metadata = List.filter (fun kv -> not (reserved kv)) raw
  ; extension =
      Option.map
        (fun name -> name, Option.value (meta extension_metadata) ~default:"")
        (meta extension_name)
  }
;;

let schema t =
  let budget = ref ((t.last - t.first) / 4) in
  { fields = List.map (field budget "") (tables t 1)
  ; metadata = kv_pairs t 2
  ; little = int16 t 0 0 = 0
  }
;;

let batch t =
  let pairs id =
    let p, n = structs t id ~size:16 in
    Array.init n (fun i ->
      let q = p + (16 * i) in
      long_at t q, long_at t (q + 8))
  in
  let p, n = structs t 4 ~size:8 in
  let length = int64 t 0 0 in
  if length < 0 then fail "a record batch's length is negative";
  { length
  ; nodes = pairs 1
  ; buffers = pairs 2
  ; compression = Option.map (fun c -> int8 c 0 0) (table t 3)
  ; variadic = List.init n (fun i -> long_at t (p + (8 * i)))
  }
;;

let parse b ~first ~last =
  let t = root b ~first ~last in
  if int16 t 0 0 < 3 then fail "the metadata version is older than V4";
  let h () =
    match table t 2 with
    | Some h -> h
    | None -> fail "a message has no header"
  in
  let dictionary d =
    match table d 1 with
    | None -> fail "a dictionary batch has no data"
    | Some r -> Dictionary { id = int64 d 0 0; delta = bool d 2 false; batch = batch r }
  in
  let header =
    match int8 t 1 0 with
    | 1 -> Some (Schema (schema (h ())))
    | 2 -> Some (dictionary (h ()))
    | 3 -> Some (Batch (batch (h ())))
    | 4 | 5 -> None
    | k -> fail "the message type %d is not one of Arrow's" k
  in
  let body_len = int64 t 3 0 in
  if body_len < 0 then fail "a message's body length is negative";
  { header; body = last; body_len }
;;

let footer b ~first ~last =
  let t = root b ~first ~last in
  let s =
    match table t 1 with
    | Some s -> schema s
    | None -> fail "a file has no schema"
  in
  let blocks id =
    let p, n = structs t id ~size:24 in
    Array.init n (fun i ->
      let q = p + (24 * i) in
      let metadata = u32 t (q + 8) in
      let offset = long_at t q
      and body = long_at t (q + 16) in
      if offset < 0 || body < 0 then fail "a block's offset or length is negative";
      { offset; metadata; body })
  in
  { schema = s; dictionaries = blocks 2; batches = blocks 3 }
;;
