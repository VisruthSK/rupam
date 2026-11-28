(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type bigbytes = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Error of int * string

type t

val make : bigbytes -> pos:int -> limit:int -> t
val pos : t -> int

type ty

val fields : t -> (int -> ty -> unit) -> unit
val structure : t -> ty -> (int -> ty -> unit) -> unit
val bool : t -> ty -> bool
val i8 : t -> ty -> int
val i32 : t -> ty -> int
val i64 : t -> ty -> int
val binary : t -> ty -> string
val list : t -> ty -> (t -> ty -> 'a) -> 'a list
val skip : t -> ty -> unit

type value =
  | Bool of bool
  | I8 of int
  | I32 of int
  | I64 of int
  | Binary of string
  | List of value list
  | Struct of (int * value) list

val encode : Buffer.t -> value -> unit
