(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Types

type bigbytes = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

exception Malformed of string

val word : bigbytes -> int -> int
val parse : bigbytes -> first:int -> last:int -> message
val footer : bigbytes -> first:int -> last:int -> footer
