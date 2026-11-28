(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

exception
  Error of
    { line : int
    ; column : int
    ; msg : string
    }

type t

val make
  :  ?batch:int
  -> ?chunk:int
  -> ?on_read:(Bytes.t -> int -> int -> unit)
  -> sep:char
  -> quote:char
  -> In_channel.t
  -> t

type batch

val next : t -> batch option
val rows : batch -> int
val fields : batch -> int -> int
val bytes : batch -> Bytes.t
val start : batch -> int -> int -> int
val stop : batch -> int -> int -> int
val quoted : batch -> int -> int -> bool
val pos : batch -> int -> int -> int
val len : batch -> int -> int -> int
val text : batch -> int -> int -> string
val line : batch -> int -> int
