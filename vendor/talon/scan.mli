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

val make : ?batch:int -> sep:char -> quote:char -> In_channel.t -> t
val next : t -> bool
val rows : t -> int
val fields : t -> int -> int
val bytes : t -> Bytes.t
val quoted : t -> int -> int -> bool
val pos : t -> int -> int -> int
val len : t -> int -> int -> int
val text : t -> int -> int -> string
val line : t -> int -> int
