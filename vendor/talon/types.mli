(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type unit_ =
  | S
  | Ms
  | Us
  | Ns

type arrow =
  | Bool
  | Int of
      { bits : int
      ; signed : bool
      }
  | Float of int
  | Utf8
  | Large_utf8
  | Utf8_view
  | Binary
  | Large_binary
  | Binary_view
  | Fixed_binary of int
  | Date_day
  | Date_ms
  | Time of unit_
  | Timestamp of unit_ * string
  | Duration of unit_
  | Decimal of
      { precision : int
      ; scale : int
      ; bits : int
      }
  | List
  | Large_list
  | Fixed_list of int
  | Map
  | Struct
  | Unsupported of string

type encoding =
  { id : int
  ; index_bits : int
  ; index_signed : bool
  }

type field =
  { name : string
  ; arrow : arrow
  ; encoding : encoding option
  ; children : field list
  ; metadata : (string * string) list
  ; extension : (string * string) option
  }

type schema =
  { fields : field list
  ; metadata : (string * string) list
  ; little : bool
  }

type batch =
  { length : int
  ; nodes : (int * int) array
  ; buffers : (int * int) array
  ; compression : int option
  ; variadic : int list
  }

type header =
  | Schema of schema
  | Dictionary of
      { id : int
      ; delta : bool
      ; batch : batch
      }
  | Batch of batch

type message =
  { header : header option
  ; body : int
  ; body_len : int
  }

type block =
  { offset : int
  ; metadata : int
  ; body : int
  }

type footer =
  { schema : schema
  ; dictionaries : block array
  ; batches : block array
  }
