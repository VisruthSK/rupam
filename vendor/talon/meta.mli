(*---------------------------------------------------------------------------
  Copyright (c) 2026 The Raven authors. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type bigbytes = Thrift.bigbytes

exception
  Error of
    { row_group : int option
    ; bytes : (int * int) option
    ; text : string option
    ; msg : string
    }

val fail
  :  ?row_group:int
  -> ?bytes:int * int
  -> ?text:string
  -> ('a, unit, string, 'b) format4
  -> 'a

type physical =
  | Boolean
  | Int32
  | Int64
  | Int96
  | Float
  | Double
  | Byte_array
  | Fixed_len_byte_array

type repetition =
  | Required
  | Optional
  | Repeated

type logical =
  | String
  | Map
  | List
  | Enum
  | Decimal of
      { precision : int
      ; scale : int
      }
  | Date
  | Time of
      { unit_ : Types.unit_
      ; utc : bool
      }
  | Timestamp of
      { unit_ : Types.unit_
      ; utc : bool
      }
  | Integer of
      { bits : int
      ; signed : bool
      }
  | Null
  | Json
  | Bson
  | Uuid
  | Float16
  | Variant
  | Geometry
  | Geography
  | Other of int

type element =
  { name : string
  ; physical : physical option
  ; length : int
  ; repetition : repetition option
  ; children : int
  ; converted : int option
  ; precision : int
  ; scale : int
  ; logical : logical option
  }

type codec =
  | Uncompressed
  | Snappy
  | Gzip
  | Lzo
  | Brotli
  | Lz4
  | Zstd
  | Lz4_raw
  | Unknown_codec of int

type stats =
  { nulls : int option
  ; nans : int option
  ; min : string option
  ; max : string option
  ; min_exact : bool option
  ; max_exact : bool option
  }

type column_meta =
  { physical : physical
  ; path : string list
  ; codec : codec
  ; values : int
  ; uncompressed_size : int
  ; compressed_size : int
  ; data_page : int
  ; dictionary_page : int option
  ; stats : stats
  }

type chunk =
  { file_path : string option
  ; encrypted : bool
  ; meta : column_meta option
  }

type row_group =
  { rows : int
  ; chunks : chunk array
  }

type file =
  { rows : int
  ; schema : element array
  ; row_groups : row_group array
  ; footer : int
  }

val footer : bigbytes -> file
val metadata : bigbytes -> footer:int -> file

type encoding =
  | Plain
  | Plain_dictionary
  | Rle
  | Bit_packed
  | Delta_binary_packed
  | Delta_length_byte_array
  | Delta_byte_array
  | Rle_dictionary
  | Byte_stream_split
  | Unknown_encoding of int

type page =
  | Data of
      { values : int
      ; encoding : encoding
      ; levels : encoding
      ; repetition : encoding option
      }
  | Data_v2 of
      { values : int
      ; nulls : int
      ; rows : int
      ; encoding : encoding
      ; definition_bytes : int
      ; repetition_bytes : int
      ; compressed : bool
      }
  | Dictionary of
      { values : int
      ; encoding : encoding
      }
  | Other

type header =
  { page : page
  ; uncompressed_size : int
  ; compressed_size : int
  ; crc : int option
  }

val page_header : bigbytes -> pos:int -> limit:int -> header * int

type column_out =
  { column : column_meta
  ; encodings : encoding list
  }

val footer_bytes
  :  schema:element array
  -> row_groups:(column_out list * int) list
  -> rows:int
  -> leaves:int
  -> created_by:string
  -> string

val data_page_bytes
  :  values:int
  -> encoding:encoding
  -> uncompressed:int
  -> compressed:int
  -> string

val dictionary_page_bytes : values:int -> uncompressed:int -> compressed:int -> string
