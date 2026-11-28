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

external get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"

let ones = 0x0101010101010101L
let highs = 0x8080808080808080L
let[@inline] spread c = Int64.mul ones (Int64.of_int (Char.code c))

let[@inline] holds word pattern =
  let x = Int64.logxor word pattern in
  Int64.logand (Int64.logand (Int64.sub x ones) (Int64.lognot x)) highs <> 0L
;;

let batch_bytes = 1 lsl 20
let chunk = 1 lsl 16

type t =
  { channel : In_channel.t
  ; batch : int
  ; chunk : int
  ; sep : char
  ; quote : char
  ; mutable buf : Bytes.t
  ; mutable len : int
  ; mutable eod : bool
  ; mutable start : int
  ; mutable stop : int
  ; mutable line : int
  ; mutable lines : int
  ; mutable bounds : int array
  ; mutable records : int array
  ; mutable rows : int
  ; mutable pending : exn option
  }

let make ?(batch = batch_bytes) ?(chunk = chunk) ~sep ~quote channel =
  { channel
  ; batch
  ; chunk
  ; sep
  ; quote
  ; buf = Bytes.create (max chunk 16)
  ; len = 0
  ; eod = false
  ; start = 0
  ; stop = 0
  ; line = 0
  ; lines = 0
  ; bounds = Array.make 64 0
  ; records = [||]
  ; rows = 0
  ; pending = None
  }
;;

let refill s =
  if s.len + s.chunk > Bytes.length s.buf
  then (
    let buf = Bytes.create (max (s.len + s.chunk) (2 * Bytes.length s.buf)) in
    Bytes.blit s.buf 0 buf 0 s.len;
    s.buf <- buf);
  match In_channel.input s.channel s.buf s.len s.chunk with
  | 0 -> s.eod <- true
  | n -> s.len <- s.len + n
;;

let compact s =
  let k = s.start in
  if k > 0
  then (
    Bytes.blit s.buf k s.buf 0 (s.len - k);
    s.len <- s.len - k;
    s.start <- 0)
;;

let skip_bom s =
  while (not s.eod) && s.len < 3 do
    refill s
  done;
  if s.len >= 3 && Bytes.sub_string s.buf 0 3 = "\xEF\xBB\xBF" then s.start <- 3
;;

let locate s i =
  let line = ref s.line
  and first = ref s.start in
  for k = s.start to i - 1 do
    if Bytes.unsafe_get s.buf k = '\n'
    then (
      incr line;
      first := k + 1)
  done;
  !line, i - !first + 1
;;

exception Syntax of int * string
exception Short

let grow_bounds s n ~keep =
  let a = Array.make n 0 in
  Array.blit s.bounds 0 a 0 keep;
  s.bounds <- a
;;

let[@inline] push_field s nf first stop =
  if (2 * nf) + 2 > Array.length s.bounds
  then grow_bounds s (2 * Array.length s.bounds) ~keep:(2 * nf);
  Array.unsafe_set s.bounds (2 * nf) first;
  Array.unsafe_set s.bounds ((2 * nf) + 1) stop
;;

let end_record s rows nf =
  if rows + 2 > Array.length s.records
  then (
    let a = Array.make (2 * (rows + 2)) 0 in
    Array.blit s.records 0 a 0 (rows + 1);
    s.records <- a);
  s.records.(rows + 1) <- nf
;;

let split s =
  let sep = s.sep
  and q = s.quote in
  let seps = spread sep
  and lines = spread '\n'
  and returns = spread '\r'
  and quotes = spread q in
  let target = s.start + s.batch in
  let nf = ref 0
  and rows = ref 0
  and i = ref s.start in
  if Array.length s.records < 2 then s.records <- Array.make 1024 0;
  s.records.(0) <- 0;
  s.lines <- 0;
  let more () = if not s.eod then raise Short in
  let record () =
    let b = s.buf
    and limit = s.len in
    let quoted_lines = ref 0 in
    let c = Bytes.unsafe_get b !i in
    if c = '\r' && !i + 1 = limit then more ();
    if c = '\n' || (c = '\r' && !i + 1 < limit && Bytes.unsafe_get b (!i + 1) = '\n')
    then (
      push_field s !nf !i !i;
      incr nf;
      i := !i + if c = '\n' then 1 else 2)
    else (
      let record_ends = ref false in
      while not !record_ends do
        let first = !i in
        let j = ref first in
        if first < limit && Bytes.unsafe_get b first = q
        then (
          let closed = ref false in
          incr j;
          while not !closed do
            if !j >= limit
            then (
              more ();
              raise (Syntax (first, "a quoted field does not end")));
            let c = Bytes.unsafe_get b !j in
            if c <> q
            then (
              if c = '\n' then incr quoted_lines;
              incr j)
            else if !j + 1 = limit && not s.eod
            then raise Short
            else if !j + 1 < limit && Bytes.unsafe_get b (!j + 1) = q
            then j := !j + 2
            else (
              closed := true;
              incr j)
          done)
        else (
          while
            !j + 8 <= limit
            &&
            let w = get64 b !j in
            not (holds w seps || holds w lines || holds w returns || holds w quotes)
          do
            j := !j + 8
          done;
          while
            !j < limit
            &&
            let c = Bytes.unsafe_get b !j in
            c <> sep && c <> '\n' && c <> '\r' && c <> q
          do
            incr j
          done;
          if !j < limit && Bytes.unsafe_get b !j = q
          then raise (Syntax (!j, "a quote in a field that does not start with one")));
        let j = !j in
        if j = limit then more ();
        if j < limit && Bytes.unsafe_get b j = '\r' && j + 1 = limit then more ();
        push_field s !nf first j;
        incr nf;
        if j = limit
        then (
          i := limit;
          record_ends := true)
        else (
          let c = Bytes.unsafe_get b j in
          if c = sep
          then i := j + 1
          else if c = '\n'
          then (
            i := j + 1;
            record_ends := true)
          else if c = '\r' && j + 1 < limit && Bytes.unsafe_get b (j + 1) = '\n'
          then (
            i := j + 2;
            record_ends := true)
          else if c = '\r'
          then raise (Syntax (j, "a carriage return that no line feed follows"))
          else
            raise
              (Syntax
                 ( j
                 , "a quoted field's closing quote is followed by a byte other than a \
                    separator or a line break" )))
      done);
    end_record s !rows !nf;
    incr rows;
    s.lines <- s.lines + 1 + !quoted_lines
  in
  (try
     let rec go () =
       if (!rows > 0 && !i >= target) || (!i >= s.len && s.eod)
       then ()
       else if !i >= s.len
       then (
         refill s;
         go ())
       else (
         let field = !nf
         and at = !i in
         match record () with
         | () -> go ()
         | exception Short ->
           nf := field;
           i := at;
           refill s;
           go ())
     in
     go ()
   with
   | Syntax (at, msg) ->
     let line, column = locate s at in
     s.pending <- Some (Error { line; column; msg }));
  s.stop <- !i;
  s.rows <- !rows
;;

let next s =
  Option.iter raise s.pending;
  if s.line = 0
  then (
    s.line <- 1;
    skip_bom s)
  else (
    s.line <- s.line + s.lines;
    s.start <- s.stop;
    compact s);
  split s;
  if s.rows > 0
  then true
  else (
    match s.pending with
    | Some e -> raise e
    | None -> false)
;;

let[@inline] rows s = s.rows
let[@inline] fields s r = s.records.(r + 1) - s.records.(r)
let[@inline] bytes s = s.buf
let[@inline] start s r j = s.bounds.(2 * (s.records.(r) + j))
let[@inline] stop s r j = s.bounds.((2 * (s.records.(r) + j)) + 1)

let[@inline] quoted s r j =
  let k = 2 * (s.records.(r) + j) in
  let first = Array.unsafe_get s.bounds k in
  Array.unsafe_get s.bounds (k + 1) > first && Bytes.unsafe_get s.buf first = s.quote
;;

let[@inline] pos s r j = if quoted s r j then start s r j + 1 else start s r j

let[@inline] len s r j =
  let n = stop s r j - start s r j in
  if quoted s r j then n - 2 else n
;;

let text s r j =
  let first = pos s r j
  and n = len s r j in
  if not (quoted s r j)
  then Bytes.sub_string s.buf first n
  else (
    let b = Buffer.create n in
    let i = ref first in
    while !i < first + n do
      let c = Bytes.unsafe_get s.buf !i in
      Buffer.add_char b c;
      i := if c = s.quote then !i + 2 else !i + 1
    done;
    Buffer.contents b)
;;

let line s r = fst (locate s (start s r 0))
