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

let batch_bytes = 1 lsl 20
let chunk = 1 lsl 16

type t =
  { channel : In_channel.t
  ; batch : int
  ; sep : char
  ; quote : char
  ; mutable buf : Bytes.t
  ; mutable len : int
  ; mutable eod : bool
  ; mutable start : int
  ; mutable stop : int
  ; mutable line : int
  ; mutable lines : int
  ; mutable ends : int
  ; mutable scanned : int
  ; mutable in_quotes : bool
  ; mutable bounds : int array
  ; mutable records : int array
  ; mutable rows : int
  ; mutable pending : exn option
  }

let make ?(batch = batch_bytes) ~sep ~quote channel =
  { channel
  ; batch
  ; sep
  ; quote
  ; buf = Bytes.create chunk
  ; len = 0
  ; eod = false
  ; start = 0
  ; stop = 0
  ; line = 0
  ; lines = 0
  ; ends = 0
  ; scanned = 0
  ; in_quotes = false
  ; bounds = Array.make 64 0
  ; records = [||]
  ; rows = 0
  ; pending = None
  }
;;

let refill s =
  if s.len + chunk > Bytes.length s.buf
  then (
    let buf = Bytes.create (max (s.len + chunk) (2 * Bytes.length s.buf)) in
    Bytes.blit s.buf 0 buf 0 s.len;
    s.buf <- buf);
  match In_channel.input s.channel s.buf s.len chunk with
  | 0 -> s.eod <- true
  | n -> s.len <- s.len + n
;;

let compact s =
  let k = s.start in
  if k > 0
  then (
    Bytes.blit s.buf k s.buf 0 (s.len - k);
    s.len <- s.len - k;
    s.start <- 0;
    s.scanned <- s.scanned - k)
;;

let skip_bom s =
  while (not s.eod) && s.len < 3 do
    refill s
  done;
  if s.len >= 3 && Bytes.sub_string s.buf 0 3 = "\xEF\xBB\xBF"
  then (
    s.start <- 3;
    s.scanned <- 3)
;;

let rec batch_end s =
  let b = s.buf
  and q = s.quote
  and last = s.start + s.batch - 1 in
  let i = ref s.scanned
  and in_quotes = ref s.in_quotes
  and found = ref (-1) in
  while !found < 0 && !i < s.len do
    let c = Bytes.unsafe_get b !i in
    if c = q
    then in_quotes := not !in_quotes
    else if c = '\n'
    then (
      s.lines <- s.lines + 1;
      if not !in_quotes
      then (
        s.ends <- s.ends + 1;
        if !i >= last then found := !i + 1));
    incr i
  done;
  s.scanned <- !i;
  s.in_quotes <- !in_quotes;
  if !found >= 0
  then !found
  else if s.eod
  then s.len
  else (
    refill s;
    batch_end s)
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

let grow_bounds s n ~keep =
  let a = Array.make n 0 in
  Array.blit s.bounds 0 a 0 keep;
  s.bounds <- a
;;

let push_field s nf first stop =
  if (2 * nf) + 2 > Array.length s.bounds
  then grow_bounds s (2 * Array.length s.bounds) ~keep:(2 * nf);
  Array.unsafe_set s.bounds (2 * nf) first;
  Array.unsafe_set s.bounds ((2 * nf) + 1) stop
;;

let reserve_bounds s nf bytes =
  let n = 2 * min (nf * (s.ends + 1)) (bytes + 1) in
  if n > Array.length s.bounds then grow_bounds s n ~keep:(2 * nf)
;;

let split s stop =
  let b = s.buf
  and sep = s.sep
  and q = s.quote in
  let nf = ref 0
  and rows = ref 0
  and i = ref s.start in
  if s.ends + 2 > Array.length s.records then s.records <- Array.make (s.ends + 2) 0;
  s.records.(0) <- 0;
  (try
     while !i < stop do
       let c = Bytes.unsafe_get b !i in
       if c = '\n' || (c = '\r' && !i + 1 < stop && Bytes.unsafe_get b (!i + 1) = '\n')
       then (
         push_field s !nf !i !i;
         incr nf;
         i := !i + if c = '\n' then 1 else 2;
         s.records.(!rows + 1) <- !nf;
         incr rows;
         if !rows = 1 then reserve_bounds s !nf (stop - s.start))
       else (
         let record_ends = ref false in
         while not !record_ends do
           let first = !i in
           let j = ref first in
           if first < stop && Bytes.unsafe_get b first = q
           then (
             let closed = ref false in
             incr j;
             while not !closed do
               if !j >= stop then raise (Syntax (first, "a quoted field does not end"));
               let c = Bytes.unsafe_get b !j in
               if c <> q
               then incr j
               else if !j + 1 < stop && Bytes.unsafe_get b (!j + 1) = q
               then j := !j + 2
               else (
                 closed := true;
                 incr j)
             done)
           else (
             while
               !j < stop
               &&
               let c = Bytes.unsafe_get b !j in
               c <> sep && c <> '\n' && c <> '\r' && c <> q
             do
               incr j
             done;
             if !j < stop && Bytes.unsafe_get b !j = q
             then raise (Syntax (!j, "a quote in a field that does not start with one")));
           push_field s !nf first !j;
           incr nf;
           let j = !j in
           if j = stop
           then (
             i := stop;
             record_ends := true)
           else (
             let c = Bytes.unsafe_get b j in
             if c = sep
             then i := j + 1
             else if c = '\n'
             then (
               i := j + 1;
               record_ends := true)
             else if c = '\r' && j + 1 < stop && Bytes.unsafe_get b (j + 1) = '\n'
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
         done;
         s.records.(!rows + 1) <- !nf;
         incr rows;
         if !rows = 1 then reserve_bounds s !nf (stop - s.start))
     done
   with
   | Syntax (at, msg) ->
     let line, column = locate s at in
     s.pending <- Some (Error { line; column; msg }));
  s.rows <- !rows
;;

let rec next s =
  Option.iter raise s.pending;
  if s.line = 0
  then (
    s.line <- 1;
    skip_bom s)
  else (
    s.line <- s.line + s.lines;
    s.start <- s.stop;
    compact s);
  s.lines <- 0;
  s.ends <- 0;
  let stop = batch_end s in
  s.stop <- stop;
  if stop = s.start
  then false
  else (
    split s stop;
    if s.rows > 0
    then true
    else (
      match s.pending with
      | Some e -> raise e
      | None -> next s))
;;

let rows s = s.rows
let fields s r = s.records.(r + 1) - s.records.(r)
let bytes s = s.buf
let start s r j = s.bounds.(2 * (s.records.(r) + j))
let stop s r j = s.bounds.((2 * (s.records.(r) + j)) + 1)

let quoted s r j =
  let k = 2 * (s.records.(r) + j) in
  let first = Array.unsafe_get s.bounds k in
  Array.unsafe_get s.bounds (k + 1) > first && Bytes.unsafe_get s.buf first = s.quote
;;

let pos s r j = if quoted s r j then start s r j + 1 else start s r j

let len s r j =
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
