type candidates =
  { mutable int : bool
  ; mutable float : bool
  ; mutable bool : bool
  ; mutable date : bool
  ; mutable present : bool
  ; mutable missing : bool
  ; mutable nan : bool
  ; mutable infinite : int option
  }

let fresh () =
  { int = true
  ; float = true
  ; bool = true
  ; date = true
  ; present = false
  ; missing = false
  ; nan = false
  ; infinite = None
  }
;;

external get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"

let int_bit = 1
let float_bit = 2
let bool_bit = 4
let date_bit = 8
let nan_bit = 16
let inf_bit = 32
let byte b i = Bytes.unsafe_get b i
let is_digit c = c >= '0' && c <= '9'

let word b p n s =
  String.length s = n
  &&
  let i = ref 0 in
  while !i < n && byte b (p + !i) = String.unsafe_get s !i do
    incr i
  done;
  !i = n
;;

let rec member b p n = function
  | [] -> false
  | token :: rest -> word b p n token || member b p n rest
;;

let largest = "9223372036854775807"

let fits b first =
  let i = ref 0 in
  while !i < 19 && byte b (first + !i) = String.unsafe_get largest !i do
    incr i
  done;
  !i = 19 || byte b (first + !i) < String.unsafe_get largest !i
;;

let nibbles = 0xF0F0F0F0F0F0F0F0L
let zeros = 0x3030303030303030L
let sixes = 0x0606060606060606L

let[@inline] eight_digits w =
  Int64.equal (Int64.logand w nibbles) zeros
  && Int64.equal (Int64.logand (Int64.add w sixes) nibbles) zeros
;;

let digits_end b i stop =
  let i = ref i in
  while !i + 8 <= stop && eight_digits (get64 b !i) do
    i := !i + 8
  done;
  while !i < stop && is_digit (byte b !i) do
    incr i
  done;
  !i
;;

let fraction_end b i stop =
  if i < stop && byte b i = '.'
  then (
    let j = digits_end b (i + 1) stop in
    if j > i + 1 then j else -1)
  else i
;;

let exponent_end b i stop =
  if i >= 0 && i < stop && (byte b i = 'e' || byte b i = 'E')
  then (
    let s =
      if i + 1 < stop && (byte b (i + 1) = '+' || byte b (i + 1) = '-')
      then i + 2
      else i + 1
    in
    let j = digits_end b s stop in
    if j > s then j else -1)
  else i
;;

let number b p n =
  let stop = p + n in
  let first = if byte b p = '-' then p + 1 else p in
  let whole = digits_end b first stop in
  let digits = whole - first in
  if digits = 0 || (digits > 1 && byte b first = '0')
  then 0
  else if whole = stop
  then if digits < 19 || (digits = 19 && fits b first) then int_bit lor float_bit else 0
  else if exponent_end b (fraction_end b whole stop) stop = stop
  then float_bit
  else 0
;;

let two b p = ((Char.code (byte b p) - 48) * 10) + Char.code (byte b (p + 1)) - 48

let date b p n =
  n = 10
  && byte b (p + 4) = '-'
  && byte b (p + 7) = '-'
  && is_digit (byte b p)
  && is_digit (byte b (p + 1))
  && is_digit (byte b (p + 2))
  && is_digit (byte b (p + 3))
  && is_digit (byte b (p + 5))
  && is_digit (byte b (p + 6))
  && is_digit (byte b (p + 8))
  && is_digit (byte b (p + 9))
  &&
  let year = (two b p * 100) + two b (p + 2)
  and month = two b (p + 5)
  and day = two b (p + 8) in
  let leap = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 in
  let days =
    match month with
    | 2 -> if leap then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | _ -> 31
  in
  month >= 1 && month <= 12 && day >= 1 && day <= days
;;

let classify b p n =
  if n = 0
  then 0
  else (
    match byte b p with
    | '0' .. '9' -> number b p n lor if date b p n then date_bit else 0
    | '-' ->
      if word b p n "-inf" || word b p n "-Inf" || word b p n "-Infinity"
      then inf_bit
      else number b p n
    | 'T' | 't' ->
      if word b p n "TRUE" || word b p n "true" || word b p n "True" then bool_bit else 0
    | 'F' | 'f' ->
      if word b p n "FALSE" || word b p n "false" || word b p n "False"
      then bool_bit
      else 0
    | 'N' -> if word b p n "NaN" then nan_bit else 0
    | 'i' | 'I' ->
      if word b p n "inf" || word b p n "Inf" || word b p n "Infinity" then inf_bit else 0
    | _ -> 0)
;;

let valid_utf_8 b p n =
  let stop = p + n in
  let i = ref p in
  while !i + 8 <= stop && Int64.logand (get64 b !i) 0x8080808080808080L = 0L do
    i := !i + 8
  done;
  let ok = ref true in
  while !ok && !i < stop do
    if Char.code (byte b !i) < 0x80
    then incr i
    else (
      let d = Bytes.get_utf_8_uchar b !i in
      let length = Uchar.utf_decode_length d in
      if Uchar.utf_decode_is_valid d && !i + length <= stop
      then i := !i + length
      else ok := false)
  done;
  !ok
;;

let observe c b p n ~missing ~longest ~scan ~row =
  if n <= longest && member b p n missing
  then c.missing <- true
  else (
    c.present <- true;
    if c.int || c.float || c.bool || c.date
    then (
      let m = classify b p n in
      if m land nan_bit <> 0
      then (
        c.nan <- true;
        c.int <- false;
        c.bool <- false;
        c.date <- false)
      else if m land inf_bit <> 0
      then (
        if c.infinite = None then c.infinite <- Some (Talon.Scan.line scan row);
        c.int <- false;
        c.bool <- false;
        c.date <- false)
      else (
        if m land int_bit = 0 then c.int <- false;
        if m land float_bit = 0 then c.float <- false;
        if m land bool_bit = 0 then c.bool <- false;
        if m land date_bit = 0 then c.date <- false)))
;;

let columns ~missing ~path format : Input.column list =
  let longest = List.fold_left (fun m t -> max m (String.length t)) 0 missing in
  let sep =
    match (format : Input.format) with
    | Tsv -> '\t'
    | Csv | Parquet -> ','
  in
  let fail line message =
    Diagnostic.fail Unreadable_input (Printf.sprintf "%s:%d: %s" path line message)
  in
  In_channel.with_open_bin path (fun channel ->
    let s = Talon.Scan.make ~sep ~quote:'"' channel in
    let names = ref [||]
    and state = ref [||]
    and header = ref true in
    (try
       while Talon.Scan.next s do
         let b = Talon.Scan.bytes s in
         let rows = Talon.Scan.rows s in
         let first = Talon.Scan.start s 0 0
         and last = Talon.Scan.stop s (rows - 1) (Talon.Scan.fields s (rows - 1) - 1) in
         if not (valid_utf_8 b first (last - first))
         then
           for r = 0 to rows - 1 do
             for j = 0 to Talon.Scan.fields s r - 1 do
               if not (valid_utf_8 b (Talon.Scan.pos s r j) (Talon.Scan.len s r j))
               then
                 fail
                   (Talon.Scan.line s r)
                   (Printf.sprintf "field %d is not valid UTF-8" (j + 1))
             done
           done;
         for r = 0 to rows - 1 do
           let fields = Talon.Scan.fields s r in
           if !header
           then (
             header := false;
             let given = Array.init fields (Talon.Scan.text s r) in
             Array.iteri
               (fun j name ->
                  if name = ""
                  then fail 1 (Printf.sprintf "column %d has an empty name" (j + 1));
                  for k = 0 to j - 1 do
                    if String.equal given.(k) name
                    then fail 1 (Printf.sprintf "the column name %s appears twice" name)
                  done)
               given;
             names := given;
             state := Array.init fields (fun _ -> fresh ()))
           else (
             if fields <> Array.length !names
             then
               fail
                 (Talon.Scan.line s r)
                 (Printf.sprintf
                    "expected %d fields, found %d"
                    (Array.length !names)
                    fields);
             for j = 0 to fields - 1 do
               let c = !state.(j) in
               if c.int || c.float || c.bool || c.date || not (c.present && c.missing)
               then (
                 let first = Talon.Scan.start s r j
                 and last = Talon.Scan.stop s r j in
                 if Talon.Scan.quoted s r j
                 then
                   observe
                     c
                     ~missing
                     ~longest
                     b
                     (first + 1)
                     (last - first - 2)
                     ~scan:s
                     ~row:r
                 else observe c ~missing ~longest b first (last - first) ~scan:s ~row:r)
             done)
         done
       done
     with
     | Talon.Scan.Error { line; column; msg } ->
       fail line (Printf.sprintf "column %d: %s" column msg));
    if !header then fail 1 "the file has no header";
    Array.to_list
      (Array.mapi
         (fun j name ->
            let c = !state.(j) in
            let typ : Types.t =
              if not c.present
              then String
              else if c.int
              then Int
              else if c.float
              then (
                Option.iter
                  (fun line ->
                     fail line (Printf.sprintf "column %s holds an infinite value" name))
                  c.infinite;
                Float)
              else if c.bool
              then Bool
              else if c.date
              then Date
              else String
            in
            let nullable = c.missing || (typ = Float && c.nan) in
            ({ name; typ = Supported typ; nullable } : Input.column))
         !names))
;;
