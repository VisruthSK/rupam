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

let digit c = c >= '0' && c <= '9'

let digits b p n =
  let rec go i = i = n || (digit (Bytes.unsafe_get b (p + i)) && go (i + 1)) in
  n > 0 && go 0
;;

let equals b p n s =
  String.length s = n
  &&
  let rec go i =
    i = n || (Bytes.unsafe_get b (p + i) = String.unsafe_get s i && go (i + 1))
  in
  go 0
;;

let unsigned_mantissa b p n =
  let rec go i = if i < n && digit (Bytes.unsafe_get b (p + i)) then go (i + 1) else i in
  let k = go 0 in
  if k = 0 || (k > 1 && Bytes.unsafe_get b p = '0') then -1 else k
;;

let sign b p n = if n > 0 && Bytes.unsafe_get b p = '-' then 1 else 0
let largest = "9223372036854775807"

let integer b p n =
  let s = sign b p n in
  let k = unsigned_mantissa b (p + s) (n - s) in
  k = n - s
  && (k < 19
      || (k = 19
          &&
          let rec go i =
            i = 19
            ||
            let c = Bytes.unsafe_get b (p + s + i) in
            c < largest.[i] || (c = largest.[i] && go (i + 1))
          in
          go 0))
;;

let decimal b p n =
  let s = sign b p n in
  let k = unsigned_mantissa b (p + s) (n - s) in
  k > 0
  &&
  let i = p + s + k
  and stop = p + n in
  let i, fraction =
    if i < stop && Bytes.unsafe_get b i = '.'
    then (
      let rec go j = if j < stop && digit (Bytes.unsafe_get b j) then go (j + 1) else j in
      let j = go (i + 1) in
      if j = i + 1 then -1, false else j, true)
    else i, false
  in
  i >= 0
  &&
  let i, exponent =
    if i < stop && (Bytes.unsafe_get b i = 'e' || Bytes.unsafe_get b i = 'E')
    then (
      let j =
        if
          i + 1 < stop
          && (Bytes.unsafe_get b (i + 1) = '+' || Bytes.unsafe_get b (i + 1) = '-')
        then i + 2
        else i + 1
      in
      if digits b j (stop - j) then stop, true else -1, false)
    else i, false
  in
  i = stop && (fraction || exponent)
;;

let infinities = [ "inf"; "-inf"; "Inf"; "-Inf"; "Infinity"; "-Infinity" ]

let date b p n =
  n = 10
  && digits b p 4
  && Bytes.unsafe_get b (p + 4) = '-'
  && digits b (p + 5) 2
  && Bytes.unsafe_get b (p + 7) = '-'
  && digits b (p + 8) 2
  &&
  let number i k =
    let rec go j acc =
      if j = k
      then acc
      else go (j + 1) ((10 * acc) + Char.code (Bytes.unsafe_get b (p + i + j)) - 48)
    in
    go 0 0
  in
  let year = number 0 4
  and month = number 5 2
  and day = number 8 2 in
  let leap = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 in
  let days =
    match month with
    | 2 -> if leap then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | _ -> 31
  in
  month >= 1 && month <= 12 && day >= 1 && day <= days
;;

let valid_utf_8 b p n =
  let rec go i =
    i >= p + n
    ||
    if Char.code (Bytes.unsafe_get b i) < 0x80
    then go (i + 1)
    else (
      let d = Bytes.get_utf_8_uchar b i in
      Uchar.utf_decode_is_valid d
      && i + Uchar.utf_decode_length d <= p + n
      && go (i + Uchar.utf_decode_length d))
  in
  go p
;;

let observe c b p n ~missing ~scan ~row =
  if List.exists (equals b p n) missing
  then c.missing <- true
  else (
    c.present <- true;
    if c.int || c.float || c.bool || c.date
    then
      if equals b p n "NaN"
      then (
        c.nan <- true;
        c.int <- false;
        c.bool <- false;
        c.date <- false)
      else if List.exists (equals b p n) infinities
      then (
        if c.infinite = None then c.infinite <- Some (Talon.Scan.line scan row);
        c.int <- false;
        c.bool <- false;
        c.date <- false)
      else (
        let whole = integer b p n in
        if c.int && not whole then c.int <- false;
        if c.float && not (whole || decimal b p n) then c.float <- false;
        if c.bool && not (equals b p n "TRUE" || equals b p n "FALSE")
        then c.bool <- false;
        if c.date && not (date b p n) then c.date <- false))
;;

let columns ~missing ~path format : Input.column list =
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
         for r = 0 to Talon.Scan.rows s - 1 do
           let fields = Talon.Scan.fields s r in
           for j = 0 to fields - 1 do
             if not (valid_utf_8 b (Talon.Scan.pos s r j) (Talon.Scan.len s r j))
             then
               fail
                 (Talon.Scan.line s r)
                 (Printf.sprintf "field %d is not valid UTF-8" (j + 1))
           done;
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
               then
                 observe
                   c
                   ~missing
                   b
                   (Talon.Scan.pos s r j)
                   (Talon.Scan.len s r j)
                   ~scan:s
                   ~row:r
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
