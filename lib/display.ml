let float x =
  let text =
    List.find
      (fun t -> float_of_string t = x)
      (List.init 17 (fun p -> Printf.sprintf "%.*e" p x))
  in
  let negative = String.starts_with ~prefix:"-" text in
  let mantissa, exponent =
    match String.split_on_char 'e' text with
    | [ m; e ] -> m, int_of_string e
    | _ -> text, 0
  in
  let digits =
    String.concat "" (String.split_on_char '.' mantissa)
    |> fun d -> if negative then String.sub d 1 (String.length d - 1) else d
  in
  let sign = if negative then "-" else "" in
  let count = String.length digits in
  if exponent < -4 || exponent >= 16
  then
    sign
    ^ (if count > 1
       then String.sub digits 0 1 ^ "." ^ String.sub digits 1 (count - 1)
       else digits)
    ^ Printf.sprintf "e%c%02d" (if exponent < 0 then '-' else '+') (abs exponent)
  else if exponent < 0
  then sign ^ "0." ^ String.make (-exponent - 1) '0' ^ digits
  else (
    let padded = digits ^ String.make (max 0 (exponent + 1 - count)) '0' in
    let whole = String.sub padded 0 (exponent + 1) in
    let fraction =
      String.sub padded (exponent + 1) (String.length padded - exponent - 1)
    in
    sign ^ whole ^ "." ^ if fraction = "" then "0" else fraction)
;;

let rounded ~digits value = Printf.sprintf "%.*f" digits value
let bool b = if b then "TRUE" else "FALSE"

let width text =
  let rec count i n =
    if i >= String.length text
    then n
    else count (i + Uchar.utf_decode_length (String.get_utf_8_uchar text i)) (n + 1)
  in
  count 0 0
;;

let count n noun = Printf.sprintf "%d %s%s" n noun (if n = 1 then "" else "s")
