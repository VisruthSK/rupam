let keywords = ["and";"or";"not";"true";"false";"None";"in";"let";"use";"contract";
                "as";"fn";"if";"then";"else";"type";"match";"import"]

let valid s =
  let rec scan i =
    if i = String.length s then i > 0 else
    let decoded = String.get_utf_8_uchar s i in
    let u = Uchar.utf_decode_uchar decoded in
    Uchar.utf_decode_is_valid decoded &&
    (Uchar.to_int u = 95 || (if i=0 then Uucp.Id.is_xid_start else Uucp.Id.is_xid_continue) u) &&
    scan (i + Uchar.utf_decode_length decoded) in
  scan 0

let quote name =
  if valid name && not (List.mem name keywords) then name else
  "`" ^ String.concat "``" (String.split_on_char '`' name) ^ "`"
