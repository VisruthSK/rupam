let () =
  let channel = open_in "../examples/shop.rp" in
  let program = Rupam.Frontend.parse (Lexing.from_channel channel) in
  close_in channel;
  assert (List.length program = 8)

let () =
  In_channel.with_open_text "../examples/shop.rp" (fun ch ->
    let format buf = Rupam.Format.program (Rupam.Frontend.parse buf) in
    let once = format (Lexing.from_channel ch) in
    assert (format (Lexing.from_string once) = once))

