module T = Matrix_test.Terminal
module S = Matrix_test.Screen
module C = Matrix_test.Child
module K = Matrix.Input.Keystroke

let rupam = Sys.getenv "RUPAM_TEST_BIN"

let rows =
  try int_of_string (Sys.getenv "FRAMES_ROWS") with
  | Not_found -> 1_000_000
;;

let text term = S.text (T.screen term)

let generate dir =
  let script =
    Printf.sprintf
      "import polars as pl\n\
       n = %d\n\
       pl.DataFrame({'id': range(n), 'group': [f'g{i %% 97}' for i in range(n)], \
       'value': [i * 0.5 for i in range(n)], 'flag': [i %% 3 == 0 for i in \
       range(n)]}).write_parquet('big.parquet')\n"
      rows
  in
  let file = Filename.concat dir "make.py" in
  Out_channel.with_open_text file (fun oc -> output_string oc script);
  if Sys.command (Printf.sprintf "cd %s && python3 make.py" (Filename.quote dir)) <> 0
  then failwith "could not generate data";
  Out_channel.with_open_text (Filename.concat dir "frames.rp") (fun oc ->
    output_string oc "let big = read_parquet(\"big.parquet\")\n")
;;

let percentile xs p =
  let a = Array.of_list (List.sort compare xs) in
  a.(min (Array.length a - 1) (int_of_float (p *. float_of_int (Array.length a))))
;;

let measure c term name key count =
  let times =
    List.init count (fun _ ->
      let before = text term in
      let start = Unix.gettimeofday () in
      C.send c (T.key term key);
      C.wait c ~within:10. (fun term -> text term <> before);
      (Unix.gettimeofday () -. start) *. 1000.)
  in
  Printf.printf
    "%-10s p50 %6.1f ms   p95 %6.1f ms   max %6.1f ms   (%d presses)\n%!"
    name
    (percentile times 0.5)
    (percentile times 0.95)
    (List.fold_left max 0. times)
    count
;;

let () =
  let dir = Filename.temp_dir "rupam-frames" "" in
  generate dir;
  let term = T.create ~cols:120 ~rows:40 () in
  let env = [ "PATH", Sys.getenv "PATH"; "HOME", dir; "TERM", "xterm-256color" ] in
  C.with_spawn
    term
    ~env
    ~cwd:dir
    ~prog:rupam
    ~args:[ "repl"; "--target"; "python-polars"; "frames.rp" ]
    (fun c ->
       let start = Unix.gettimeofday () in
       C.wait c ~within:600. (fun term ->
         Astring.String.is_infix ~affix:"rupam \xe2\x9d\xaf" (text term));
       Printf.printf
         "%d rows; REPL ready with the table bound in %.0f ms\n%!"
         rows
         ((Unix.gettimeofday () -. start) *. 1000.);
       C.send c (T.text term ":view big");
       C.send c (T.key term K.Enter);
       let start = Unix.gettimeofday () in
       C.wait c ~within:300. (fun term ->
         Astring.String.is_infix ~affix:"rows \xc3\x97" (text term));
       Printf.printf
         "%d rows; viewer opened in %.0f ms\n%!"
         rows
         ((Unix.gettimeofday () -. start) *. 1000.);
       measure c term "PgDn" K.Page_down 40;
       measure c term "PgUp" K.Page_up 40;
       for _ = 1 to 10 do
         measure c term "Last row" (K.Char (Uchar.of_char 'G')) 1;
         measure c term "First row" (K.Char (Uchar.of_char 'g')) 1
       done)
;;
