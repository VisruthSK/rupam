module T = Matrix_test.Terminal
module S = Matrix_test.Screen
module C = Matrix_test.Child
module K = Matrix.Input.Keystroke

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
;;

let rupam = absolute (Sys.getenv "RUPAM_TEST_BIN")
let dev = absolute (Sys.getenv "RUPAM_DEV_TEST_BIN")
let examples = absolute (Sys.getenv "RUPAM_EXAMPLES")
let selected = Matrix.Ansi.Color.of_rgb 79 62 48
let contains = Astring.String.is_infix

let fresh files =
  let dir = Filename.temp_dir "rupam-terminal" "" in
  List.iter
    (fun name ->
       let src = Filename.concat examples name in
       let data = In_channel.with_open_bin src In_channel.input_all in
       Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
         output_string oc data))
    files;
  dir
;;

let lines term = String.split_on_char '\n' (S.text (T.screen term))
let shows text term = List.exists (contains ~affix:text) (lines term)
let screen = ref None

let until c ready =
  match !screen with
  | Some term when ready term -> ()
  | _ -> C.wait c ~within:20. ready
;;

let poll c ready =
  let deadline = Unix.gettimeofday () +. 20. in
  let rec go () =
    if ready ()
    then ()
    else if Unix.gettimeofday () > deadline
    then failwith "condition not met within 20s"
    else (
      (try C.wait c ~within:0.1 (fun _ -> ready ()) with
       | Failure _ -> ());
      go ())
  in
  go ()
;;

let see c text = until c (shows text)
let gone c text = until c (fun term -> not (shows text term))

let highlighted text term =
  let screen = T.screen term in
  List.exists
    (fun (row, col) ->
       let _, style = S.cell screen ~row ~col in
       style.Matrix.Ansi.Style.bg = Some selected)
    (S.find screen text)
;;

let key term ?(mods = K.no_mods) k = T.key term ~mods k
let char c = K.Char (Uchar.of_char c)
let type_ c term s = C.send c (T.text term s)
let paste c term s = C.send c (T.paste term s)
let press c term ?mods k = C.send c (key term ?mods k)

let submit c term s =
  type_ c term s;
  press c term K.Enter
;;

let click c term text =
  match S.find (T.screen term) text with
  | (row, col) :: _ ->
    C.send
      c
      (Printf.sprintf
         "\027[<0;%d;%dM\027[<0;%d;%dm"
         (col + 1)
         (row + 1)
         (col + 1)
         (row + 1))
  | [] -> failwith ("no " ^ text ^ " on screen")
;;

let wheel_down c term text =
  match S.find (T.screen term) text with
  | (row, col) :: _ -> C.send c (Printf.sprintf "\027[<65;%d;%dM" (col + 1) (row + 1))
  | [] -> failwith ("no " ^ text ^ " on screen")
;;

let session ?(env = []) ~dir args f =
  let term = T.create ~cols:100 ~rows:30 () in
  let env =
    [ "PATH", Sys.getenv "PATH"
    ; "HOME", dir
    ; "TERM", "xterm-256color"
    ; "COLORTERM", "truecolor"
    ; "XDG_CONFIG_HOME", Filename.concat dir "config"
    ; "XDG_STATE_HOME", Filename.concat dir "state"
    ]
    |> List.filter (fun (key, _) -> not (List.mem_assoc key env))
    |> fun base -> base @ env
  in
  screen := Some term;
  C.with_spawn term ~env ~cwd:dir ~prog:rupam ~args (fun c ->
    see c "rupam ❯";
    f c term)
;;

let run_in dir command =
  let out = Filename.concat dir "out.txt" in
  let code =
    Sys.command
      (Printf.sprintf
         "cd %s && { %s; } > %s 2>&1"
         (Filename.quote dir)
         command
         (Filename.quote out))
  in
  code, In_channel.with_open_text out In_channel.input_all
;;

let check name ok = if not ok then failwith name

let count affix s =
  let rec go i n =
    match Astring.String.find_sub ~start:i ~sub:affix s with
    | Some j -> go (j + 1) (n + 1)
    | None -> n
  in
  go 0 0
;;

let completion () =
  let dir = fresh [] in
  session ~dir [ "--target"; "python-polars" ] (fun c term ->
    type_ c term ":h";
    press c term K.Tab;
    see c "commands and keys";
    press c term ~mods:{ K.no_mods with ctrl = true } (char 'u');
    gone c "commands and keys";
    type_ c term ":";
    press c term K.Tab;
    see c "first rows";
    type_ c term "t";
    until c (fun term ->
      shows "show or set the target" term
      && shows "inferred type" term
      && not (shows "first rows" term));
    press c term ~mods:{ K.no_mods with ctrl = true } (char 'u');
    type_ c term ":ty";
    press c term K.Tab;
    press c term K.Enter;
    press c term K.Enter;
    see c ":type EXPR  inferred type";
    check "waits for the argument" (not (shows "expects" term));
    type_ c term "1 + 41";
    press c term K.Enter;
    see c "Int";
    submit c term "40 + 2";
    see c "42";
    press c term ~mods:{ K.no_mods with ctrl = true } (char 'r');
    type_ c term "41";
    see c "reverse search: 41";
    see c "1 + 41";
    press c term K.Enter;
    gone c "reverse search";
    press c term ~mods:{ K.no_mods with ctrl = true } (char 'u'))
;;

let grid_pipeline () =
  let dir = fresh [ "shop.rp"; "orders.parquet"; "data-dict.yaml" ] in
  session ~dir [ "repl"; "--target"; "python-polars"; "shop.rp" ] (fun c term ->
    submit c term ":view orders";
    see c "╭";
    press c term K.Right;
    until c (highlighted "North");
    type_ c term "SSS";
    see c "ordered by (region desc)";
    press c term K.Left;
    type_ c term "S";
    see c "ordered by (region desc, customer_id desc)";
    see c "South";
    click c term "South";
    until c (highlighted "South");
    type_ c term "d";
    see c "2 distinct";
    see c "n = 4";
    press c term K.Right;
    press c term K.Right;
    until c (highlighted "1.00");
    type_ c term "d";
    see c "Skewness";
    see c "Min 0.00   Q1 0.50";
    see c "Mean 1.00   SD 1.00";
    type_ c term "fj";
    until c (highlighted "0.00");
    type_ c term "k";
    until c (highlighted "1.00");
    type_ c term "=";
    see c "1 Skip";
    type_ c term "1";
    see c "1 row × 4 columns";
    press c term K.Enter;
    see c "name ❯";
    C.send c (String.concat "" (List.init 11 (fun _ -> key term K.Backspace)));
    paste c term "kept_orders";
    press c term K.Enter;
    gone c "╭";
    submit c term ":save saved.rp";
    see c "Saved saved.rp";
    submit c term ":quit");
  let saved =
    In_channel.with_open_text (Filename.concat dir "saved.rp") In_channel.input_all
  in
  let viewer = List.nth (Astring.String.cuts ~sep:"let kept_orders" saved) 1 in
  check "one arrange" (count "arrange(" viewer = 1);
  check "sort keys" (contains ~affix:"desc($region), desc($customer_id)" viewer);
  check "exact filter" (contains ~affix:"filter($cost == 1.0, missing = skip)" viewer);
  let code, out = run_in dir (Filename.quote rupam ^ " check saved.rp") in
  check ("check: " ^ out) (code = 0);
  let code, ir =
    run_in dir (Filename.quote dev ^ " transform-ir --binding kept_orders saved.rp")
  in
  check ("ir: " ^ ir) (code = 0 && count "(Arrange" ir = 1 && count "(Filter" ir = 1);
  List.iter
    (fun target ->
       let code, out =
         run_in
           dir
           (Printf.sprintf
              "printf 'kept_orders\\n:quit\\n' | %s repl --target %s saved.rp"
              (Filename.quote rupam)
              target)
       in
       check
         (target ^ ": " ^ out)
         (code = 0 && contains ~affix:"South" out && contains ~affix:"1 row" out))
    [ "python-polars"; "r-dplyr" ]
;;

let multiline_draft () =
  let dir = fresh [ "viewer-penguins.rp"; "penguins.csv" ] in
  session
    ~dir
    [ "repl"; "--target"; "python-polars"; "viewer-penguins.rp" ]
    (fun c term ->
       submit c term ":view penguins";
       see c "344 rows";
       see c "39.10";
       wheel_down c term "39.10";
       type_ c term "i";
       press c term ~mods:{ K.no_mods with shift = true } K.Enter;
       press c term ~mods:{ K.no_mods with ctrl = true; shift = true } (char 'm');
       check "grid kept" (shows "344 rows" term);
       paste c term "filter(body_mass > 4000, missing = skip)";
       until c (fun term ->
         List.exists
           (fun l ->
              contains ~affix:"rows × 8 columns" l && not (contains ~affix:"344 rows" l))
           (lines term));
       paste c term "\n    |> write_parquet(\"unapproved.parquet\")";
       see c "not-a-table";
       check "no write" (not (Sys.file_exists (Filename.concat dir "unapproved.parquet")));
       press c term K.Escape;
       press c term K.Escape;
       gone c "╭";
       submit c term ":bindings";
       see c "penguins :";
       check "no implicit binding" (not (shows "view_result" term)))
;;

let exact_cell () =
  let dir = fresh [] in
  Out_channel.with_open_text (Filename.concat dir "rupam.toml") (fun oc ->
    output_string oc "[preview]\ndigits = 3\n");
  Out_channel.with_open_text (Filename.concat dir "exact.csv") (fun oc ->
    output_string oc "measurement\n0.30000000000000004\n0.3\n");
  session ~dir [ "--target"; "python-polars" ] (fun c term ->
    submit c term "let exact = read_csv(\"exact.csv\")";
    submit c term ":view exact";
    until c (highlighted "0.300");
    type_ c term "d";
    see c "Min 0.300   Q1 0.300";
    type_ c term "=";
    see c "1 row × 1 column";
    see c "$measurement == 0.30000000000000004";
    press c term K.Enter;
    see c "name ❯";
    submit c term "exact_row";
    see c "let exact_row")
;;

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
;;

let cancellation () =
  let dir = fresh [ "viewer-penguins.rp"; "penguins.csv" ] in
  let slow = Filename.concat dir "slow"
  and started = Filename.concat dir "started" in
  let python = Filename.concat dir "python" in
  let real = String.trim (snd (run_in dir "command -v python3 || command -v python")) in
  Out_channel.with_open_text python (fun oc ->
    output_string
      oc
      "#!/bin/sh\n\
       if [ \"$1\" = -u ] && [ -f \"$TEST_SLOW\" ]; then\n\
      \  printf \"%s\\n\" \"$$\" > \"$TEST_STARTED\"\n\
      \  exec \"$TEST_PYTHON\" -c \"import time; time.sleep(60)\"\n\
       fi\n\
       exec \"$TEST_PYTHON\" \"$@\"\n");
  Unix.chmod python 0o755;
  let touch path = Out_channel.with_open_text path ignore in
  let started_pid () =
    let pid = ref None in
    let ready () =
      match In_channel.with_open_text started In_channel.input_all with
      | text when String.trim text <> "" ->
        pid := Some (int_of_string (String.trim text));
        true
      | _ -> false
      | exception Sys_error _ -> false
    in
    pid, ready
  in
  let env =
    [ "PATH", dir ^ ":" ^ Sys.getenv "PATH"
    ; "TEST_SLOW", slow
    ; "TEST_STARTED", started
    ; "TEST_PYTHON", real
    ]
  in
  session
    ~env
    ~dir
    [ "repl"; "--target"; "python-polars"; "viewer-penguins.rp" ]
    (fun c term ->
       submit c term ":view penguins";
       see c "╭";
       touch slow;
       type_ c term "s";
       let pid, ready = started_pid () in
       poll c ready;
       let first = Option.get !pid in
       Sys.remove slow;
       type_ c term "S";
       see c "ordered by (species desc)";
       check "superseded worker killed" (not (alive first));
       type_ c term "q";
       gone c "╭";
       submit c term ":view penguins";
       see c "╭";
       Sys.remove started;
       touch slow;
       type_ c term "s";
       let pid, ready = started_pid () in
       poll c ready;
       let second = Option.get !pid in
       press c term K.Escape;
       gone c "╭";
       check "cancelled worker killed" (not (alive second));
       Sys.remove slow;
       submit c term ":preview penguins";
       see c "344 rows")
;;

let paste_formats () =
  let dir = fresh [ "viewer-penguins.rp"; "penguins.csv" ] in
  session
    ~dir
    [ "repl"; "--target"; "python-polars"; "viewer-penguins.rp" ]
    (fun c term ->
       paste
         c
         term
         "penguins |> arrange($bill_len, desc($bill_dep)) |> filter(some(bill_dep) and \
          bill_dep > 17.9)";
       until c (fun term ->
         List.exists
           (fun line ->
              contains ~affix:"|> filter(" line && not (contains ~affix:"penguins" line))
           (lines term)))
;;

let target_menu () =
  let dir = fresh [ "viewer-penguins.rp"; "penguins.csv" ] in
  session
    ~dir
    [ "repl"; "--target"; "python-polars"; "viewer-penguins.rp" ]
    (fun c term ->
       type_ c term ":target ";
       press c term K.Tab;
       see c "python-polars";
       check
         "no tables offered"
         (not (List.exists (contains ~affix:"  penguins") (lines term)));
       check "no kind label" (not (shows "target" term && shows "r-dplyr  target" term));
       press c term K.Down;
       see c ":target r-dplyr";
       press c term K.Enter;
       check "accepting does not run" (not (shows "r-dplyr · :help" term));
       press c term K.Enter;
       see c "r-dplyr · :help")
;;

let tests =
  [ ":target completes, chooses and runs like typed input", target_menu
  ; "paste formats a complete pipeline", paste_formats
  ; "escape cancels a preview without killing the REPL worker", cancellation
  ; "completion menu filters while typing", completion
  ; "grid pipeline and saved source", grid_pipeline
  ; "multiline draft keeps the grid and defers writes", multiline_draft
  ; "value filter uses the exact typed cell", exact_cell
  ]
;;

let () =
  let failed =
    List.filter
      (fun (name, test) ->
         match test () with
         | () ->
           Printf.printf "ok    %s\n%!" name;
           false
         | exception exn ->
           Printf.printf "FAIL  %s\n%s\n%!" name (Printexc.to_string exn);
           true)
      tests
  in
  if failed <> [] then exit 1
;;
