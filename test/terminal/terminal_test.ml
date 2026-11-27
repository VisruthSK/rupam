module T = Matrix_test.Terminal
module S = Matrix_test.Screen
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
let running = Atomic.make 0

let process_perform job =
  Atomic.incr running;
  ignore
    (Thread.create
       (fun () -> Fun.protect ~finally:(fun () -> Atomic.decr running) job)
       ())
;;

let quiesce () =
  let deadline = Unix.gettimeofday () +. 0.05 in
  while Atomic.get running > 0 && Unix.gettimeofday () < deadline do
    Thread.delay 0.002
  done
;;

let tick = 50_000_000

let until c ready =
  let term = Option.get !screen in
  let deadline = Unix.gettimeofday () +. 30. in
  let rec go () =
    if ready term
    then ()
    else if Unix.gettimeofday () > deadline
    then failwith ("not ready within 30s; the screen:\n" ^ S.text (T.screen term))
    else (
      Matrix_test.advance c tick;
      go ())
  in
  go ()
;;

let poll c ready = until c (fun _ -> ready ())
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

let send c bytes = Matrix_test.input c bytes
let key term ?(mods = K.no_mods) k = T.key term ~mods k
let char c = K.Char (Uchar.of_char c)
let type_ c term s = send c (T.text term s)
let paste c term s = send c (T.paste term s)
let press c term ?mods k = send c (key term ?mods k)

let submit c term s =
  type_ c term s;
  press c term K.Enter
;;

let click c term text =
  match S.find (T.screen term) text with
  | (row, col) :: _ ->
    send
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
  | (row, col) :: _ -> send c (Printf.sprintf "\027[<65;%d;%dM" (col + 1) (row + 1))
  | [] -> failwith ("no " ^ text ^ " on screen")
;;

let project dir args =
  let project = Rupam_native.Project.load dir in
  let rec target = function
    | "--target" :: name :: _ -> List.assoc_opt name Rupam.Target.targets
    | _ :: rest -> target rest
    | [] -> None
  in
  match target args with
  | None -> project
  | Some chosen ->
    { project with target = { value = Some chosen; origin = Session "--target" } }
;;

let session ?(env = []) ~dir args f =
  let path = List.find_opt (fun a -> Filename.check_suffix a ".rp") args in
  let base =
    [ "HOME", dir
    ; "USERPROFILE", dir
    ; "LOCALAPPDATA", Filename.concat dir "state"
    ; "XDG_CONFIG_HOME", Filename.concat dir "config"
    ; "XDG_STATE_HOME", Filename.concat dir "state"
    ]
  in
  let previous = Sys.getcwd () in
  let saved = List.map (fun (k, _) -> k, Sys.getenv_opt k) (base @ env) in
  List.iter (fun (k, v) -> Unix.putenv k v) (base @ env);
  Sys.chdir dir;
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir previous;
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)
    (fun () ->
       Rupam_console.Console.with_capture
       @@ fun capture ->
       let app =
         Option.get
           (Rupam_console.Console.app ~capture ~project:(project dir args) ?path ())
       in
       let term = T.create ~cols:100 ~rows:30 () in
       screen := Some term;
       Mosaic_test.run
         ~config:Rupam_console.Console.config
         ~process_perform
         ~quiesce
         term
         app
         (fun c ->
            see c "rupam ❯";
            f c term))
;;

let run_in ?(input = "") dir prog args =
  let file name = Filename.concat dir name in
  Out_channel.with_open_bin (file "in.txt") (fun oc -> output_string oc input);
  let stdin = Unix.openfile (file "in.txt") [ O_RDONLY ] 0
  and stdout = Unix.openfile (file "out.txt") [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  let previous = Sys.getcwd () in
  Sys.chdir dir;
  let pid =
    Fun.protect
      ~finally:(fun () -> Sys.chdir previous)
      (fun () ->
         Unix.create_process prog (Array.of_list (prog :: args)) stdin stdout stdout)
  in
  let _, status = Unix.waitpid [] pid in
  Unix.close stdin;
  Unix.close stdout;
  let code =
    match status with
    | WEXITED n -> n
    | WSIGNALED _ | WSTOPPED _ -> 255
  in
  code, In_channel.with_open_bin (file "out.txt") In_channel.input_all
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
    send c (String.concat "" (List.init 11 (fun _ -> key term K.Backspace)));
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
  let code, out = run_in dir rupam [ "check"; "saved.rp" ] in
  check ("check: " ^ out) (code = 0);
  let code, ir =
    run_in dir dev [ "transform-ir"; "--binding"; "kept_orders"; "saved.rp" ]
  in
  check ("ir: " ^ ir) (code = 0 && count "(Arrange" ir = 1 && count "(Filter" ir = 1);
  List.iter
    (fun target ->
       let code, out =
         run_in
           ~input:"kept_orders\n:quit\n"
           dir
           rupam
           [ "repl"; "--target"; target; "saved.rp" ]
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
    gone c "name ❯";
    submit c term ":bindings";
    see c "exact_row :")
;;

let slow_worker =
  {|import os, sys, time
if "-u" in sys.orig_argv and os.path.exists(os.environ.get("TEST_SLOW", "")):
    with open(os.environ["TEST_STARTED"], "w") as f:
        f.write(str(os.getpid()))
    for beat in range(1200):
        with open(os.environ["TEST_HEARTBEAT"], "w") as f:
            f.write(str(beat))
        time.sleep(0.05)
    os._exit(0)
|}
;;

let cancellation () =
  let dir = fresh [ "viewer-penguins.rp"; "penguins.csv" ] in
  let slow = Filename.concat dir "slow"
  and started = Filename.concat dir "started"
  and heartbeat = Filename.concat dir "heartbeat"
  and site = Filename.concat dir "site" in
  Sys.mkdir site 0o755;
  Out_channel.with_open_bin (Filename.concat site "sitecustomize.py") (fun oc ->
    output_string oc slow_worker);
  let beat () =
    try In_channel.with_open_bin heartbeat In_channel.input_all with
    | Sys_error _ -> ""
  in
  let alive _ =
    let before = beat () in
    Unix.sleepf 0.5;
    beat () <> before
  in
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
    [ "PYTHONPATH", site
    ; "TEST_SLOW", slow
    ; "TEST_STARTED", started
    ; "TEST_HEARTBEAT", heartbeat
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
      (match List.tl (Array.to_list Sys.argv) with
       | [] -> tests
       | chosen ->
         List.filter
           (fun (name, _) -> List.exists (fun c -> contains ~affix:c name) chosen)
           tests)
  in
  if failed <> [] then exit 1
;;
