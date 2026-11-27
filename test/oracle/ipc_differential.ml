module A = Rupam_native.Arrow_result
module O = Nanoarrow_oracle

let failures = ref 0

let report file fmt =
  Printf.ksprintf
    (fun message ->
       incr failures;
       if !failures <= 50 then Printf.printf "%s: %s\n" file message)
    fmt
;;

let same_value (ours : A.value) (theirs : O.value) =
  match ours, theirs with
  | Missing, Missing | Unsupported, Unsupported -> true
  | Bool a, Bool b -> a = b
  | Int a, Int b -> Int64.equal a b
  | Float a, Float b -> Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)
  | Text a, Text b -> String.equal a b
  | _ -> false
;;

let show_ours : A.value -> string = function
  | Missing -> "Missing"
  | Unsupported -> "Unsupported"
  | Bool b -> string_of_bool b
  | Int n -> Int64.to_string n
  | Float x -> Printf.sprintf "%h" x
  | Text s -> String.escaped s
  | Date d -> string_of_int d
;;

let outcome f =
  match f () with
  | v -> Ok v
  | exception Failure m -> Error m
;;

let compare_file file =
  let ours = A.open_ipc file
  and theirs = O.open_ipc file in
  let rows = A.rows ours in
  if rows <> O.rows theirs then report file "rows %d vs %d" rows (O.rows theirs);
  let names = A.names ours in
  if names <> O.names theirs then report file "names differ";
  let width = Array.length names in
  for column = 0 to width - 1 do
    for row = 0 to rows - 1 do
      let a = A.value ours row column
      and b = O.value theirs row column in
      if not (same_value a b)
      then report file "value %s row %d: %s" names.(column) row (show_ours a)
    done
  done;
  let all = Array.init width Fun.id in
  let windows =
    [ 0, rows; 0, 1; rows - 1, 5; rows / 3, 97; rows / 2, rows; -4, 10; rows + 3, 4 ]
  in
  List.iter
    (fun (offset, limit) ->
       let a = A.cells ours ~columns:all ~offset ~limit
       and b = O.cells theirs offset limit all in
       if a <> b then report file "cells offset %d limit %d" offset limit)
    windows;
  for column = 0 to width - 1 do
    let a = outcome (fun () -> A.histogram ours ~column ~bins:20)
    and b = outcome (fun () -> O.histogram theirs column 20) in
    (match a, b with
     | Ok h, Ok (low, high, counts, missing) ->
       if
         not
           (Int64.equal (Int64.bits_of_float h.low) (Int64.bits_of_float low)
            && Int64.equal (Int64.bits_of_float h.high) (Int64.bits_of_float high)
            && h.counts = counts
            && h.missing = missing)
       then report file "histogram %s" names.(column)
     | Error x, Error y when String.equal x y -> ()
     | Error _, Ok _ when rows = 0 -> ()
     | _ -> report file "histogram outcome %s" names.(column));
    match a, b with
    | Ok _, Ok (_, _, _, missing) ->
      let size = rows - missing in
      let a = A.summary ours ~column
      and values = O.numbers theirs column size in
      let sorted = Array.copy values in
      Array.sort Float.compare sorted;
      let median =
        if size = 0
        then None
        else (
          let k = float_of_int (size - 1) *. 0.5 in
          let i = int_of_float k in
          let w = k -. float_of_int i in
          Some (((1. -. w) *. sorted.(i)) +. (w *. sorted.(min (i + 1) (size - 1)))))
      in
      if not (Option.equal Float.equal a.median median)
      then report file "median %s" names.(column);
      let quantile p =
        if size = 0
        then None
        else (
          let k = float_of_int (size - 1) *. p in
          let i = int_of_float k in
          let w = k -. float_of_int i in
          Some (((1. -. w) *. sorted.(i)) +. (w *. sorted.(min (i + 1) (size - 1)))))
      in
      let bits = Option.map Int64.bits_of_float in
      if
        not
          (Option.equal Float.equal a.q1 (quantile 0.25)
           && Option.equal Float.equal a.q3 (quantile 0.75))
      then report file "quartiles %s" names.(column);
      let distinct =
        let seen = Hashtbl.create 64 in
        Array.iter
          (fun x ->
             Hashtbl.replace seen (Int64.bits_of_float (if x = 0. then 0. else x)) ())
          values;
        Hashtbl.length seen
      in
      if a.distinct <> distinct then report file "distinct %s" names.(column);
      let mean, sd, skewness =
        if size = 0
        then None, None, None
        else (
          let low = a.histogram.low
          and high = a.histogram.high in
          let midpoint = (high /. 2.) +. (low /. 2.) in
          let scale = max (high -. midpoint) (midpoint -. low) in
          let observations = float_of_int size in
          if scale = 0.
          then Some midpoint, (if size > 1 then Some 0. else None), None
          else (
            let scaled_sum f = Array.fold_left (fun sum v -> sum +. f v) 0. values in
            let scaled x = (x -. midpoint) /. scale in
            let center = scaled_sum scaled /. observations in
            let deviation x = scaled x -. center in
            let second = scaled_sum (fun x -> deviation x *. deviation x) in
            let sd = scale *. sqrt (second /. (observations -. 1.)) in
            let skewness =
              if size < 3 || second = 0.
              then None
              else (
                let third =
                  scaled_sum (fun x ->
                    let d = deviation x in
                    d *. d *. d)
                in
                let variance = second /. observations in
                let correction =
                  sqrt (observations *. (observations -. 1.)) /. (observations -. 2.)
                in
                Some (correction *. (third /. observations) /. (variance *. sqrt variance)))
            in
            ( Some (midpoint +. (scale *. center))
            , (if size > 1 && Float.is_finite sd then Some sd else None)
            , skewness )))
      in
      if
        bits a.mean <> bits mean
        || bits a.sd <> bits sd
        || bits a.skewness <> bits skewness
      then report file "moments %s" names.(column)
    | _ -> ()
  done;
  for column = 0 to width - 1 do
    let reference = Hashtbl.create 64
    and missing = ref 0 in
    Array.iter
      (function
        | None -> incr missing
        | Some s ->
          Hashtbl.replace
            reference
            s
            (1 + Option.value (Hashtbl.find_opt reference s) ~default:0))
      (O.cells theirs 0 rows [| column |]).(0);
    let f = A.frequencies ours ~column in
    let top =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) reference []
      |> List.sort (fun (a, n) (b, m) ->
        match Int.compare m n with
        | 0 -> String.compare a b
        | order -> order)
      |> List.filteri (fun i _ -> i < 3)
    in
    if f.missing <> !missing || f.distinct <> Hashtbl.length reference || f.top <> top
    then report file "frequencies %s" names.(column)
  done;
  A.close ours;
  O.close theirs
;;

let compare_expected file expected =
  let wanted =
    match Yojson.Safe.from_file expected with
    | `List values ->
      Array.of_list
        (List.map
           (function
             | `String s -> Some s
             | `Null -> None
             | _ -> failwith "expected strings or null")
           values)
    | _ -> failwith "expected a JSON list"
  in
  let ours = A.open_ipc file in
  let rows = A.rows ours in
  if rows <> Array.length wanted
  then report file "rows %d vs %d" rows (Array.length wanted);
  let got = (A.cells ours ~columns:[| 0 |] ~offset:0 ~limit:rows).(0) in
  if got <> wanted then report file "cells differ from the expected values";
  Array.iteri
    (fun row want ->
       match A.value ours row 0, want with
       | Text a, Some b when String.equal a b -> ()
       | Missing, None -> ()
       | v, _ -> report file "value row %d: %s" row (show_ours v))
    wanted;
  let f = A.frequencies ours ~column:0 in
  let missing = Array.fold_left (fun n v -> if v = None then n + 1 else n) 0 wanted in
  if f.missing <> missing
  then report file "frequencies missing %d vs %d" f.missing missing;
  A.close ours
;;

let () =
  let dir = Sys.argv.(1) in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".arrow")
    |> List.sort String.compare
  in
  if files = [] then failwith "no IPC fixtures";
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let expected = Filename.remove_extension path ^ ".json" in
       try
         if Sys.file_exists expected
         then compare_expected path expected
         else compare_file path
       with
       | e -> report path "raised %s" (Printexc.to_string e))
    files;
  if !failures > 0
  then (
    Printf.printf "ipc differential: %d disagreements\n" !failures;
    exit 1)
  else Printf.printf "ipc differential: %d files agree\n" (List.length files)
;;
