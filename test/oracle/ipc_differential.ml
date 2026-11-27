module A = Rupam_native.Arrow_result

let failures = ref 0

let report file fmt =
  Printf.ksprintf
    (fun message ->
       incr failures;
       if !failures <= 50 then Printf.printf "%s: %s\n" file message)
    fmt
;;

let same (ours : A.value) (want : Yojson.Safe.t) =
  match ours, want with
  | Missing, `Null -> true
  | Bool a, `Bool b -> Bool.equal a b
  | Int a, `Int b -> Int64.equal a (Int64.of_int b)
  | Int a, `Intlit b -> String.equal (Int64.to_string a) b
  | Float a, `Float b -> Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)
  | Float a, `Int b -> Float.equal a (float_of_int b)
  | Float a, `String "nan" -> Float.is_nan a
  | Float a, `String "inf" -> a = Float.infinity
  | Float a, `String "-inf" -> a = Float.neg_infinity
  | Text a, `String b -> String.equal a b
  | _ -> false
;;

let text : Yojson.Safe.t -> string option = function
  | `Null -> None
  | `Bool b -> Some (if b then "true" else "false")
  | `Int n -> Some (string_of_int n)
  | `Intlit s -> Some s
  | `Float f -> Some (Printf.sprintf "%.17g" f)
  | `String s -> Some s
  | _ -> None
;;

let number : Yojson.Safe.t -> float option = function
  | `Int n -> Some (float_of_int n)
  | `Intlit s -> Some (Int64.to_float (Int64.of_string s))
  | `Float f -> Some f
  | `String "nan" -> Some Float.nan
  | `String "inf" -> Some Float.infinity
  | `String "-inf" -> Some Float.neg_infinity
  | _ -> None
;;

let numeric values = List.for_all (fun v -> v = `Null || number v <> None) values

let quantile sorted p =
  let n = Array.length sorted in
  if n = 0
  then None
  else (
    let k = float_of_int (n - 1) *. p in
    let i = int_of_float k in
    let w = k -. float_of_int i in
    Some (((1. -. w) *. sorted.(i)) +. (w *. sorted.(min (i + 1) (n - 1)))))
;;

let check_numeric file ours name column values =
  let present = Array.of_list (List.filter_map number values) in
  let missing = List.length values - Array.length present in
  let low, high =
    Array.fold_left
      (fun (lo, hi) x -> (if x < lo then x else lo), if x > hi then x else hi)
      (if Array.length present = 0 then 0., 0. else present.(0), present.(0))
      present
  in
  let h = A.histogram ours ~column ~bins:20 in
  if
    not
      (Float.equal h.low low
       && Float.equal h.high high
       && h.missing = missing
       && Array.fold_left ( + ) 0 h.counts = Array.length present)
  then report file "histogram %s" name;
  let s = A.summary ours ~column in
  let sorted = Array.copy present in
  Array.sort Float.compare sorted;
  if
    not
      (Option.equal Float.equal s.median (quantile sorted 0.5)
       && Option.equal Float.equal s.q1 (quantile sorted 0.25)
       && Option.equal Float.equal s.q3 (quantile sorted 0.75))
  then report file "quartiles %s" name;
  let distinct = Hashtbl.create 64 in
  Array.iter
    (fun x ->
       Hashtbl.replace distinct (Int64.bits_of_float (if x = 0. then 0. else x)) ())
    present;
  if s.distinct <> Hashtbl.length distinct then report file "distinct %s" name
;;

let check_frequencies file ours name column values =
  let counts = Hashtbl.create 64
  and missing = ref 0 in
  List.iter
    (fun v ->
       match text v with
       | None -> incr missing
       | Some s ->
         Hashtbl.replace counts s (1 + Option.value (Hashtbl.find_opt counts s) ~default:0))
    values;
  let top =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) counts []
    |> List.sort (fun (a, n) (b, m) ->
      match Int.compare m n with
      | 0 -> String.compare a b
      | order -> order)
    |> List.filteri (fun i _ -> i < 3)
  in
  let f = A.frequencies ours ~column in
  if f.missing <> !missing || f.distinct <> Hashtbl.length counts || f.top <> top
  then report file "frequencies %s" name
;;

let compare_file file expected =
  let columns =
    List.map
      (fun c ->
         ( Yojson.Safe.Util.(to_string (member "name" c))
         , Array.of_list Yojson.Safe.Util.(to_list (member "values" c)) ))
      (Yojson.Safe.Util.to_list (Yojson.Safe.from_file expected))
  in
  let ours = A.open_ipc file in
  let rows =
    match columns with
    | (_, v) :: _ -> Array.length v
    | [] -> 0
  in
  if A.rows ours <> rows then report file "rows %d vs %d" (A.rows ours) rows;
  if A.names ours <> Array.of_list (List.map fst columns) then report file "names differ";
  List.iteri
    (fun column (name, values) ->
       Array.iteri
         (fun row want ->
            if not (same (A.value ours row column) want)
            then report file "value %s row %d" name row)
         values)
    columns;
  let all = Array.of_list (List.mapi (fun i _ -> i) columns) in
  List.iter
    (fun (offset, limit) ->
       let got = A.cells ours ~columns:all ~offset ~limit in
       let lo = max 0 (min offset rows) in
       let hi = min rows (lo + max 0 limit) in
       let want =
         Array.of_list
           (List.map
              (fun (_, v) -> Array.init (hi - lo) (fun i -> text v.(lo + i)))
              columns)
       in
       if got <> want then report file "cells offset %d limit %d" offset limit)
    [ 0, rows; 0, 1; rows - 1, 5; rows / 3, 97; rows / 2, rows; -4, 10; rows + 3, 4 ];
  if rows > 0
  then
    List.iteri
      (fun column (name, values) ->
         let values = Array.to_list values in
         if numeric values then check_numeric file ours name column values;
         check_frequencies file ours name column values)
      columns;
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
       try compare_file path (Filename.remove_extension path ^ ".json") with
       | e -> report path "raised %s" (Printexc.to_string e))
    files;
  if !failures > 0
  then (
    Printf.printf "ipc differential: %d disagreements\n" !failures;
    exit 1)
  else Printf.printf "ipc differential: %d files agree with pyarrow\n" (List.length files)
;;
