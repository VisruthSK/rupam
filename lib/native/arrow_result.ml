type t

external open_ipc : string -> t = "rupam_arrow_open"
external rows : t -> int = "rupam_arrow_rows"
external names : t -> string array = "rupam_arrow_names"

type value =
  | Missing
  | Unsupported
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Date of int

external value : t -> int -> int -> value = "rupam_arrow_value"

external cells_
  :  t
  -> int
  -> int
  -> int array
  -> string option array array
  = "rupam_arrow_cells"

let cells t ~columns ~offset ~limit = cells_ t offset limit columns

type histogram =
  { low : float
  ; high : float
  ; counts : int array
  ; missing : int
  }

external histogram_
  :  t
  -> int
  -> int
  -> float * float * int array * int
  = "rupam_arrow_histogram"

let histogram t ~column ~bins =
  let low, high, counts, missing = histogram_ t column bins in
  { low; high; counts; missing }
;;

type summary =
  { histogram : histogram
  ; q1 : float option
  ; median : float option
  ; q3 : float option
  ; mean : float option
  ; sd : float option
  ; skewness : float option
  ; distinct : int
  }

external numbers : t -> int -> int -> float array = "rupam_arrow_numbers"
external distinct_floats : float array -> int = "rupam_distinct_floats"

let swap (a : float array) i j =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x
;;

let rec select (a : float array) lo hi k =
  if lo < hi
  then (
    let mid = lo + ((hi - lo) / 2) in
    if a.(mid) < a.(lo) then swap a mid lo;
    if a.(hi) < a.(lo) then swap a hi lo;
    if a.(hi) < a.(mid) then swap a hi mid;
    let pivot = a.(mid) in
    let i = ref lo
    and j = ref hi in
    while !i <= !j do
      while a.(!i) < pivot do
        incr i
      done;
      while a.(!j) > pivot do
        decr j
      done;
      if !i <= !j
      then (
        swap a !i !j;
        incr i;
        decr j)
    done;
    if k <= !j then select a lo !j k else if k >= !i then select a !i hi k)
;;

let quantile (values : float array) p =
  let n = Array.length values in
  if n = 0
  then None
  else (
    let k = float_of_int (n - 1) *. p in
    let i = int_of_float k in
    let weight = k -. float_of_int i in
    select values 0 (n - 1) i;
    let next = ref values.(min (i + 1) (n - 1)) in
    for j = i + 2 to n - 1 do
      if values.(j) < !next then next := values.(j)
    done;
    Some (((1. -. weight) *. values.(i)) +. (weight *. !next)))
;;

let summary t ~column =
  let histogram = histogram t ~column ~bins:20 in
  let n = rows t - histogram.missing in
  let values = numbers t column n in
  let distinct = ref (distinct_floats values) in
  let quantile = quantile values in
  let scaled_sum f = Array.fold_left (fun sum value -> sum +. f value) 0. values in
  let mean, sd, skewness =
    if n = 0
    then None, None, None
    else (
      let midpoint = (histogram.high /. 2.) +. (histogram.low /. 2.) in
      let scale = max (histogram.high -. midpoint) (midpoint -. histogram.low) in
      let observations = float_of_int n in
      if scale = 0.
      then Some midpoint, (if n > 1 then Some 0. else None), None
      else (
        let scaled x = (x -. midpoint) /. scale in
        let center = scaled_sum scaled /. observations in
        let mean = midpoint +. (scale *. center) in
        let deviation x = scaled x -. center in
        let second =
          scaled_sum (fun x ->
            let d = deviation x in
            d *. d)
        in
        let sd = scale *. sqrt (second /. (observations -. 1.)) in
        let skewness =
          if n < 3 || second = 0.
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
        Some mean, (if n > 1 && Float.is_finite sd then Some sd else None), skewness))
  in
  { histogram
  ; q1 = quantile 0.25
  ; median = quantile 0.5
  ; q3 = quantile 0.75
  ; mean
  ; sd
  ; skewness
  ; distinct = !distinct
  }
;;

type frequencies =
  { missing : int
  ; distinct : int
  ; top : (string * int) list
  }

type profile =
  | Numeric of summary
  | Frequencies of frequencies

let frequencies t ~column =
  let counts = Hashtbl.create 32 in
  let missing = ref 0 in
  let total = rows t in
  let rec scan offset =
    if offset < total
    then (
      let limit = min (total - offset) 4096 in
      Array.iter
        (function
          | None -> incr missing
          | Some value ->
            let n = Option.value (Hashtbl.find_opt counts value) ~default:0 in
            Hashtbl.replace counts value (n + 1))
        (cells t ~columns:[| column |] ~offset ~limit).(0);
      scan (offset + limit))
  in
  scan 0;
  let compare (left, n) (right, m) =
    match Int.compare m n with
    | 0 -> String.compare left right
    | order -> order
  in
  let top =
    Hashtbl.fold
      (fun value count top ->
         List.filteri (fun i _ -> i < 3) (List.merge compare [ value, count ] top))
      counts
      []
  in
  { missing = !missing; distinct = Hashtbl.length counts; top }
;;

external close : t -> unit = "rupam_arrow_close"
