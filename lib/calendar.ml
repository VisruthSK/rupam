let of_ymd date =
  Option.map
    (fun t -> fst (Ptime.Span.to_d_ps (Ptime.to_span t)))
    (Ptime.of_date date)
;;

let ymd days =
  Ptime.to_date (Option.get (Ptime.of_span (Ptime.Span.of_d_ps (days, 0L) |> Option.get)))
;;

let to_iso days =
  let y, m, d = ymd days in
  Printf.sprintf "%04d-%02d-%02d" y m d
;;

let parse ~format text =
  let digits s = String.for_all (fun c -> c >= '0' && c <= '9') s && s <> "" in
  let rec go f t (y, m, d) =
    match f, t with
    | [], [] -> Option.bind y (fun y -> Option.bind m (fun m -> Option.bind d (fun d -> of_ymd (y, m, d))))
    | '%' :: 'Y' :: f, t -> field 4 f t (fun v -> Some v, m, d)
    | '%' :: 'm' :: f, t -> field 2 f t (fun v -> y, Some v, d)
    | '%' :: 'd' :: f, t -> field 2 f t (fun v -> y, m, Some v)
    | c :: f, c' :: t when c = c' -> go f t (y, m, d)
    | _ -> None
  and field width f t set =
    let taken = List.filteri (fun i _ -> i < width) t in
    let text = String.of_seq (List.to_seq taken) in
    if List.length taken = width && digits text
    then go f (List.filteri (fun i _ -> i >= width) t) (set (int_of_string text))
    else None
  in
  go (List.of_seq (String.to_seq format)) (List.of_seq (String.to_seq text)) (None, None, None)
;;

let of_iso = parse ~format:"%Y-%m-%d"

let add_months days n =
  let y, m, d = ymd days in
  let total = (y * 12) + (m - 1) + n in
  let y = Int.div total 12 - if total mod 12 < 0 then 1 else 0 in
  let m = ((total mod 12) + 12) mod 12 + 1 in
  let rec clamp d = match of_ymd (y, m, d) with Some v -> v | None -> clamp (d - 1) in
  clamp d
;;

let weekday days = ((days + 3) mod 7 + 7) mod 7 + 1

let year_day days =
  let y, _, _ = ymd days in
  days - Option.get (of_ymd (y, 1, 1)) + 1
;;
