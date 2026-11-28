module Source = Rupam_native.Parquet_source

let describe (c : Input.column) =
  match c.typ with
  | Supported t ->
    Printf.sprintf "%s %s%s" c.name (Types.display t) (if c.nullable then "?" else "")
  | Unsupported raw -> Printf.sprintf "%s unsupported(%s)" c.name raw
;;

let agree (a : Input.column) (b : Input.column) =
  String.equal a.name b.name
  &&
  match a.typ, b.typ with
  | Supported x, Supported y -> x = y && Bool.equal a.nullable b.nullable
  | Unsupported _, Unsupported _ -> true
  | _ -> false
;;

let same_cell (cell : Source.cell) (duck : string option) =
  match cell, duck with
  | Missing, None -> true
  | Bool b, Some s -> String.equal s (if b then "true" else "false")
  | Int n, Some s -> String.equal (Int64.to_string n) s
  | Float f, Some s ->
    (Float.is_nan f && String.equal s "nan")
    || Int64.equal (Int64.bits_of_float f) (Int64.bits_of_float (float_of_string s))
  | Text a, Some b -> String.equal a b
  | Date d, Some s -> Calendar.of_iso s = Some d
  | _ -> false
;;

let values_disagree path =
  let names, rows = Duckdb_oracle.rows path in
  List.filter_map
    (fun (name, cells) ->
       match Array.find_index (String.equal name) names with
       | None -> Some (name ^ " is not a DuckDB column")
       | Some _ when Array.length cells <> Array.length rows ->
         Some
           (Printf.sprintf
              "%s has %d values, DuckDB %d"
              name
              (Array.length cells)
              (Array.length rows))
       | Some j ->
         Seq.find
           (fun i -> not (same_cell cells.(i) rows.(i).(j)))
           (Seq.init (Array.length rows) Fun.id)
         |> Option.map (fun i -> Printf.sprintf "%s differs at row %d" name i))
    (Source.rows path)
;;

let () =
  let dir = Sys.argv.(1) in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".parquet")
    |> List.sort String.compare
  in
  let failures = ref 0 in
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let ours =
         (Rupam_native.Data_file.read ~missing:Input.default_missing path).columns
       and theirs = Duckdb_oracle.parquet_columns path in
       if List.length ours <> List.length theirs || not (List.for_all2 agree ours theirs)
       then (
         incr failures;
         Printf.printf
           "%s\n  ours:   %s\n  duckdb: %s\n"
           f
           (String.concat ", " (List.map describe ours))
           (String.concat ", " (List.map describe theirs)));
       match values_disagree path with
       | [] -> ()
       | problems ->
         incr failures;
         Printf.printf "%s values: %s\n" f (String.concat "; " problems))
    files;
  if !failures > 0
  then (
    Printf.printf
      "parquet differential: %d disagreements over %d files\n"
      !failures
      (List.length files);
    exit 1)
  else
    Printf.printf "parquet differential: %d files agree with DuckDB\n" (List.length files)
;;
