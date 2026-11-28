let fields ?chunk path =
  In_channel.with_open_bin path (fun channel ->
    let s = Talon.Scan.make ~batch:4096 ?chunk ~sep:',' ~quote:'"' channel in
    let rows = ref [] in
    while Talon.Scan.next s do
      for r = 0 to Talon.Scan.rows s - 1 do
        rows := Array.init (Talon.Scan.fields s r) (Talon.Scan.text s r) :: !rows
      done
    done;
    Array.of_list (List.rev !rows))
;;

let () =
  let dir = Sys.argv.(1) in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".csv")
    |> List.sort String.compare
  in
  let failures = ref 0 in
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let tiny = fields ~chunk:7 path
       and ours = fields path in
       let _, theirs =
         Duckdb_oracle.query
           ("SELECT * FROM read_csv('"
            ^ path
            ^ "', all_varchar = true, header = false, delim = ',', quote = '\"', escape \
               = '\"')")
       in
       let text = Option.value ~default:"" in
       let agree =
         tiny = ours
         && Array.length ours = Array.length theirs
         && Array.for_all2
              (fun a b ->
                 Array.length a = Array.length b
                 && Array.for_all2 (fun x y -> String.equal x (text y)) a b)
              ours
              theirs
       in
       if not agree
       then (
         incr failures;
         Printf.printf
           "%s: %d rows ours, %d DuckDB\n"
           f
           (Array.length ours)
           (Array.length theirs)))
    files;
  if !failures > 0
  then exit 1
  else Printf.printf "csv differential: %d files agree with DuckDB\n" (List.length files)
;;
