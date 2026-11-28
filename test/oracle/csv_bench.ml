let time label f =
  let start = Unix.gettimeofday () in
  let result = f () in
  Printf.printf "  %-26s %7.3f s\n%!" label (Unix.gettimeofday () -. start);
  result
;;

let () =
  let path = Sys.argv.(1) in
  let fields =
    time "tokenize only" (fun () ->
      In_channel.with_open_bin path (fun channel ->
        let s = Talon.Scan.make ~sep:',' ~quote:'"' channel in
        let total = ref 0 in
        Seq.iter
          (fun b ->
             for r = 0 to Talon.Scan.rows b - 1 do
               total := !total + Talon.Scan.fields b r
             done)
          (Seq.of_dispenser (fun () -> Talon.Scan.next s));
        !total))
  in
  Printf.printf "  %d fields\n" fields;
  ignore
    (time "full inference" (fun () ->
       (Rupam_native.Data_file.read ~missing:Input.default_missing path).columns));
  let describe () =
    Duckdb_oracle.query
      ("DESCRIBE SELECT * FROM read_csv('" ^ path ^ "', sample_size = -1)")
  in
  ignore (time "DuckDB inference" describe);
  ignore (Duckdb_oracle.query "SET memory_limit = '8GB'");
  ignore
    (time "DuckDB parallel load, 8 GB" (fun () ->
       Duckdb_oracle.query
         ("CREATE OR REPLACE TEMP TABLE loaded AS SELECT * FROM read_csv('" ^ path ^ "')")));
  ignore (Duckdb_oracle.query "DROP TABLE loaded")
;;
