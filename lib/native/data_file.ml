let digest path =
  let workers = min 7 (Domain.recommended_domain_count () - 1) in
  if
    workers < 1
    || In_channel.with_open_bin path In_channel.length < Int64.of_int (8 lsl 20)
  then Blake3.file path
  else (
    let pool = Domainslib.Task.setup_pool ~num_domains:workers () in
    Fun.protect
      ~finally:(fun () -> Domainslib.Task.teardown_pool pool)
      (fun () ->
         Domainslib.Task.run pool (fun () ->
           Blake3.file
             ~parallel:(fun count body ->
               if count > 0
               then Domainslib.Task.parallel_for pool ~start:0 ~finish:(count - 1) ~body)
             path)))
;;

let columns ~missing path : Input.column list =
  if not (Sys.file_exists path)
  then Diagnostic.fail Unreadable_input ("No such data file " ^ path);
  match Input.format path with
  | Parquet -> Parquet_source.columns path
  | (Csv | Tsv) as format -> Delimited.columns ~missing ~path format
;;
