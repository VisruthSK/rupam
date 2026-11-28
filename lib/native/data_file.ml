let digest path =
  let size = In_channel.with_open_bin path In_channel.length in
  Parallel.run ~size (fun parallel -> Blake3.file ~parallel:(Parallel.each parallel) path)
;;

let columns ~missing path : Input.column list =
  if not (Sys.file_exists path)
  then Diagnostic.fail Unreadable_input ("No such data file " ^ path);
  match Input.format path with
  | Parquet -> Parquet_source.columns path
  | (Csv | Tsv) as format -> Delimited.columns ~missing ~path format
;;
