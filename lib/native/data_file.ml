let digest path =
  let size = In_channel.with_open_bin path In_channel.length in
  Parallel.run ~size (fun parallel -> Blake3.file ~parallel:(Parallel.each parallel) path)
;;

let read ~missing path : Input.snapshot =
  if not (Sys.file_exists path)
  then Diagnostic.fail Unreadable_input ("No such data file " ^ path);
  match Input.format path with
  | Parquet -> { digest = digest path; columns = Parquet_source.columns path }
  | (Csv | Tsv) as format ->
    let hasher = Blake3.hasher () in
    let columns = Delimited.columns ~on_read:(Blake3.feed hasher) ~missing ~path format in
    { digest = Blake3.digest hasher; columns }
;;
