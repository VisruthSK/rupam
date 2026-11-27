let digest path =
  In_channel.with_open_bin path (fun channel ->
    let buffer = Bytes.create 65536 in
    let rec feed context =
      match In_channel.input channel buffer 0 (Bytes.length buffer) with
      | 0 -> Digestif.SHA256.(to_hex (get context))
      | n -> feed (Digestif.SHA256.feed_bytes context buffer ~off:0 ~len:n)
    in
    feed Digestif.SHA256.empty)
;;

let columns ~missing path : Input.column list =
  if not (Sys.file_exists path)
  then Diagnostic.fail Unreadable_input ("No such data file " ^ path);
  match Input.format path with
  | Parquet -> Parquet_source.columns path
  | (Csv | Tsv) as format -> Delimited.columns ~missing ~path format
;;
