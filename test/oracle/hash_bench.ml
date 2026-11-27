let () =
  let path = Sys.argv.(1) in
  let size = (Unix.stat path).st_size in
  let start = Unix.gettimeofday () in
  let digest = Rupam_native.Data_file.digest path in
  let elapsed = Unix.gettimeofday () -. start in
  let gb = float_of_int size /. 1e9 in
  Printf.printf
    "BLAKE3:     %s\nTime:       %.3f s\nThroughput: %.3f GB/s\n"
    (String.sub digest 0 16)
    elapsed
    (gb /. elapsed)
;;
