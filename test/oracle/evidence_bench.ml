let time label size f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf
    "  %-22s %8.3f s  %6.2f GB/s\n%!"
    label
    elapsed
    (float_of_int size /. 1e9 /. elapsed);
  result
;;

let () =
  Array.iteri
    (fun i path ->
       if i > 0
       then (
         let size = (Unix.stat path).st_size in
         Printf.printf "%s (%.2f GB)\n" (Filename.basename path) (float_of_int size /. 1e9);
         ignore (time "digest" size (fun () -> Rupam_native.Data_file.digest path));
         let columns =
           time "inference + evidence" size (fun () ->
             Rupam_native.Data_file.columns ~missing:Input.default_missing path)
         in
         List.iter
           (fun (c : Input.column) ->
              Printf.printf
                "    %s %s%s\n"
                c.name
                (match c.typ with
                 | Supported t -> Types.display t
                 | Unsupported raw -> raw)
                (if c.nullable then "?" else ""))
           columns))
    Sys.argv
;;
