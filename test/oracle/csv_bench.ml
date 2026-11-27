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
        while Talon.Scan.next s do
          for r = 0 to Talon.Scan.rows s - 1 do
            total := !total + Talon.Scan.fields s r
          done
        done;
        !total))
  in
  Printf.printf "  %d fields\n" fields;
  ignore
    (time "full inference" (fun () ->
       Rupam_native.Data_file.columns ~missing:Input.default_missing path))
;;
