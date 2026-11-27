module A = Rupam_native.Arrow_result

let time label repeat f =
  ignore (f ());
  let samples =
    Array.init repeat (fun _ ->
      let start = Unix.gettimeofday () in
      ignore (Sys.opaque_identity (f ()));
      Unix.gettimeofday () -. start)
  in
  Array.sort Float.compare samples;
  let ms i = samples.(i) *. 1000. in
  Printf.printf
    "  %-34s p50 %9.3f ms  p95 %9.3f ms\n%!"
    label
    (ms (repeat / 2))
    (ms (min (repeat - 1) (repeat * 95 / 100)))
;;

let bench file =
  Printf.printf "%s\n" (Filename.basename file);
  let ours = A.open_ipc file in
  let rows = A.rows ours in
  let columns = Array.init (min (Array.length (A.names ours)) 10) Fun.id in
  let random = Random.State.make [| 7 |] in
  let offsets = Array.init 64 (fun _ -> Random.State.int random (max 1 (rows - 60))) in
  let page = ref 0 in
  let next () =
    page := (!page + 1) mod Array.length offsets;
    offsets.(!page)
  in
  time "open + close" 20 (fun () -> A.close (A.open_ipc file));
  time "viewport 60 rows x 10 columns" 200 (fun () ->
    ignore (A.cells ours ~columns ~offset:(next ()) ~limit:60));
  time "histogram of a float column" 10 (fun () ->
    ignore (A.histogram ours ~column:1 ~bins:20));
  time "summary of a float column" 10 (fun () -> ignore (A.summary ours ~column:1));
  time "frequencies of a text column" 5 (fun () -> ignore (A.frequencies ours ~column:3));
  A.close ours
;;

let () =
  let dir = Sys.argv.(1) in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".arrow")
  |> List.sort String.compare
  |> List.iter (fun f -> bench (Filename.concat dir f))
;;
