module A = Rupam_native.Arrow_result
module O = Nanoarrow_oracle

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
  let ours = A.open_ipc file
  and theirs = O.open_ipc file in
  let rows = A.rows ours in
  let width = Array.length (A.names ours) in
  let columns = Array.init (min width 10) Fun.id in
  let random = Random.State.make [| 7 |] in
  let offsets = Array.init 64 (fun _ -> Random.State.int random (max 1 (rows - 60))) in
  let page = ref 0 in
  let next () =
    page := (!page + 1) mod Array.length offsets;
    offsets.(!page)
  in
  List.iter
    (fun (name, repeat, ours_f, theirs_f) ->
       time ("talon     " ^ name) repeat ours_f;
       time ("nanoarrow " ^ name) repeat theirs_f)
    [ ( "open + close"
      , 20
      , (fun () -> A.close (A.open_ipc file))
      , fun () -> O.close (O.open_ipc file) )
    ; ( "viewport 60 rows x 10 columns"
      , 200
      , (fun () -> ignore (A.cells ours ~columns ~offset:(next ()) ~limit:60))
      , fun () -> ignore (O.cells theirs (next ()) 60 columns) )
    ; ( "histogram of a float column"
      , 10
      , (fun () -> ignore (A.histogram ours ~column:1 ~bins:20))
      , fun () -> ignore (O.histogram theirs 1 20) )
    ];
  time "talon     summary of a float column" 10 (fun () -> A.summary ours ~column:1);
  time "talon     frequencies of a text column" 5 (fun () -> A.frequencies ours ~column:3);
  A.close ours;
  O.close theirs
;;

let () =
  let dir = Sys.argv.(1) in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".arrow")
  |> List.sort String.compare
  |> List.iter (fun f -> bench (Filename.concat dir f))
;;
