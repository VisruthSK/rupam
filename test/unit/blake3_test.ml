let%expect_test "official vectors" =
  print_endline (Blake3.string "");
  print_endline (Blake3.string "abc");
  [%expect
    {|
    af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262
    6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85
    |}]
;;

let%test "parallel subtrees equal the sequential hash at every boundary" =
  List.for_all
    (fun n ->
       let data = String.init n (fun i -> Char.chr (i mod 251)) in
       let path = Filename.temp_file "rupam-blake3" ".bin" in
       Fun.protect
         ~finally:(fun () -> Sys.remove path)
         (fun () ->
            Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc data);
            String.equal (Rupam_native.Data_file.digest path) (Blake3.string data)))
    [ 0; 1; 1023; 1024; 1025; 16777215; 16777216; 16777217; 33554433; 50331647; 67108865 ]
;;

let%test "feeding any split of the input equals hashing it whole" =
  let data = String.init 200_000 (fun i -> Char.chr (i * 7 mod 256)) in
  let whole = Blake3.string data in
  List.for_all
    (fun step ->
       let h = Blake3.hasher () in
       let bytes = Bytes.of_string data in
       let i = ref 0 in
       while !i < Bytes.length bytes do
         let n = min step (Bytes.length bytes - !i) in
         Blake3.feed h bytes !i n;
         i := !i + n
       done;
       String.equal (Blake3.digest h) whole)
    [ 1; 7; 64; 1023; 1024; 1025; 65536; 200_000 ]
;;

let%test "a CSV read hashes exactly the bytes of the file" =
  let path = Filename.temp_file "rupam-read" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
       Out_channel.with_open_bin path (fun oc ->
         Out_channel.output_string oc "\xEF\xBB\xBFid,label\r\n";
         for i = 1 to 50_000 do
           Printf.fprintf oc "%d,\"row %d\"\r\n" i i
         done);
       let snapshot =
         Rupam_native.Data_file.read ~missing:Rupam.Input.default_missing path
       in
       String.equal snapshot.digest (Rupam_native.Data_file.digest path)
       && List.length snapshot.columns = 2)
;;
