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
