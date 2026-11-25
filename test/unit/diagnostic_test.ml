open Rupam

let%expect_test "every code has a unique name that parses back" =
  let names = List.map Diagnostic.name Diagnostic.all_of_code in
  let unique = List.sort_uniq String.compare names in
  Printf.printf
    "%b %b\n"
    (List.length unique = List.length names)
    (List.for_all
       (fun c -> Diagnostic.of_name (Diagnostic.name c) = Some c)
       Diagnostic.all_of_code);
  [%expect {| true true |}]
;;

let%expect_test "unexpected exceptions become internal errors" =
  List.iter
    (fun exn ->
       let d = Diagnostic.of_exn exn in
       print_endline (Diagnostic.render d))
    [ Failure "boom"; Invalid_argument "bad"; Not_found; Sys_error "gone.csv" ];
  [%expect
    {|
    internal-error
      boom
    internal-error
      bad
    internal-error
      Not_found
    unreadable-input
      gone.csv
    |}]
;;
