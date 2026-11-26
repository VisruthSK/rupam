open Rupam

let run source =
  match
    let env, items =
      Elaborate.program
        (Scope.empty ~directory:"." ~strict:true)
        (Frontend.parse (Lexing.from_string source))
    in
    let values, shown = Reference.run [] items in
    List.iter
      (fun (name, binding, description) ->
         match (binding : Scope.binding) with
         | Value { id; _ } ->
           Printf.printf
             "%s : %s = %s\n"
             name
             description
             (Reference.display (List.assoc id.stamp values))
         | Function _ -> Printf.printf "%s : %s\n" name description
         | _ -> ())
      (Scope.bindings env);
    List.iter
      (fun ((e : Core.expr), v) ->
         Printf.printf "%s : %s\n" (Reference.display v) (Types.display e.typ))
      shown
  with
  | () -> ()
  | exception exn -> print_endline (Diagnostic.name (Diagnostic.of_exn exn).code)
;;

let%expect_test "functions generalise and literals adapt" =
  run "let square(x) = x * x\nsquare(3)\nsquare(1.5)";
  run "let inc(x) = x + 1\n(inc(2), inc(2.5))";
  run "let half(x) = x / 2\nhalf(9)";
  [%expect
    {|
    square : (number) -> number
    9 : Int64
    2.25 : Float64
    inc : (number) -> number
    (3, 3.5) : (Int64, Float64)
    half : (number) -> Float64
    4.5 : Float64
    |}]
;;

let%expect_test "closures capture values, not names" =
  run "let a = 2\nlet f(x) = x * a\nlet a = 3\n4 |> f";
  [%expect
    {|
    f : (Int64) -> Int64
    a : Int64 = 3
    8 : Int64
    |}]
;;

let%expect_test "guarded branches and overflow" =
  run "if (FALSE) 9223372036854775807 + 1 else 3";
  run "9223372036854775807 + 1";
  run "1e308 * 1e308";
  [%expect
    {|
    3 : Int64
    arithmetic-overflow
    arithmetic-overflow
    |}]
;;

let%expect_test "annotations" =
  run "let shift(d: Float64): Float64 = d + 1.0\nshift(3.0)";
  run "let bad(x: Int64): String = x";
  [%expect
    {|
    shift : (Float64) -> Float64
    4.0 : Float64
    type-mismatch
    |}]
;;

let%expect_test "records are row polymorphic" =
  run "let station(note) = note.station\nstation({station = \"MRS\", n = 3})";
  [%expect
    {|
    station : ({station: a, ..b}) -> a
    "MRS" : String
    |}]
;;

let%expect_test "reductions and table parameters are typed at definition" =
  run "let center(x) = x - mean(x)";
  run "let active_mean(data) = data |> filter(active) |> summarize(m = mean(weight))";
  run "let broken(x) = x + unknown_name";
  [%expect
    {|
    center : (Float64) -> Float64?
    active_mean : (Table<{active: Bool, weight: number, ..a}>) -> Table<{m: Float64?}>
    unknown-name
    |}]
;;

let%expect_test "optional values lift" =
  run "let inc(x) = x + 1\n(inc(Some(2)), inc(None))";
  run "coalesce(None, 4)";
  run "\"n = {1 + 1}\"";
  [%expect
    {|
    inc : (number) -> number
    (3, None) : (Int64?, Int64?)
    4 : Int64
    "n = 2" : String
    |}]
;;

let%expect_test "matches are exhaustive" =
  run
    "type Maybe<a> = | Empty | Just(value: a)\n\
     let get(m) = match m { Empty => 0, Just(n) => n }\n\
     get(Just(5))";
  run "let f(p) = match p { (TRUE, FALSE) => 1, (FALSE, _) => 2 }";
  run "let f(n) = match n { 0 => TRUE }";
  run "let f(x) = match x { (a, Some(b)) => b, (_, None) => 0 }";
  run
    "let grade(bmi) = match bmi { ..<18.5 => 1, 18.5..<25.0 => 2, 25.0.. => 3 }\n\
     grade(22.0)";
  [%expect
    {|
    get : (Maybe<number>) -> number
    5 : Int64
    nonexhaustive-match
    nonexhaustive-match
    f : ((a, number?)) -> number
    grade : (Float64) -> number
    2 : Int64
    |}]
;;

let%expect_test "reserved and R forms" =
  run "let f(y, x) = y ~ x + 1";
  run "1:3";
  [%expect
    {|
    unsupported-feature
    r-range
    |}]
;;

let%expect_test "comparisons chain in one direction" =
  run "let x = 5\n0 < x * 2 <= 10";
  run "1 < 2 > 0";
  run "1 == 1 == 1";
  [%expect
    {|
    x : Int64 = 5
    TRUE : Bool
    syntax-error
    syntax-error
    |}]
;;
