open Rupam
open Rupam_native
module G = QCheck.Gen

type outcome =
  | Value of string
  | Rows of string list * string list list
  | Failed

let ( let* ) = G.( >>= )
let pick xs = G.oneof_list xs
let call name args = name ^ "(" ^ String.concat ", " args ^ ")"
let binary op a b = "(" ^ a ^ " " ^ op ^ " " ^ b ^ ")"

let rec int depth =
  let leaf = G.map string_of_int (G.int_range (-20) 20) in
  if depth = 0
  then leaf
  else (
    let sub = int (depth - 1) in
    G.oneof
      [ leaf
      ; G.map3 binary (pick [ "+"; "-"; "*" ]) sub sub
      ; G.map (fun a -> call "abs" [ a ]) sub
      ; G.map3 (fun f a b -> call f [ a; b ]) (pick [ "pmin"; "pmax" ]) sub sub
      ; G.map3 (fun c a b -> "if (" ^ c ^ ") " ^ a ^ " else " ^ b) (bool (depth - 1)) sub sub
      ; G.map (fun xs -> call "length" [ xs ]) (ints (depth - 1))
      ; G.map (fun xs -> call "reduce" [ xs; "0"; "fn(a, v) => a + v" ]) (ints (depth - 1))
      ; G.map (fun a -> call "coalesce" [ "None"; a ]) sub
      ; G.map2 (fun f d -> call f [ d ]) (pick [ "year"; "month"; "day"; "wday"; "yday" ]) (date (depth - 1))
      ; G.map2 (fun a b -> binary "-" a b) (date (depth - 1)) (date (depth - 1))
      ])

and float depth =
  let leaf = G.map (fun n -> Printf.sprintf "%.2f" (float_of_int n /. 4.)) (G.int_range (-40) 40) in
  if depth = 0
  then leaf
  else (
    let sub = float (depth - 1) in
    G.oneof
      [ leaf
      ; G.map3 binary (pick [ "+"; "-"; "*" ]) sub sub
      ; G.map2 (fun a d -> call "round" [ a; string_of_int d ]) sub (G.int_range 0 2)
      ; G.map (fun a -> call "sqrt" [ call "abs" [ a ] ]) sub
      ; G.map (fun a -> binary "*" a "1.0") (int (depth - 1))
      ])

and bool depth =
  let leaf = pick [ "TRUE"; "FALSE" ] in
  if depth = 0
  then leaf
  else (
    let sub = bool (depth - 1) in
    G.oneof
      [ leaf
      ; G.map3 binary (pick [ "<"; "<="; "=="; "!=" ]) (int (depth - 1)) (int (depth - 1))
      ; G.map (fun a -> "(not " ^ a ^ ")") sub
      ; G.map3 binary (pick [ "and"; "or" ]) sub sub
      ; G.map2 (fun s p -> call "str_detect" [ s; p ]) (text (depth - 1)) (pick [ {|"o"|}; {|"^b"|} ])
      ])

and date depth =
  let leaf =
    G.map3
      (fun y m d -> Printf.sprintf {|d"%04d-%02d-%02d"|} y m d)
      (G.int_range 1999 2030)
      (G.int_range 1 12)
      (G.int_range 1 28)
  in
  if depth = 0
  then leaf
  else (
    let sub = date (depth - 1) in
    G.oneof
      [ leaf
      ; G.map3 binary (pick [ "+"; "-" ]) sub (G.map (fun n -> call "months" [ string_of_int n ]) (G.int_range (-30) 30))
      ; G.map3 binary (pick [ "+"; "-" ]) sub (G.map (fun n -> call "days" [ string_of_int n ]) (G.int_range (-400) 400))
      ; G.map2 (fun d u -> call "floor_date" [ d; u ]) sub (pick [ {|"year"|}; {|"month"|}; {|"week"|} ])
      ])

and text depth =
  let leaf = pick [ {|"foo"|}; {|"Bar"|}; {|"  baz "|}; {|""|} ] in
  if depth = 0
  then leaf
  else
    G.oneof
      [ leaf
      ; G.map2 (fun f s -> call f [ s ]) (pick [ "str_to_upper"; "str_to_lower"; "str_trim" ]) (text (depth - 1))
      ]

and ints depth =
  let literal =
    G.map
      (fun xs -> "[" ^ String.concat ", " xs ^ "]")
      (G.list_size (G.int_range 0 5) (int 0))
  in
  if depth = 0
  then literal
  else (
    let sub = ints (depth - 1) in
    G.oneof
      [ literal
      ; G.map2 (fun f xs -> call f [ xs ]) (pick [ "sort"; "rev"; "unique" ]) sub
      ; G.map (fun xs -> call "map" [ xs; "fn(v) => v * 2" ]) sub
      ; G.map (fun xs -> call "keep" [ xs; "fn(v) => v > 0" ]) sub
      ; G.map3 (fun a b s -> call "seq" [ a; b; s ]) (int 0) (int 0) (pick [ "1"; "2"; "-1"; "-3" ])
      ])
;;

let scalar =
  let* parts = G.list_size (G.int_range 1 4) (G.oneof [ int 3; float 3; bool 3; text 2; ints 2; date 2 ]) in
  G.return
    (match parts with
     | [ one ] -> one
     | parts -> "(" ^ String.concat ", " parts ^ ")")
;;

let pipeline =
  let stage =
    pick
      [ "filter(x > 0, missing = skip)"
      ; "filter(some(y))"
      ; "filter(g != \"b\")"
      ; "mutate(z = x * 3 + 1)"
      ; "mutate(w = coalesce(y, 0.0) * 2.0)"
      ; "mutate(u = str_to_upper(coalesce(s, \"none\")))"
      ; "mutate(e = d + months(1), f = floor_date(d, \"week\"), k = d - d\"2025-01-01\", q = wday(d))"
      ; "filter(d >= d\"2025-01-01\", missing = skip)"
      ]
  in
  let* stages = G.list_size (G.int_range 0 4) stage in
  let* order = pick [ []; [ "arrange(g, x)" ]; [ "arrange(desc(y))" ] ] in
  let* last =
    pick
      [ []
      ; [ "select(g, x)" ]
      ; [ "summarize(n = n(), total = sum(x, missing = skip), by = g)" ]
      ; [ "summarize(mean_y = mean(y, missing = skip))" ]
      ]
  in
  G.return
    (String.concat " |> " ({|read_csv("small.csv")|} :: (stages @ order @ last)))
;;

let reference source =
  match
    let items =
      snd
        (Elaborate.program
           (Scope.empty ~inputs:Native.inputs ~directory:"." ~strict:true ())
           (Frontend.read source))
    in
    let env, shown = Reference.run [] items in
    match List.rev items with
    | TransformIR.Define (id, _) :: _ ->
      Option.get (Reference.rows (List.assoc id.stamp env))
      |> fun (names, rows) -> Rows (names, rows)
    | _ -> Value (Reference.display (snd (List.hd (List.rev shown))))
  with
  | outcome -> outcome
  | exception Diagnostic.Error _ -> Failed
;;


let counter = ref 0

let host session source ~table =
  match
    if table
    then (
      incr counter;
      let name = Printf.sprintf "t%d" !counter in
      ignore (Session.submit session (Frontend.read ("let " ^ name ^ " = " ^ source)));
      let result = Session.result session (Session.table session name) in
      let names = List.map fst (Session.table session name).schema in
      Rows
        ( names
        , List.init (Session.total result) (fun row ->
            List.map (fun n -> Rupam_fixture.Fixture.cell (snd (Session.cell result ~row n))) names) ))
    else (
      match
        List.find_map
          (function
            | Session.Value (text, _) -> Some text
            | _ -> None)
          (Session.submit session (Frontend.read source))
      with
      | Some text -> Value text
      | None -> Failed)
  with
  | outcome -> outcome
  | exception _ -> Failed
;;

let show = function
  | Value v -> v
  | Failed -> "<failed>"
  | Rows (names, rows) ->
    String.concat "\n" (String.concat "," names :: List.map (String.concat ",") rows)
;;

let sessions =
  List.map
    (fun name ->
       let target = List.assoc name Target.targets in
       let session = Session.create ~project:Project.standalone ~directory:"." in
       Session.set_target session target ~origin:"differential";
       name, session)
    [ "r-dplyr"; "python-polars" ]
;;

let ran = Hashtbl.create 2

let agree ~table source =
  let expected = reference (if table then "let t = " ^ source else source) in
  let key = if table then "table pipelines" else "scalars and vectors" in
  let total, failed = Option.value (Hashtbl.find_opt ran key) ~default:(0, 0) in
  Hashtbl.replace ran key (total + 1, if expected = Failed then failed + 1 else failed);
  List.for_all
    (fun (name, session) ->
       let actual = host session source ~table in
       actual = expected
       || QCheck.Test.fail_reportf
            "%s\nreference:\n%s\n%s:\n%s"
            source
            (show expected)
            name
            (show actual))
    sessions
;;

let () =
  let tests =
    [ QCheck.Test.make ~count:150 ~name:"scalars and vectors" (QCheck.make ~print:Fun.id scalar)
        (agree ~table:false)
    ; QCheck.Test.make ~count:60 ~name:"table pipelines" (QCheck.make ~print:Fun.id pipeline)
        (agree ~table:true)
    ]
  in
  let failed =
    List.filter
      (fun test ->
         match QCheck.Test.check_exn test with
         | () -> false
         | exception exn ->
           prerr_endline (Printexc.to_string exn);
           true)
      tests
  in
  Hashtbl.iter
    (fun name (total, failed) ->
       Printf.printf "differential %s: %d programs, %d rejected by the reference
" name total failed)
    ran;
  List.iter (fun (_, s) -> Session.close s) sessions;
  if failed <> [] then exit 1
;;
