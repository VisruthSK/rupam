open Rupam

let inputs : Scope.inputs =
  { Scope.offline with
    columns =
      (fun ~missing:_ _ ->
        [ { Input.name = "species"; typ = Supported String; nullable = false }
        ; { name = "mass"; typ = Supported Int; nullable = true }
        ])
  ; digest = (fun _ -> "descriptor")
  }
;;

let source =
  {|let birds = read_csv("birds.csv")
let heavy = birds |> filter(some(mass), mass > 4000) |> mutate(kg = mass / 1000.0, by = species)
|}
;;

let () =
  let env = Scope.empty ~inputs ~directory:"." ~strict:true () in
  let _, items = Elaborate.program env (Frontend.read source) in
  List.iter
    (fun (module B : Backend.BACKEND) ->
       let script, _ = B.script items in
       if not (Astring.String.is_infix ~affix:"heavy" script)
       then failwith ("generated code lacks the binding:\n" ^ script))
    [ (module Backend.R_dplyr); (module Backend.Python_polars) ];
  match
    Elaborate.program (Scope.empty ~directory:"." ~strict:true ()) (Frontend.read source)
  with
  | _ -> failwith "an offline compiler read an input"
  | exception Diagnostic.Error { code = Unreadable_input; _ } -> ()
;;
