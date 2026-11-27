let definition =
  Re.compile
    (Re.Perl.re {|^(?:def\s+([A-Za-z_][A-Za-z0-9_]*)|class\s+([A-Za-z_][A-Za-z0-9_]*)|([A-Za-z_][A-Za-z0-9_.]*)\s*(?:<-|=)\s)|})
;;

let word = Re.compile (Re.Perl.re {|[A-Za-z_.][A-Za-z0-9_.]*|})

let top_level line =
  line <> "" && line.[0] <> ' ' && line.[0] <> '\t' && line.[0] <> '}' && line.[0] <> ')'
;;

let blocks adapters =
  List.fold_left
    (fun blocks line ->
       match blocks with
       | (name, body) :: rest when not (top_level line) -> (name, line :: body) :: rest
       | _ ->
         let name =
           Option.bind (Re.exec_opt definition line) (fun g ->
             List.find_map (fun i -> Re.Group.get_opt g i) [ 1; 2; 3 ])
         in
         (name, [ line ]) :: blocks)
    []
    (String.split_on_char '\n' adapters)
  |> List.rev_map (fun (name, body) -> name, String.concat "\n" (List.rev body))
;;

let words text = List.map (fun g -> Re.Group.get g 0) (Re.all word text)

let used adapters code =
  let blocks = blocks adapters in
  let named = List.filter_map (fun (n, body) -> Option.map (fun n -> n, body) n) blocks in
  let rec close needed = function
    | [] -> needed
    | w :: rest when List.mem w needed || not (List.mem_assoc w named) -> close needed rest
    | w :: rest -> close (w :: needed) (words (List.assoc w named) @ rest)
  in
  let needed = close [] (words code) in
  blocks
  |> List.filter (fun (name, body) ->
    match name with
    | Some n -> List.mem n needed
    | None -> String.trim body <> "")
  |> List.map snd
  |> String.concat "\n"
;;
