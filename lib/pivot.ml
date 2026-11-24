open Shape

type t =
  { wider : bool
  ; ids : (string * expr) list
  ; values : (string * expr) list
  ; key : string
  ; value : string
  ; labels : string list
  }

let parse schema verb arguments =
  let argument n =
    match List.assoc_opt (Some n) arguments with
    | Some x -> x
    | None -> raise (Error ("Missing pivot argument " ^ n))
  in
  let name x =
    match x.Syntax.form with
    | Name (Bare, n) | Name (Column, n) -> n
    | Literal (Text n) -> n
    | _ -> raise (Error "Expected literal field name")
  in
  let fields n =
    match (argument n).Syntax.form with
    | Tuple xs | List xs -> List.map name xs
    | _ -> [ name (argument n) ]
  in
  let wider = verb = "pivot_wider" in
  let key, value, labels, selected =
    if wider
    then (
      let k = name (argument "names_from")
      and v = name (argument "values_from") in
      k, v, fields "levels", [ k; v ])
    else (
      let ns = fields "cols" in
      name (argument "names_to"), name (argument "values_to"), ns, ns)
  in
  if
    List.length arguments <> 3
    || labels = []
    || List.length labels <> List.length (List.sort_uniq compare labels)
  then raise (Error "Expected three pivot arguments and distinct nonempty field names");
  let ids = List.filter (fun (n, _) -> not (List.mem n selected)) schema in
  let values = List.map (fun n -> n, Check.lookup n schema) selected in
  let t = { wider; ids; values; key; value; labels } in
  let fields =
    if wider
    then (
      if (List.assoc key schema).typ.primitive <> String
      then raise (Error "Pivot names must be strings");
      List.map (fun n -> n, { (List.assoc value schema).typ with nullable = true }) labels)
    else (
      let ty = (snd (List.hd values)).typ in
      if not (List.for_all (fun (_, x) -> x.typ.primitive = ty.primitive) values)
      then raise (Error "Pivot values must share a type");
      [ key, { ty with primitive = String; nullable = false }
      ; value, { ty with nullable = List.exists (fun (_, x) -> x.typ.nullable) values }
      ])
  in
  let output = List.map (fun (n, x) -> n, x.typ) ids @ fields in
  let names = List.map fst output in
  if List.length names <> List.length (List.sort_uniq compare names)
  then raise (Error "Pivot produces duplicate labels");
  t, output
;;

let execute p ~rows ~eval =
  let open Column_eval in
  let id_arrays = List.map (fun (_, e) -> eval e) p.ids in
  if not p.wider
  then (
    let n = List.length p.values in
    let vs = Array.of_list (List.map (fun (k, e) -> k, eval e) p.values) in
    let count = rows * n in
    ( count
    , List.map (fun a -> Array.init count (fun i -> a.(i / n))) id_arrays
      @ [ Array.init count (fun i -> S (fst vs.(i mod n)))
        ; Array.init count (fun i -> (snd vs.(i mod n)).(i / n))
        ] ))
  else
    let module Groups = Map.Make (struct
        type t = value list

        let compare = compare
      end)
    in
    let groups = ref Groups.empty in
    let keys = eval (List.assoc p.key p.values)
    and values = eval (List.assoc p.value p.values) in
    for i = 0 to rows - 1 do
      let id = List.map (fun a -> a.(i)) id_arrays in
      let key =
        match keys.(i) with
        | S s when List.mem s p.labels -> s
        | _ -> raise (Error "Unexpected pivot key")
      in
      let cells = Option.value ~default:[] (Groups.find_opt id !groups) in
      if List.mem_assoc key cells then raise (Error "Duplicate pivot cell");
      groups := Groups.add id ((key, values.(i)) :: cells) !groups
    done;
    let records =
      Groups.bindings !groups
      |> List.map (fun (id, cells) ->
        Array.of_list
          (id
           @ List.map
               (fun k -> Option.value ~default:Missing (List.assoc_opt k cells))
               p.labels))
      |> Array.of_list
    in
    ( Array.length records
    , List.init
        (List.length p.ids + List.length p.labels)
        (fun i -> Array.map (fun r -> r.(i)) records) )
;;
