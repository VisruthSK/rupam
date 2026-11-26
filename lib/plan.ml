module C = Core

let explain (t : C.table) =
  let rec source (t : C.table) =
    match t.source with
    | Read f -> [ Printf.sprintf "read %s" (Filename.basename f.path) ]
    | Load { table; contract; _ } ->
      [ Printf.sprintf "load %s from %s" table.name (Filename.basename contract.path) ]
    | Bound id -> [ "start from " ^ id.name ]
    | Join j ->
      source j.left
      @ source j.right
      @ [ Printf.sprintf "%s join on %s" (C.join_name j.kind) j.key ]
  in
  let step (s : C.stage) =
    match s.step with
    | Filter _ -> "filter rows"
    | Mutate { name; by = []; _ } -> "mutate " ^ name
    | Mutate { name; by; _ } ->
      Printf.sprintf "mutate %s by %s" name (String.concat ", " by)
    | Select names -> "select " ^ String.concat ", " names
    | Rename changes ->
      "rename "
      ^ String.concat ", " (List.map (fun (fresh, old) -> old ^ " to " ^ fresh) changes)
    | Summarize { by; measures } ->
      "summarize "
      ^ String.concat ", " (List.map fst measures)
      ^ if by = [] then "" else " by " ^ String.concat ", " by
    | Arrange _ -> "arrange"
    | Take n -> Printf.sprintf "take the first %d rows" n
    | Distinct _ -> "distinct rows"
    | Assert_key columns -> "check that " ^ String.concat ", " columns ^ " is a key"
    | Pivot_longer _ -> "pivot longer"
    | Pivot_wider _ -> "pivot wider"
  in
  source t @ List.map step t.stages
;;
