open Shape

type step =
  | Arrange of (expr * bool) list
  | Summarize of (string * expr) list * (string * expr option) list
  | Take of int
  | Distinct of expr list
  | Filter of expr
  | Window of string * expr * (string * expr) list
  | Pivot of Pivot.t
  | Rename of (string * string * int) list
  | Bind of string * expr
  | Select of string list

type t =
  { locations : (step * Syntax.span) list
  ; context : int
  ; schema : (string * expr) list
  ; steps : step list
  ; next : int
  ; ordered : bool
  }

let source context fields =
  let schema =
    List.mapi
      (fun id (name, primitive, nullable) ->
         name, input id { primitive; nullable; shape = Column context })
      fields
  in
  { locations = []
  ; context
  ; schema
  ; steps = []
  ; ordered = false
  ; next = List.length fields
  }
;;

let names table = List.map fst table.schema

let select table arguments =
  let name = function
    | { Syntax.form = Name ((Bare | Column), n); _ } -> n
    | _ -> raise (Error "Expected named column selector")
  in
  let resolve x =
    let n = name x in
    ignore (Check.lookup n table.schema);
    n
  in
  let fields = names table in
  let range first last =
    let rec from = function
      | [] -> raise (Error "Invalid schema range")
      | n :: rest -> if n = first then through (n :: rest) else from rest
    and through = function
      | [] -> raise (Error "Range ends before its start")
      | n :: rest -> if n = last then [ n ] else n :: through rest
    in
    from fields
  in
  let expand = function
    | ( None
      , { Syntax.form =
            Call
              ( { form = Name (Bare, "starts_with"); _ }
              , [ (None, { form = Literal (Text prefix); _ }) ] )
        ; _
        } ) -> List.filter (String.starts_with ~prefix) fields
    | ( None
      , { Syntax.form =
            Call
              ( { form = Name (Bare, "where"); _ }
              , [ (None, { form = Name (Bare, "is_numeric"); _ }) ] )
        ; _
        } ) ->
      List.filter
        (fun n ->
           match (List.assoc n table.schema).typ.primitive with
           | Int64 | Float64 -> true
           | _ -> false)
        fields
    | None, { Syntax.form = Range (a, b); _ } -> range (resolve a) (resolve b)
    | Some _, x | None, x -> [ resolve x ]
  in
  let excluded, included =
    List.partition
      (function
        | None, { Syntax.form = Unary (Negate, _); _ } -> true
        | _ -> false)
      arguments
  in
  let removed =
    List.map
      (function
        | None, { Syntax.form = Unary (Negate, x); _ } -> resolve x
        | _ -> assert false)
      excluded
  in
  let selected =
    (if included = [] && removed <> [] then fields else List.concat_map expand included)
    |> List.filter (fun n -> not (List.mem n removed))
  in
  if List.length selected <> List.length (List.sort_uniq String.compare selected)
  then raise (Error "Duplicate selected column");
  let aliases =
    List.filter_map
      (function
        | Some dst, x -> Some (resolve x, dst)
        | _ -> None)
      included
  in
  let changes =
    List.filter_map
      (fun n ->
         match List.assoc_opt n aliases with
         | None -> None
         | Some dst ->
           (match (List.assoc n table.schema).node with
            | Input i -> Some (n, dst, i)
            | _ -> assert false))
      selected
  in
  let schema =
    List.map
      (fun n ->
         Option.value ~default:n (List.assoc_opt n aliases), List.assoc n table.schema)
      selected
  in
  if List.length schema <> List.length (List.sort_uniq compare (List.map fst schema))
  then raise (Error "Duplicate projection label");
  let steps = Select selected :: table.steps in
  { table with schema; steps = (if changes = [] then steps else Rename changes :: steps) }
;;

let grouping table groups =
  let rec keys x =
    match x.Syntax.form with
    | Name ((Bare | Column), n) -> [ n, Check.lookup n table.schema ]
    | Tuple xs -> List.concat_map keys xs
    | _ -> raise (Error "Expected named grouping columns")
  in
  match groups with
  | [] -> []
  | [ (_, x) ] -> keys x
  | _ -> raise (Error "Repeated by argument")
;;

let transform env table verb arguments =
  match verb, arguments with
  | ("pivot_longer" | "pivot_wider"), _ ->
    let p, fields = Pivot.parse table.schema verb arguments in
    let schema = List.mapi (fun i (n, t) -> n, input (table.next + i) t) fields in
    { table with
      schema
    ; next = table.next + List.length fields
    ; ordered = false
    ; steps = Pivot p :: table.steps
    }
  | "rename", _ ->
    let mapping =
      List.map
        (function
          | Some a, { Syntax.form = Name (Bare, b); _ } ->
            let x = Check.lookup a table.schema in
            (match x.node with
             | Input i -> a, b, i
             | _ -> assert false)
          | _ -> raise (Error "Expected rename(old = new)"))
        arguments
    in
    let changes = List.map (fun (a, b, _) -> a, b) mapping in
    let schema =
      List.map
        (fun (n, v) -> Option.value ~default:n (List.assoc_opt n changes), v)
        table.schema
    in
    let ns = List.map fst schema
    and old = List.map fst changes in
    if
      List.length ns <> List.length (List.sort_uniq compare ns)
      || List.length old <> List.length (List.sort_uniq compare old)
    then raise (Error "Duplicate rename field");
    { table with schema; steps = Rename mapping :: table.steps }
  | "take", [ (None, { Syntax.form = Literal (Int n); _ }) ] ->
    if not table.ordered
    then raise (Error "take requires arrange with explicit ordering keys");
    let count =
      match int_of_string_opt n with
      | Some i when i >= 0 -> i
      | _ -> raise (Error "Invalid take count")
    in
    { table with steps = Take count :: table.steps }
  | "distinct", _ ->
    let projected = if arguments = [] then table else select table arguments in
    { table with
      steps = Distinct (List.map snd projected.schema) :: table.steps
    ; ordered = false
    }
  | "arrange", [] -> raise (Error "arrange requires at least one ordering key")
  | "arrange", _ ->
    let key = function
      | None, { Syntax.form = Call ({ form = Name (Bare, "desc"); _ }, [ (None, x) ]); _ }
        -> Check.expression env table.schema x, true
      | None, x -> Check.expression env table.schema x, false
      | _ -> raise (Error "Expected unnamed order key")
    in
    { table with ordered = true; steps = Arrange (List.map key arguments) :: table.steps }
  | "summarize", _ ->
    let groups, values = List.partition (fun (label, _) -> label = Some "by") arguments in
    let keys = grouping table groups in
    let measures =
      List.map
        (function
          | ( Some n
            , { Syntax.form = Call ({ form = Name (Bare, ("count" | "n")); _ }, []); _ } )
            -> n, None
          | Some n, x ->
            let value = Check.expression env table.schema x in
            if value.typ.shape <> Scalar then raise (Error "Summary must yield a scalar");
            n, Some value
          | _ -> raise (Error "Summary requires named expressions"))
        values
    in
    let fields =
      List.map (fun (n, x) -> n, x.typ) keys
      @ List.map
          (fun (n, x) ->
             ( n
             , Option.fold
                 ~none:{ primitive = Int64; nullable = false; shape = Scalar }
                 ~some:(fun e -> e.typ)
                 x ))
          measures
    in
    let labels = List.map fst fields in
    if List.length labels <> List.length (List.sort_uniq String.compare labels)
    then raise (Error "Duplicate summary label");
    let schema =
      List.mapi
        (fun i (n, t) ->
           n, input (table.next + i) { t with shape = Column table.context })
        fields
    in
    { table with
      schema
    ; ordered = false
    ; next = table.next + List.length schema
    ; steps = Summarize (keys, measures) :: table.steps
    }
  | "select", _ -> select table arguments
  | "filter", (None, x) :: options ->
    let policy =
      match options with
      | [] -> None
      | [ (Some "missing", { Syntax.form = Name (_, "skip"); _ }) ] -> Some Shape.Skip
      | [ (Some "missing", { Syntax.form = Name (_, "propagate"); _ }) ] -> Some Propagate
      | [ (Some "missing", { Syntax.form = Name (_, "error"); _ }) ] -> Some Fail
      | [ (Some "missing", _) ] ->
        raise (Error "missing must be skip, propagate or error")
      | _ -> raise (Error "filter takes one condition and an optional missing =")
    in
    let predicate =
      Shape.filter table.context policy (Check.expression env table.schema x)
    in
    { table with ordered = false; steps = Filter predicate :: table.steps }
  | "filter", _ -> raise (Error "filter takes one condition and an optional missing =")
  | "mutate", _ ->
    let by, arguments = List.partition (fun (n, _) -> n = Some "by") arguments in
    let keys = grouping table by in
    List.fold_left
      (fun table (name, x) ->
         let n =
           match name with
           | Some n -> n
           | None -> raise (Error "Expected named binding")
         in
         let exists = List.mem_assoc n table.schema in
         let value =
           broadcast (Column table.context) (Check.expression env table.schema x)
         in
         let field = n, input table.next value.typ in
         let schema =
           if exists
           then List.map (fun ((k, _) as f) -> if k = n then field else f) table.schema
           else table.schema @ [ field ]
         in
         { table with
           schema
         ; next = table.next + 1
         ; steps =
             (if keys = [] then Bind (n, value) else Window (n, value, keys))
             :: table.steps
         })
      table
      arguments
  | _ -> raise (Error ("Unsupported table operation: " ^ verb))
;;

let stage ?span env table verb arguments =
  let result = transform env table verb arguments in
  let location = function
    | Bind (n, _) | Window (n, _, _) ->
      (match List.assoc_opt (Some n) arguments with
       | Some x -> Some x.Syntax.span
       | None -> span)
    | _ -> span
  in
  let rec collect steps acc =
    if steps == table.steps
    then acc
    else (
      match steps with
      | [] -> acc
      | s :: rest ->
        collect
          rest
          (match location s with
           | Some p -> (s, p) :: acc
           | None -> acc))
  in
  { result with locations = collect result.steps result.locations }
;;

let render_step annotate table render step =
  let text = render step in
  match List.find_opt (fun (s, _) -> s == step) table.locations with
  | Some (_, span) -> annotate span text
  | None -> text
;;

let execute plan ~rows inputs =
  let open Column_eval in
  let buffers = Array.make plan.next [||] in
  List.iteri
    (fun i values ->
       if Array.length values <> rows then raise (Error "Source column length mismatch");
       buffers.(i) <- values)
    inputs;
  let size = ref rows
  and next = ref (List.length inputs) in
  let eval x =
    match evaluate ~rows:!size ~input:(fun i -> Many buffers.(i)) x with
    | Many vs -> vs
    | One _ -> raise (Error "Expected column expression")
  in
  let reorder indices =
    for i = 0 to !next - 1 do
      if Array.length buffers.(i) = !size
      then buffers.(i) <- Array.map (Array.get buffers.(i)) indices
    done;
    size := Array.length indices
  in
  let apply = function
    | Take n -> reorder (Array.init (min !size n) Fun.id)
    | Distinct keys ->
      let module Seen = Set.Make (struct
          type t = value list

          let compare = compare
        end)
      in
      let seen = ref Seen.empty in
      let vs = List.map eval keys in
      let rows =
        Seq.ints 0
        |> Seq.take !size
        |> Seq.filter (fun i ->
          let key = List.map (fun a -> a.(i)) vs in
          if Seen.mem key !seen
          then false
          else (
            seen := Seen.add key !seen;
            true))
      in
      reorder (Array.of_seq rows)
    | Arrange keys ->
      let keys = List.map (fun (x, d) -> eval x, d) keys in
      let order = Array.init !size Fun.id in
      let rec compare_keys i j = function
        | [] -> 0
        | (vs, descending) :: rest ->
          let c =
            match vs.(i), vs.(j) with
            | Missing, Missing -> 0
            | Missing, _ -> 1
            | _, Missing -> -1
            | a, b -> if descending then compare b a else compare a b
          in
          if c = 0 then compare_keys i j rest else c
      in
      Array.stable_sort (fun i j -> compare_keys i j keys) order;
      reorder order
    | Summarize (keys, measures) ->
      let key_values = List.map (fun (_, x) -> eval x) keys in
      let module Groups = Map.Make (struct
          type t = value list

          let compare = compare
        end)
      in
      let groups = ref (if keys = [] then Groups.singleton [] [] else Groups.empty) in
      for i = 0 to !size - 1 do
        let key = List.map (fun a -> a.(i)) key_values in
        groups
        := Groups.update
             key
             (fun rows -> Some (i :: Option.value rows ~default:[]))
             !groups
      done;
      let total = Groups.cardinal !groups in
      let outputs =
        Array.init
          (List.length keys + List.length measures)
          (fun _ -> Array.make total Missing)
      in
      Groups.bindings !groups
      |> List.iteri (fun row (labels, indices) ->
        let indices = Array.of_list (List.rev indices) in
        let measure = function
          | _, None -> I (Int64.of_int (Array.length indices))
          | _, Some x ->
            let input i = Many (Array.map (Array.get buffers.(i)) indices) in
            (match evaluate ~rows:(Array.length indices) ~input x with
             | One v -> v
             | _ -> assert false)
        in
        List.iteri
          (fun col v -> outputs.(col).(row) <- v)
          (labels @ List.map measure measures));
      Array.iter
        (fun a ->
           buffers.(!next) <- a;
           incr next)
        outputs;
      size := total
    | Pivot p ->
      let count, columns = Pivot.execute p ~rows:!size ~eval in
      List.iter
        (fun a ->
           buffers.(!next) <- a;
           incr next)
        columns;
      size := count
    | Rename _ | Select _ -> ()
    | Window (_, v, keys) ->
      let module Groups = Map.Make (struct
          type t = value list

          let compare = compare
        end)
      in
      let groups = ref Groups.empty
      and out = Array.make !size Missing in
      let keys = List.map (fun (_, x) -> eval x) keys in
      for i = 0 to !size - 1 do
        let key = List.map (fun vs -> vs.(i)) keys in
        groups
        := Groups.update key (fun g -> Some (i :: Option.value ~default:[] g)) !groups
      done;
      Groups.iter
        (fun _ rows ->
           let ids = Array.of_list (List.rev rows) in
           let input i = Many (Array.map (Array.get buffers.(i)) ids) in
           match evaluate ~rows:(Array.length ids) ~input v with
           | Many vs -> Array.iteri (fun k i -> out.(i) <- vs.(k)) ids
           | _ -> assert false)
        !groups;
      buffers.(!next) <- out;
      incr next
    | Bind (_, x) ->
      buffers.(!next) <- eval x;
      incr next
    | Filter x ->
      let keep =
        Array.map
          (function
            | B b -> b
            | _ -> raise (Error "Expected Bool"))
          (eval x)
      in
      let indices =
        Array.to_seqi keep
        |> Seq.filter_map (fun (i, b) -> if b then Some i else None)
        |> Array.of_seq
      in
      reorder indices
  in
  List.iter apply (List.rev plan.steps);
  !size, List.map (fun (name, x) -> name, eval x) plan.schema
;;

let rec pipeline env tables x =
  match x.Syntax.form with
  | Syntax.Name (Bare, n) -> Check.lookup n tables
  | Pipe (base, { form = Call ({ form = Name (Bare, verb); _ }, args); _ }) ->
    stage env (pipeline env tables base) verb args
  | _ -> raise (Error "Expected a table pipeline")
;;
