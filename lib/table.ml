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
  | Assert_key of string list

type key =
  { columns : string list
  ; present : bool
  }

type t =
  { locations : (step * Syntax.span) list
  ; context : int
  ; schema : (string * expr) list
  ; steps : step list
  ; next : int
  ; keys : key list
  ; order : string list option
  }

let source ?(keys = []) context fields =
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
  ; keys
  ; order = None
  ; next = List.length fields
  }
;;

let names table = List.map fst table.schema

let key table columns =
  { columns
  ; present = List.for_all (fun c -> not (List.assoc c table.schema).typ.nullable) columns
  }
;;

let unique table columns =
  List.exists (fun k -> List.for_all (fun c -> List.mem c columns) k.columns) table.keys
;;

let total table =
  match table.order with
  | Some sorted -> unique table sorted
  | None -> false
;;

let prefix keep columns =
  let rec go = function
    | c :: rest when keep c -> c :: go rest
    | _ -> []
  in
  go columns
;;

let restrict keep table =
  { table with
    keys = List.filter (fun k -> List.for_all keep k.columns) table.keys
  ; order = Option.map (prefix keep) table.order
  }
;;

let rename_columns changes table =
  let renamed n = Option.value ~default:n (List.assoc_opt n changes) in
  { table with
    keys = List.map (fun k -> { k with columns = List.map renamed k.columns }) table.keys
  ; order = Option.map (List.map renamed) table.order
  }
;;

let select table arguments =
  let name = function
    | { Syntax.form = Name ((Bare | Column), n); _ } -> n
    | _ -> Diagnostic.fail Invalid_selector "Expected named column selector"
  in
  let resolve x =
    let n = name x in
    ignore (Check.column n table.schema);
    n
  in
  let fields = names table in
  let range first last =
    let rec from = function
      | [] -> Diagnostic.fail Invalid_selector "Invalid schema range"
      | n :: rest -> if n = first then through (n :: rest) else from rest
    and through = function
      | [] -> Diagnostic.fail Invalid_selector "Range ends before its start"
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
  then Diagnostic.fail Duplicate_name "Duplicate selected column";
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
  then Diagnostic.fail Duplicate_name "Duplicate projection label";
  let steps = Select selected :: table.steps in
  let kept = restrict (fun c -> List.mem c selected) table in
  rename_columns
    (List.map (fun (a, b, _) -> a, b) changes)
    { kept with
      schema
    ; steps = (if changes = [] then steps else Rename changes :: steps)
    }
;;

let grouping table groups =
  let rec keys x =
    match x.Syntax.form with
    | Name ((Bare | Column), n) -> [ n, Check.column n table.schema ]
    | Tuple xs | List xs -> List.concat_map keys xs
    | _ -> Diagnostic.fail Wrong_arguments "by takes a column or a list of columns"
  in
  match groups with
  | [] -> []
  | [ (_, x) ] -> keys x
  | _ -> Diagnostic.fail Wrong_arguments "Repeated by argument"
;;

type options =
  { missing : Shape.policy option
  ; group_missing : Shape.policy option
  ; rest : (string option * Syntax.expr) list
  }

let options arguments =
  let named label = List.assoc_opt (Some label) arguments |> Option.map Check.policy in
  { missing = named "missing"
  ; group_missing = named "group_missing"
  ; rest =
      List.filter
        (fun (label, _) -> label <> Some "missing" && label <> Some "group_missing")
        arguments
  }
;;

let group_policy env options keys =
  if options.group_missing <> None && keys = []
  then Diagnostic.fail Group_missing_without_by "group_missing needs by =";
  let first = List.find_map Fun.id in
  match List.filter (fun (_, (k : Shape.expr)) -> k.typ.nullable) keys with
  | [] -> None
  | (name, _) :: _ ->
    (match
       first
         [ options.group_missing
         ; options.missing
         ; Check.default env Group_missing
         ; Check.default env Missing
         ]
     with
     | Some p -> Some p
     | None ->
       Diagnostic.fail
         Missing_policy_required
         ("The grouping key "
          ^ name
          ^ " can be missing. Say what to do with those rows: group_missing = skip \
             leaves them out, propagate makes missing a group, error stops the run"))
;;

let present keys =
  List.filter_map
    (fun (_, (k : Shape.expr)) ->
       if k.typ.nullable then Some (Shape.apply Present [ k ]) else None)
    keys
  |> function
  | [] -> Shape.constant (`Bool true)
  | t :: rest -> List.fold_left (fun a b -> Shape.binary And a b) t rest
;;

let require_keys table keys =
  let check =
    { Shape.node =
        Required (Shape.choose (present keys) (Shape.constant (`Bool true)) Shape.null)
    ; typ = { primitive = Bool; nullable = false; shape = Column table.context }
    }
  in
  { table with steps = Filter check :: table.steps }
;;

let drop_missing_keys table keys =
  let schema =
    List.map
      (fun ((n, (e : Shape.expr)) as field) ->
         if List.mem_assoc n keys
         then n, { e with typ = { e.typ with nullable = false } }
         else field)
      table.schema
  in
  { table with schema; steps = Filter (present keys) :: table.steps }
;;

let transform env table verb arguments =
  match verb, arguments with
  | ("pivot_longer" | "pivot_wider"), _ ->
    let p, fields = Pivot.parse table.schema verb arguments in
    let schema = List.mapi (fun i (n, t) -> n, input (table.next + i) t) fields in
    { table with
      schema
    ; next = table.next + List.length fields
    ; keys = []
    ; order = None
    ; steps = Pivot p :: table.steps
    }
  | "rename", _ ->
    let mapping =
      List.map
        (function
          | Some b, { Syntax.form = Name ((Bare | Column), a); _ } ->
            let x = Check.column a table.schema in
            (match x.node with
             | Input i -> a, b, i
             | _ -> assert false)
          | _ -> Diagnostic.fail Wrong_arguments "rename takes new = old")
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
    then Diagnostic.fail Duplicate_name "Duplicate rename field";
    rename_columns changes { table with schema; steps = Rename mapping :: table.steps }
  | "slice_head", [ (Some "n", { Syntax.form = Literal (Int n); _ }) ] ->
    if not (total table)
    then
      Diagnostic.fail
        Unordered_slice
        "slice_head needs a total order: arrange by columns that include a key of the \
         table, or establish one with assert_key";
    let count =
      match int_of_string_opt n with
      | Some i when i >= 0 -> i
      | _ -> Diagnostic.fail Wrong_arguments "slice_head needs a nonnegative n"
    in
    { table with steps = Take count :: table.steps }
  | "slice_head", _ -> Diagnostic.fail Wrong_arguments "slice_head takes n = count"
  | "distinct", _ ->
    let projected = if arguments = [] then table else select table arguments in
    { table with
      steps = Distinct (List.map snd projected.schema) :: table.steps
    ; keys = [ key projected (names projected) ]
    ; order = None
    }
  | "assert_key", _ ->
    let columns =
      List.map
        (function
          | None, { Syntax.form = Name ((Bare | Column), n); _ } ->
            ignore (Check.column n table.schema);
            n
          | _ -> Diagnostic.fail Wrong_arguments "assert_key takes column names")
        arguments
    in
    if columns = []
    then Diagnostic.fail Wrong_arguments "assert_key needs at least one column";
    { table with
      keys = key table columns :: table.keys
    ; steps = Assert_key columns :: table.steps
    }
  | "arrange", [] ->
    Diagnostic.fail Wrong_arguments "arrange requires at least one ordering key"
  | "arrange", _ ->
    let key = function
      | None, { Syntax.form = Call ({ form = Name (Bare, "desc"); _ }, [ (None, x) ]); _ }
        -> Check.expression env table.schema x, true
      | None, x -> Check.expression env table.schema x, false
      | _ -> Diagnostic.fail Wrong_arguments "Expected unnamed order key"
    in
    let keys = List.map key arguments in
    let sorted =
      List.map
        (fun ((x : Shape.expr), _) ->
           List.find_map
             (fun (n, (e : Shape.expr)) -> if e == x then Some n else None)
             table.schema)
        keys
      |> prefix Option.is_some
      |> List.map Option.get
    in
    { table with order = Some sorted; steps = Arrange keys :: table.steps }
  | "summarize", _ ->
    let options = options arguments in
    let missing = options.missing in
    let groups, values =
      List.partition (fun (label, _) -> label = Some "by") options.rest
    in
    let keys = grouping table groups in
    let table =
      match group_policy env options keys with
      | Some Skip -> drop_missing_keys table keys
      | Some Fail -> require_keys table keys
      | Some Propagate | None -> table
    in
    let keys = grouping table groups in
    let measures =
      List.map
        (function
          | ( Some n
            , { Syntax.form = Call ({ form = Name (Bare, ("count" | "n")); _ }, []); _ } )
            -> n, None
          | Some n, x ->
            let value = Check.expression ?missing env table.schema x in
            if value.typ.shape <> Scalar
            then Diagnostic.fail Scalar_required "Summary must yield a scalar";
            n, Some value
          | _ -> Diagnostic.fail Wrong_arguments "Summary requires named expressions")
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
    then Diagnostic.fail Duplicate_name "Duplicate summary label";
    let schema =
      List.mapi
        (fun i (n, t) ->
           n, input (table.next + i) { t with shape = Column table.context })
        fields
    in
    let result =
      { table with
        schema
      ; next = table.next + List.length schema
      ; steps = Summarize (keys, measures) :: table.steps
      ; order = (if keys = [] then Some [] else None)
      }
    in
    { result with keys = [ key result (List.map fst keys) ] }
  | "select", _ -> select table arguments
  | "filter", (None, x) :: options ->
    let policy =
      match options with
      | [] -> Check.default env Missing
      | [ (Some "missing", p) ] -> Some (Check.policy p)
      | _ ->
        Diagnostic.fail
          Wrong_arguments
          "filter takes one condition and an optional missing ="
    in
    let predicate =
      Shape.filter table.context policy (Check.expression env table.schema x)
    in
    let rec present (x : Syntax.expr) =
      match x.form with
      | Call
          ( { form = Name (Bare, "some"); _ }
          , [ (None, { form = Name ((Bare | Column), n); _ }) ] ) -> [ n ]
      | Binary (And, a, b) -> present a @ present b
      | _ -> []
    in
    let refined = present x in
    let schema =
      List.map
        (fun ((n, (e : Shape.expr)) as field) ->
           if List.mem n refined
           then n, { e with typ = { e.typ with nullable = false } }
           else field)
        table.schema
    in
    { table with schema; steps = Filter predicate :: table.steps }
  | "filter", _ ->
    Diagnostic.fail Wrong_arguments "filter takes one condition and an optional missing ="
  | "mutate", _ ->
    let options = options arguments in
    let missing = options.missing in
    let by, arguments = List.partition (fun (n, _) -> n = Some "by") options.rest in
    let keys = grouping table by in
    let group = group_policy env options keys in
    let table = if group = Some Fail then require_keys table keys else table in
    let grouped value =
      let dependent =
        Shape.exists
          (fun e ->
             match e.node with
             | Reduce _ | Count -> true
             | _ -> false)
          value
      in
      if group = Some Skip && dependent
      then Shape.choose (present keys) value Shape.null
      else value
    in
    List.fold_left
      (fun table (name, x) ->
         let n =
           match name with
           | Some n -> n
           | None -> Diagnostic.fail Wrong_arguments "Expected named binding"
         in
         let exists = List.mem_assoc n table.schema in
         let table = if exists then restrict (fun c -> c <> n) table else table in
         let value =
           broadcast (Column table.context) (Check.expression ?missing env table.schema x)
           |> grouped
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
  | _ -> Diagnostic.fail Unknown_verb ("Unsupported table operation: " ^ verb)
;;

let stage ?span env table verb arguments =
  let verb, arguments =
    match verb with
    | "count" ->
      let span =
        match arguments with
        | (_, x) :: _ -> x.Syntax.span
        | [] -> Option.value span ~default:(Lexing.dummy_pos, Lexing.dummy_pos)
      in
      let at form = { Syntax.form; span } in
      ( "summarize"
      , [ Some "n", at (Call (at (Name (Bare, "n")), []))
        ; Some "by", at (List (List.map snd arguments))
        ] )
    | _ -> verb, arguments
  in
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

let execute plan ~rows inputs =
  let open Column_eval in
  let buffers = Array.make plan.next [||] in
  List.iteri
    (fun i values ->
       if Array.length values <> rows then invalid_arg "Source column length mismatch";
       buffers.(i) <- values)
    inputs;
  let size = ref rows
  and next = ref (List.length inputs) in
  let eval x =
    match evaluate ~rows:!size ~input:(fun i -> Many buffers.(i)) x with
    | Many vs -> vs
    | One _ -> invalid_arg "Expected column expression"
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
    | Rename _ | Select _ | Assert_key _ -> ()
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
            | _ -> invalid_arg "Expected Bool")
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
  | Syntax.Name (Bare, n) ->
    (match List.assoc_opt n tables with
     | Some t -> t
     | None ->
       Diagnostic.fail
         Unknown_table
         ("Unknown table "
          ^ n
          ^ Check.did_you_mean (Check.closest n (List.map fst tables))))
  | Pipe (base, { form = Call ({ form = Name (Bare, verb); _ }, args); _ }) ->
    stage env (pipeline env tables base) verb args
  | _ -> Diagnostic.fail Not_a_table "Expected a table pipeline"
;;
