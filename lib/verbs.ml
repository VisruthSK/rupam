open Syntax
module T = Types
module C = TransformIR

type context =
  { expression : ?missing:C.policy -> T.row -> Syntax.expr -> C.expr
  ; table : Syntax.expr -> C.table
  ; missing : C.policy option
  ; group_missing : C.policy option
  }

type arguments = (string option * Syntax.expr) list

let names =
  [ "filter"
  ; "mutate"
  ; "select"
  ; "rename"
  ; "summarize"
  ; "count"
  ; "arrange"
  ; "slice_head"
  ; "distinct"
  ; "assert_key"
  ; "pivot_longer"
  ; "pivot_wider"
  ]
  @ List.map fst C.join_kinds
;;

let mem name = List.mem name names
let fail span code message = Diagnostic.fail ~span code message
let make span shape typ node = { C.node; typ; shape; span }
let distinct xs = List.length xs = List.length (List.sort_uniq compare xs)

let column span (t : C.table) name =
  match T.field 1 (C.row t) name with
  | Some typ -> typ
  | None ->
    let hint =
      List.map (fun n -> Distance.between n name, n) (C.names t)
      |> List.sort compare
      |> function
      | (d, n) :: _ when d <= 2 -> ". Did you mean " ^ n ^ "?"
      | _ -> ""
    in
    fail span Unknown_column ("No column named " ^ name ^ hint)
;;

let typ (t : C.table) name =
  match T.field 1 (C.row t) name with
  | Some typ -> typ
  | None -> invalid_arg ("No column " ^ name)
;;

let column_name (x : Syntax.expr) =
  match x.form with
  | Name ((Bare | Column), n) | Literal (Text n) -> n
  | _ -> fail x.span Invalid_selector "Expected a column name"
;;

let column_names (x : Syntax.expr) =
  match x.form with
  | Tuple xs | List xs -> List.map column_name xs
  | _ -> [ column_name x ]
;;

let unique (t : C.table) columns =
  List.exists (List.for_all (fun c -> List.mem c columns)) t.keys
;;

let total (t : C.table) =
  match t.order with
  | Some sorted -> unique t sorted
  | None -> false
;;

let rec prefix keep = function
  | c :: rest when keep c -> c :: prefix keep rest
  | _ -> []
;;

let restrict keep (t : C.table) =
  { t with
    keys = List.filter (List.for_all keep) t.keys
  ; order = Option.map (prefix keep) t.order
  }
;;

let private_ (t : C.table) e =
  List.exists (fun n -> List.mem n t.restricted) (C.columns e)
;;

let everything (t : C.table) = { t with restricted = C.names t }
let add span step (t : C.table) = { t with stages = t.stages @ [ { C.step; at = span } ] }

let renamed changes n =
  match List.find_opt (fun (_, old) -> old = n) changes with
  | Some (fresh, _) -> fresh
  | None -> n
;;

let rename_columns changes (t : C.table) =
  let renamed = renamed changes in
  { t with
    schema = List.map (fun (n, typ) -> renamed n, typ) t.schema
  ; keys = List.map (List.map renamed) t.keys
  ; order = Option.map (List.map renamed) t.order
  ; restricted = List.map renamed t.restricted
  }
;;

let select span (t : C.table) (arguments : arguments) =
  let fields = C.names t in
  let resolve x =
    let n = column_name x in
    ignore (column x.span t n);
    n
  in
  let range first last =
    let rec from = function
      | [] ->
        fail span Invalid_selector "The range starts at a column that does not exist"
      | n :: rest -> if n = first then through (n :: rest) else from rest
    and through = function
      | [] -> fail span Invalid_selector "The range ends before its start"
      | n :: rest -> if n = last then [ n ] else n :: through rest
    in
    from fields
  in
  let expand = function
    | ( None
      , { form =
            Call
              ( { form = Name (Bare, "starts_with"); _ }
              , [ (None, { form = Literal (Text prefix); _ }) ] )
        ; _
        } ) -> List.filter (String.starts_with ~prefix) fields
    | ( None
      , { form =
            Call
              ( { form = Name (Bare, "where"); _ }
              , [ (None, { form = Name (Bare, ("is_numeric" | "is_number")); _ }) ] )
        ; _
        } ) -> List.filter (fun n -> T.numeric (T.present (typ t n))) fields
    | None, { form = Range (a, b); _ } -> range (resolve a) (resolve b)
    | (Some _ | None), x -> [ resolve x ]
  in
  let excluded, included =
    List.partition
      (function
        | None, { form = Unary (Negate, _); _ } -> true
        | _ -> false)
      arguments
  in
  let removed =
    List.map
      (function
        | _, { form = Unary (Negate, x); _ } -> resolve x
        | _ -> assert false)
      excluded
  in
  let selected =
    (if included = [] && removed <> [] then fields else List.concat_map expand included)
    |> List.filter (fun n -> not (List.mem n removed))
  in
  if not (distinct selected) then fail span Duplicate_name "A column is selected twice";
  let changes =
    List.filter_map
      (function
        | Some fresh, x -> Some (fresh, resolve x)
        | None, _ -> None)
      included
  in
  let kept =
    restrict
      (fun c -> List.mem c selected)
      { t with
        schema = List.map (fun n -> n, typ t n) selected
      ; restricted = List.filter (fun n -> List.mem n selected) t.restricted
      }
  in
  let kept = add span (Select selected) kept in
  if changes = []
  then kept
  else (
    let result = rename_columns changes (add span (Rename changes) kept) in
    if not (distinct (C.names result))
    then fail span Duplicate_name "Two selected columns have the same name";
    result)
;;

let grouping span (t : C.table) (arguments : arguments) =
  match List.filter (fun (label, _) -> label = Some "by") arguments with
  | [] -> []
  | [ (_, x) ] ->
    let keys = column_names x in
    List.iter (fun k -> ignore (column span t k)) keys;
    keys
  | _ -> fail span Wrong_arguments "by is given twice"
;;

let options (arguments : arguments) =
  let policy label =
    Option.map
      (fun (x : Syntax.expr) ->
         match x.form with
         | Name (_, n) when List.mem_assoc n C.policies -> List.assoc n C.policies
         | _ -> fail x.span Invalid_policy "A missing policy is skip, keep or error")
      (List.assoc_opt (Some label) arguments)
  in
  ( policy "missing"
  , policy "group_missing"
  , List.filter
      (fun (label, _) -> label <> Some "missing" && label <> Some "group_missing")
      arguments )
;;

let present span (t : C.table) keys =
  let tests =
    List.filter_map
      (fun k ->
         let typ = typ t k in
         if T.is_optional typ
         then
           Some
             (make
                span
                Columnar
                T.Bool
                (Builtin (Present, [ make span Columnar typ (Column k) ])))
         else None)
      keys
  in
  match tests with
  | [] -> make span Scalar T.Bool (Constant (Bool true))
  | first :: rest ->
    List.fold_left (fun a b -> make span Columnar T.Bool (Binary (And, a, b))) first rest
;;

let group_policy context span (t : C.table) ~missing ~group_missing keys =
  if group_missing <> None && keys = []
  then fail span Group_missing_without_by "group_missing needs by =";
  match List.filter (fun k -> T.is_optional (typ t k)) keys with
  | [] -> None
  | first :: _ ->
    (match
       List.find_map
         Fun.id
         [ group_missing; missing; context.group_missing; context.missing ]
     with
     | Some p -> Some p
     | None ->
       fail
         span
         Missing_policy_required
         ("The grouping key "
          ^ first
          ^ " can be missing. Say what to do with those rows: group_missing = skip \
             leaves them out, keep makes missing a group, error stops the run"))
;;

let apply_group_policy span policy (t : C.table) keys =
  match policy with
  | Some C.Skip ->
    let t = add span (Filter (present span t keys)) t in
    { t with
      schema =
        List.map
          (fun (n, typ) -> n, if List.mem n keys then T.present typ else typ)
          t.schema
    }
  | Some Fail ->
    add span (Filter (make span Columnar T.Bool (Require (present span t keys)))) t
  | Some Keep | None -> t
;;

let summarize context span (t : C.table) arguments =
  let missing, group_missing, rest = options arguments in
  let keys = grouping span t rest in
  let policy = group_policy context span t ~missing ~group_missing keys in
  let t = apply_group_policy span policy t keys in
  let measures =
    List.filter_map
      (function
        | Some "by", _ -> None
        | Some n, x ->
          let value = context.expression ?missing (C.row t) x in
          if value.shape <> Scalar
          then fail x.span Scalar_required "A summary needs one value per group";
          Some (n, value)
        | None, x -> fail x.span Wrong_arguments "summarize needs named summaries")
      rest
  in
  let schema =
    List.map (fun k -> k, typ t k) keys
    @ List.map (fun (n, (v : C.expr)) -> n, v.typ) measures
  in
  if not (distinct (List.map fst schema))
  then fail span Duplicate_name "A summary repeats a column name";
  let restricted =
    if List.exists (fun k -> List.mem k t.restricted) keys
    then List.map fst schema
    else List.filter_map (fun (n, v) -> if private_ t v then Some n else None) measures
  in
  let t = add span (Summarize { by = keys; measures }) t in
  { t with
    schema
  ; keys = [ keys ]
  ; order = (if keys = [] then Some [] else None)
  ; restricted
  }
;;

let mutate context span (t : C.table) arguments =
  let missing, group_missing, rest = options arguments in
  let keys = grouping span t rest in
  let policy = group_policy context span t ~missing ~group_missing keys in
  let t =
    match policy with
    | Some Fail -> apply_group_policy span policy t keys
    | _ -> t
  in
  List.fold_left
    (fun (t : C.table) -> function
       | Some "by", _ -> t
       | None, (x : Syntax.expr) ->
         fail x.span Wrong_arguments "mutate needs named columns"
       | Some name, x ->
         let value = context.expression ?missing (C.row t) x in
         let value =
           if policy = Some Skip && C.reduces value
           then
             make
               value.span
               Columnar
               (T.optional value.typ)
               (If
                  ( present span t keys
                  , value
                  , make span Scalar (T.optional value.typ) Absent ))
           else value
         in
         let exists = List.mem_assoc name t.schema in
         let t = if exists then restrict (fun c -> c <> name) t else t in
         let schema =
           if exists
           then List.map (fun (n, typ) -> n, if n = name then value.typ else typ) t.schema
           else t.schema @ [ name, value.typ ]
         in
         let restricted =
           List.filter (( <> ) name) t.restricted
           @ if private_ t value then [ name ] else []
         in
         add x.span (Mutate { name; value; by = keys }) { t with schema; restricted })
    t
    rest
;;

let filter context span (t : C.table) = function
  | (None, (x : Syntax.expr)) :: options ->
    let policy =
      match options with
      | [] -> context.missing
      | [ (Some "missing", p) ] ->
        (match p.form with
         | Name (_, n) when List.mem_assoc n C.policies -> Some (List.assoc n C.policies)
         | _ -> fail p.span Invalid_policy "A missing policy is skip, keep or error")
      | _ ->
        fail span Wrong_arguments "filter takes one condition and an optional missing ="
    in
    let predicate = context.expression ?missing:policy (C.row t) x in
    if predicate.shape <> Columnar
    then
      fail x.span Row_domain_mismatch "filter needs a condition on this table's columns";
    T.unify T.Bool (T.present predicate.typ);
    let constant b = make span Scalar T.Bool (Constant (Bool b)) in
    let predicate =
      match T.is_optional predicate.typ, policy with
      | false, _ -> predicate
      | true, Some Skip ->
        make span Columnar T.Bool (Coalesce (predicate, constant false))
      | true, Some Keep -> make span Columnar T.Bool (Coalesce (predicate, constant true))
      | true, Some Fail -> make span Columnar T.Bool (Require predicate)
      | true, None ->
        fail
          x.span
          Optional_condition
          "This condition is missing where its inputs are missing. Say what happens to \
           those rows: missing = skip drops them, missing = keep keeps them, missing = \
           error stops the run"
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
    let t = add span (Filter predicate) t in
    let t =
      { t with
        schema =
          List.map
            (fun (n, typ) -> n, if List.mem n refined then T.present typ else typ)
            t.schema
      }
    in
    if private_ t predicate then everything t else t
  | _ -> fail span Wrong_arguments "filter takes one condition and an optional missing ="
;;

let arrange context span (t : C.table) arguments =
  if arguments = [] then fail span Wrong_arguments "arrange needs at least one column";
  let keys =
    List.map
      (function
        | None, { form = Call ({ form = Name (Bare, "desc"); _ }, [ (None, x) ]); _ } ->
          context.expression (C.row t) x, C.Descending
        | None, x -> context.expression (C.row t) x, C.Ascending
        | Some _, (x : Syntax.expr) ->
          fail x.span Wrong_arguments "arrange takes columns, not named arguments")
      arguments
  in
  let sorted =
    List.map
      (fun ((x : C.expr), _) ->
         match x.node with
         | Column n -> Some n
         | _ -> None)
      keys
    |> prefix Option.is_some
    |> List.map Option.get
  in
  let t = add span (Arrange keys) { t with order = Some sorted } in
  if List.exists (fun (x, _) -> private_ t x) keys then everything t else t
;;

let rename span (t : C.table) arguments =
  let changes =
    List.map
      (function
        | Some fresh, (x : Syntax.expr) ->
          let old = column_name x in
          ignore (column x.span t old);
          fresh, old
        | None, x -> fail x.span Wrong_arguments "rename takes new = old")
      arguments
  in
  let result = rename_columns changes (add span (Rename changes) t) in
  if (not (distinct (C.names result))) || not (distinct (List.map snd changes))
  then fail span Duplicate_name "rename gives two columns the same name";
  result
;;

let pivot span (t : C.table) wider arguments =
  let argument n =
    match List.assoc_opt (Some n) arguments with
    | Some x -> x
    | None -> fail span Wrong_arguments ("pivot needs " ^ n ^ " =")
  in
  if List.length arguments <> 3
  then fail span Pivot_shape "A pivot takes exactly three named arguments";
  if wider
  then (
    let names_from = column_name (argument "names_from")
    and values_from = column_name (argument "values_from")
    and levels = column_names (argument "levels") in
    if levels = [] || not (distinct levels)
    then fail span Pivot_shape "levels must be distinct and nonempty";
    if T.repr (T.present (column span t names_from)) <> String
    then fail span Pivot_shape "names_from must be a String column";
    let value = T.optional (column span t values_from) in
    let ids = List.filter (fun n -> n <> names_from && n <> values_from) (C.names t) in
    let schema =
      List.map (fun n -> n, typ t n) ids @ List.map (fun l -> l, value) levels
    in
    if not (distinct (List.map fst schema))
    then fail span Pivot_shape "The pivot repeats a column name";
    let hidden = List.mem values_from t.restricted || List.mem names_from t.restricted in
    let t = add span (Pivot_wider { ids; names_from; values_from; levels }) t in
    { t with
      schema
    ; keys = []
    ; order = None
    ; restricted =
        (List.filter (fun n -> List.mem n ids) t.restricted
         @ if hidden then levels else [])
    })
  else (
    let columns = column_names (argument "cols")
    and names_to = column_name (argument "names_to")
    and values_to = column_name (argument "values_to") in
    if columns = [] || not (distinct columns)
    then fail span Pivot_shape "cols must be distinct and nonempty";
    let types = List.map (column span t) columns in
    let element =
      List.fold_left
        (fun acc typ ->
           match T.repr acc, T.repr (T.present typ) with
           | Int, Float | Float, Int -> T.Float
           | _ ->
             (try T.unify acc (T.present typ) with
              | T.Mismatch -> fail span Pivot_shape "Pivoted columns must share a type");
             acc)
        (T.present (List.hd types))
        types
    in
    let value = if List.exists T.is_optional types then T.Option element else element in
    let ids = List.filter (fun n -> not (List.mem n columns)) (C.names t) in
    let schema =
      List.map (fun n -> n, typ t n) ids @ [ names_to, T.String; values_to, value ]
    in
    if not (distinct (List.map fst schema))
    then fail span Pivot_shape "The pivot repeats a column name";
    let hidden = List.exists (fun c -> List.mem c t.restricted) columns in
    let t = add span (Pivot_longer { ids; columns; names_to; values_to }) t in
    { t with
      schema
    ; keys = []
    ; order = None
    ; restricted =
        (List.filter (fun n -> List.mem n ids) t.restricted
         @ if hidden then [ values_to ] else [])
    })
;;

let join context span kind (left : C.table) arguments : C.table =
  let right, options =
    match arguments with
    | (None, x) :: options -> context.table x, options
    | _ -> fail span Wrong_arguments "A join needs the table to join"
  in
  let verb = C.join_name kind ^ "_join" in
  let key, right_key =
    match List.assoc_opt (Some "by") options with
    | Some { form = Name (_, k); _ } -> k, k
    | Some
        { form = Binary (Equal, { form = Name (_, l); _ }, { form = Name (_, r); _ }); _ }
      -> l, r
    | _ -> fail span Join_keys (verb ^ " needs by = key, or by = (left_key == right_key)")
  in
  let covered =
    match List.assoc_opt (Some "unmatched") options, kind with
    | None, _ | Some { form = Name (_, "allow"); _ }, _ -> false
    | Some { form = Name (_, "error"); _ }, (Semi | Anti) ->
      fail span Wrong_arguments ("unmatched = error is not available on " ^ verb)
    | Some { form = Name (_, "error"); _ }, _ -> true
    | Some _, _ -> fail span Wrong_arguments "unmatched is allow or error"
  in
  let declared =
    match List.assoc_opt (Some "relationship") options with
    | Some { form = Name (_, name); _ } when List.mem_assoc name C.relationships ->
      Some (List.assoc name C.relationships)
    | Some _ ->
      fail
        span
        Wrong_arguments
        "relationship is one_to_one, many_to_one, one_to_many or many_to_many"
    | None -> None
  in
  (match
     List.find_opt
       (fun (n, _) -> n <> Some "by" && n <> Some "relationship" && n <> Some "unmatched")
       options
   with
   | Some (Some n, _) -> fail span Unknown_option (verb ^ " takes no option named " ^ n)
   | Some (None, _) -> fail span Wrong_arguments (verb ^ " takes one table")
   | None -> ());
  let right =
    if key = right_key
    then right
    else rename span right [ Some key, { Syntax.form = Name (Bare, right_key); span } ]
  in
  let lk = column span left key
  and rk = column span right key in
  if T.present lk <> T.present rk && T.resolve (T.present lk) <> T.resolve (T.present rk)
  then fail span Join_keys "The join keys have different types";
  let relationship =
    match declared, kind with
    | Some r, _ -> Some r
    | None, (Semi | Anti) -> None
    | None, (Left | Inner) when unique right [ key ] -> Some C.Many_to_one
    | None, Right when unique left [ key ] -> Some One_to_many
    | None, Full when unique left [ key ] && unique right [ key ] -> Some One_to_one
    | None, _ ->
      fail
        span
        Join_relationship
        (verb
         ^ " needs relationship = one_to_one, many_to_one, one_to_many or many_to_many: \
            no key proves how many rows match on "
         ^ key)
  in
  let optional_left = kind = Right || kind = Full
  and optional_right = kind = Left || kind = Full in
  let lift optional typ = if optional then T.optional typ else typ in
  let schema =
    List.map
      (fun (n, typ) ->
         ( n
         , if n = key
           then lift (T.is_optional rk && optional_left) typ
           else lift optional_left typ ))
      left.schema
    @ List.filter_map
        (fun (n, typ) ->
           if n = key || kind = Semi || kind = Anti
           then None
           else (
             if List.mem_assoc n left.schema
             then fail span Join_collision ("Both tables have a column named " ^ n);
             Some (n, lift optional_right typ)))
        right.schema
  in
  let at_most_one_right =
    match relationship with
    | Some (One_to_one | Many_to_one) -> true
    | _ -> kind = Semi || kind = Anti
  and at_most_one_left =
    match relationship with
    | Some (One_to_one | One_to_many) -> true
    | _ -> false
  in
  let keys =
    ((if at_most_one_right && kind <> Right && kind <> Full then left.keys else [])
     @ if at_most_one_left && (kind = Right || kind = Inner) then right.keys else [])
    |> List.filter (List.for_all (fun c -> List.mem_assoc c schema))
  in
  let hidden =
    left.restricted @ List.filter (fun n -> List.mem_assoc n schema) right.restricted
  in
  { source = Join { left; right; key; kind; relationship; covered }
  ; stages = []
  ; schema
  ; rest = None
  ; keys
  ; order = None
  ; restricted = (if List.mem key hidden then List.map fst schema else hidden)
  }
;;

let apply context span name (t : C.table) arguments =
  match name, arguments with
  | "filter", _ -> filter context span t arguments
  | "mutate", _ -> mutate context span t arguments
  | "select", _ -> select span t arguments
  | "rename", _ -> rename span t arguments
  | "summarize", _ -> summarize context span t arguments
  | "count", _ ->
    let at form = { Syntax.form; span } in
    summarize
      context
      span
      t
      [ Some "n", at (Call (at (Name (Bare, "n")), []))
      ; Some "by", at (List (List.map snd arguments))
      ]
  | "arrange", _ -> arrange context span t arguments
  | "slice_head", [ (Some "n", { form = Literal (Int n); _ }) ] ->
    if not (total t)
    then
      fail
        span
        Unordered_slice
        "slice_head needs a total order: arrange by columns that include a key of the \
         table, or establish one with assert_key";
    (match int_of_string_opt n with
     | Some k when k >= 0 -> add span (Take k) t
     | _ -> fail span Wrong_arguments "slice_head needs a nonnegative n")
  | "slice_head", _ -> fail span Wrong_arguments "slice_head takes n = count"
  | "distinct", _ ->
    let projected = if arguments = [] then t else select span t arguments in
    let columns = C.names projected in
    let t = add span (Distinct columns) projected in
    let t = { t with keys = [ columns ]; order = None } in
    if List.exists (fun c -> List.mem c t.restricted) columns then everything t else t
  | "assert_key", _ ->
    let columns =
      List.map
        (function
          | None, (x : Syntax.expr) ->
            let n = column_name x in
            ignore (column x.span t n);
            n
          | Some _, x -> fail x.span Wrong_arguments "assert_key takes column names")
        arguments
    in
    if columns = [] then fail span Wrong_arguments "assert_key needs at least one column";
    add span (Assert_key columns) { t with keys = columns :: t.keys }
  | "pivot_longer", _ -> pivot span t false arguments
  | "pivot_wider", _ -> pivot span t true arguments
  | name, _ when List.mem_assoc name C.join_kinds ->
    join context span (List.assoc name C.join_kinds) t arguments
  | _ -> fail span Unknown_verb ("Unknown table verb " ^ name)
;;
