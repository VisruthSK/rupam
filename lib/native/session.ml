type accepted =
  { source : Syntax.item
  ; items : TransformIR.item list
  }

type revision =
  { number : int
  ; env : Scope.env
  ; accepted : accepted list
  }

type host =
  { worker : Worker.t
  ; names : Host.names
  ; established : (int, unit) Hashtbl.t
  ; mutable target : Target.t
  }

type result =
  { table : TransformIR.table
  ; file : string
  ; arrow : Arrow_result.t
  ; ordering : string
  }

type t =
  { directory : string
  ; mutable project : Project.t
  ; mutable path : string option
  ; mutable current : revision
  ; mutable past : revision list
  ; mutable future : revision list
  ; mutable host : host option
  ; mutable saved : int option
  ; mutable results : (string * result) list
  ; mutable counter : int
  }

type draft =
  { preview_session : t
  ; active_worker : Worker.t option Atomic.t
  ; cancelled : bool Atomic.t
  }

type event =
  | Bound of string * string
  | Output of string
  | Value of string * Types.t
  | Table of Page.t
  | Viewed of Syntax.expr * result
  | Schema of int * (string * Types.t) list
  | Typed of Types.t
  | Histogram of string * Arrow_result.summary
  | Bars of string * Arrow_result.frequencies
  | Inspection_failed of Diagnostic.t

let history_limit = 100
let result_limit = 8
let remove path = if Sys.file_exists path then Sys.remove path

let discard r =
  Arrow_result.close r.arrow;
  remove r.file
;;

let create ~project ~directory =
  { directory
  ; project
  ; path = None
  ; current =
      { number = 0
      ; env = Scope.empty ~inputs:Native.inputs ~directory ~strict:project.Project.strict.value ()
      ; accepted = []
      }
  ; past = []
  ; future = []
  ; host = None
  ; saved = None
  ; results = []
  ; counter = 0
  }
;;

let env t = t.current.env
let directory t = t.directory
let project t = t.project
let path t = t.path
let revision t = t.current.number
let items t = List.concat_map (fun a -> a.items) t.current.accepted
let target t = t.project.target.value

let close t =
  Option.iter (fun h -> Worker.stop h.worker) t.host;
  List.iter (fun (_, r) -> discard r) t.results;
  t.results <- []
;;

let stop t =
  close t;
  t.host <- None
;;

let host ?(started = fun _ -> ()) t =
  let target = Project.require_target t.project in
  match t.host with
  | Some h when h.target.language = target.language ->
    started h.worker;
    if h.target <> target
    then (
      let module B = (val Backend.of_target target) in
      ignore (Worker.evaluate h.worker B.preamble);
      h.target <- target);
    h
  | _ ->
    stop t;
    let worker =
      Worker.start (Runtime.resolve t.project target.language) target.language
    in
    let module B = (val Backend.of_target target) in
    let h =
      { worker
      ; names = B.names ()
      ; established = Hashtbl.create 64
      ; target
      }
    in
    t.host <- Some h;
    started worker;
    ignore (Worker.evaluate worker B.preamble);
    h
;;

let run h items =
  match items with
  | [] -> ""
  | _ ->
    let module B = (val Backend.of_target h.target) in
    Worker.evaluate h.worker (B.statements h.names items)
;;

let defined = function
  | TransformIR.Define (id, _) -> Some id.TransformIR.stamp
  | _ -> None
;;

let establish ?started t =
  let h = host ?started t in
  let pending =
    List.filter
      (fun item ->
         match item with
         | TransformIR.Define (id, _) -> not (Hashtbl.mem h.established id.stamp)
         | _ -> false)
      (items t)
  in
  ignore (run h pending);
  List.iter
    (fun i -> Option.iter (fun s -> Hashtbl.replace h.established s ()) (defined i))
    pending;
  h
;;

let commit t revision =
  t.past <- List.filteri (fun i _ -> i < history_limit) (t.current :: t.past);
  t.future <- [];
  t.current <- revision
;;

let ordering t (table : TransformIR.table) =
  let open TransformIR in
  let rec directions table =
    match
      List.find_map
        (function
          | { step = Arrange keys; _ } -> Some (List.map snd keys)
          | _ -> None)
        (List.rev table.stages)
    with
    | Some keys -> keys
    | None ->
      (match table.source with
       | Bound source ->
         List.find_map
           (function
             | Define (id, { node = Table table; _ }) when id = source ->
               Some (directions table)
             | _ -> None)
           (items t)
         |> Option.value ~default:[]
       | Read _ | Load _ | Join _ -> [])
  in
  match table.order with
  | None -> "unordered"
  | Some names ->
    let order = directions table in
    let keys =
      List.map2
        (fun name direction ->
           if List.mem name table.restricted
           then None
           else
             Some
               (Identifier.quote name
                ^
                match direction with
                | Ascending -> " asc"
                | Descending -> " desc"))
        (List.filteri (fun i _ -> i < List.length order) names)
        (List.filteri (fun i _ -> i < List.length names) order)
      |> List.filter_map Fun.id
    in
    (match keys with
     | [] -> "ordered"
     | _ -> "ordered by (" ^ String.concat ", " keys ^ ")")
;;


let materialize h ~ordering table =
  let file = Filename.temp_file "rupam" ".arrow" in
  match
    let module B = (val Backend.of_target h.target) in
    ignore (Worker.evaluate h.worker (B.result h.names table ~path:file));
    { table; file; arrow = Arrow_result.open_ipc file; ordering }
  with
  | result -> result
  | exception exn ->
    remove file;
    raise exn
;;

let result t (table : TransformIR.table) =
  let h = establish t in
  let key =
    Target.name h.target ^ Sexplib0.Sexp.to_string (TransformIR.sexp_of_table table)
    |> Digest.string
    |> Digest.to_hex
  in
  match List.assoc_opt key t.results with
  | Some r -> r
  | None ->
    let r = materialize h ~ordering:(ordering t table) table in
    List.iteri (fun i (_, old) -> if i >= result_limit - 1 then discard old) t.results;
    t.results <- (key, r) :: List.filteri (fun i _ -> i < result_limit - 1) t.results;
    r
;;

let column_index r name =
  let names = Arrow_result.names r.arrow in
  let rec find i =
    if i >= Array.length names
    then Diagnostic.fail Unknown_column ("!hist has no column " ^ name)
    else if names.(i) = name
    then i
    else find (i + 1)
  in
  find 0
;;

let ordering_of r = r.ordering

let page ?columns r ~offset ~limit =
  Page.of_arrow
    ?columns
    r.arrow
    ~ordering:r.ordering
    ~offset
    ~limit
    r.table.schema
;;

let column_summary r name =
  if List.mem name r.table.restricted
  then Diagnostic.fail Restricted_output "This column is restricted";
  let column = column_index r name in
  let arrow = Arrow_result.open_ipc r.file in
  Fun.protect
    ~finally:(fun () -> Arrow_result.close arrow)
    (fun () ->
       match Types.repr (Types.present (List.assoc name r.table.schema)) with
       | Float -> Arrow_result.Numeric (Arrow_result.summary arrow ~column)
       | Int ->
         let summary = Arrow_result.summary arrow ~column in
         let counts = Arrow_result.frequencies arrow ~column in
         Numeric { summary with distinct = counts.distinct }
       | String | Bool | Variant _ -> Frequencies (Arrow_result.frequencies arrow ~column)
       | _ -> Diagnostic.fail Unsupported_column_type "This column has no scalar summary")
;;

let bins (summary : Arrow_result.summary) =
  let h = summary.histogram in
  let present = Array.fold_left ( + ) 0 h.counts in
  let range = h.high -. h.low in
  if present < 2 || range <= 0.
  then 1
  else (
    let n = float_of_int present in
    let sturges = int_of_float (Float.ceil (Float.log2 n)) + 1 in
    let freedman =
      match summary.q1, summary.q3 with
      | Some q1, Some q3 when q3 > q1 ->
        int_of_float (Float.ceil (range /. (2. *. (q3 -. q1) /. Float.cbrt n)))
      | _ -> 0
    in
    max 1 (min 200 (max sturges freedman)))
;;

let histogram r name =
  let column = column_index r name in
  let summary = Arrow_result.summary r.arrow ~column in
  { summary with
    histogram = Arrow_result.histogram r.arrow ~column ~bins:(bins summary)
  }
;;

let cell r ~row name =
  if List.mem name r.table.restricted
  then Diagnostic.fail Restricted_output "This column is restricted";
  let typ = List.assoc name r.table.schema in
  ( typ
  , match Types.repr (Types.present typ), Arrow_result.value r.arrow row (column_index r name) with
    | Date, Int days -> Arrow_result.Date (Int64.to_int days)
    | _, value -> value )
;;

let total r = Arrow_result.rows r.arrow
let table_of_result r = r.table
let preview t table ~offset ~limit = page (result t table) ~offset ~limit

let describe env (d : Syntax.declaration) =
  match d with
  | Let (name, _, _) ->
    (match Scope.lookup env name with
     | Some (Table_value _) | None -> []
     | Some b ->
       Option.fold ~none:[] ~some:(fun s -> [ Bound (name, s) ]) (Scope.describe b))
  | _ -> []
;;

let declare t (env, accepted, events) (source : Syntax.item) =
  let d = source.declaration in
  let env, items = Elaborate.item env source in
  let effects =
    List.filter
      (function
        | TransformIR.Define _ | Write _ -> true
        | Show _ | Variant _ -> false)
      items
  in
  let deferred =
    target t = None
    && List.for_all
         (function
           | TransformIR.Define _ -> true
           | _ -> false)
         effects
  in
  let events =
    if effects = [] || deferred
    then events
    else (
      let h = establish t in
      let output = run h effects in
      List.iter
        (fun i -> Option.iter (fun s -> Hashtbl.replace h.established s ()) (defined i))
        effects;
      if output = "" then events else Output output :: events)
  in
  let events = List.rev_append (describe env d) events in
  let events =
    match d, deferred with
    | Let (name, _, _), false ->
      (match Scope.lookup env name with
       | Some (Table_value { table; _ }) ->
         Schema (total (result t table), table.schema) :: events
       | _ -> events)
    | _ -> events
  in
  let events =
    List.fold_left
      (fun events (item : TransformIR.item) ->
         match item with
         | Show { node = Table table; _ } ->
           Table (preview t table ~offset:0 ~limit:20) :: events
         | Show e ->
           let h = establish t in
           Value (String.trim (run h [ item ]), e.typ) :: events
         | Define _ | Write _ | Variant _ -> events)
      events
      items
  in
  let accepted =
    match d with
    | Evaluate _
      when not
             (List.exists
                (function
                  | TransformIR.Write _ -> true
                  | _ -> false)
                items) -> accepted
    | _ -> { source; items } :: accepted
  in
  env, accepted, events
;;

let inspect t env (tap : Tap.t) =
  let _, shown =
    Elaborate.item env { declaration = Evaluate tap.subject; span = tap.subject.span }
  in
  match tap.kind, shown, tap.arguments with
  | Print, [ Show { node = Table table; _ } ], [] ->
    Table (preview t table ~offset:0 ~limit:20)
  | Print, [ (Show e as item) ], [] ->
    Value (String.trim (run (establish t) [ item ]), e.typ)
  | View, [ Show { node = Table table; _ } ], [] -> Viewed (tap.subject, result t table)
  | Schema, [ Show { node = Table table; _ } ], [] ->
    Schema (total (result t table), table.schema)
  | Schema, [ Show e ], [] -> Typed e.typ
  | Hist, [ Show { node = Table table; _ } ], [ (None, { form = Name (_, column); _ }) ]
    ->
    let r = result t table in
    Histogram (column, histogram r column)
  | Bar, [ Show { node = Table table; _ } ], [ (None, { form = Name (_, column); _ }) ] ->
    let r = result t table in
    Bars (column, Arrow_result.frequencies r.arrow ~column:(column_index r column))
  | (View | Hist | Bar), _, _ ->
    Diagnostic.fail
      ~span:tap.subject.span
      Not_a_table
      "!view, !hist(column) and !bar(column) inspect a table"
  | _ ->
    Diagnostic.fail
      ~span:tap.subject.span
      Wrong_arguments
      "Unexpected arguments to an inspection"
;;

let step t (env, accepted, events) item =
  let item, taps = Tap.split item in
  let observe tap =
    match inspect t env tap with
    | event -> event
    | exception exn -> Inspection_failed (Diagnostic.of_exn exn)
  in
  let events = List.rev_append (List.map observe taps) events in
  match item with
  | Some item -> declare t (env, accepted, events) item
  | None -> env, accepted, events
;;

let submit t declarations =
  let env, accepted, events =
    List.fold_left (step t) (t.current.env, List.rev t.current.accepted, []) declarations
  in
  t.counter <- t.counter + 1;
  commit t { number = t.counter; env; accepted = List.rev accepted };
  List.rev events
;;

let load t path =
  ignore
    (submit
       t
       (Frontend.read ~file:path (In_channel.with_open_text path In_channel.input_all)));
  t.path <- Some path;
  t.saved <- Some t.current.number
;;

let check t (i : Syntax.item) = snd (Elaborate.item t.current.env i)

let table_of t (e : Syntax.expr) =
  match check t { declaration = Evaluate e; span = e.span } with
  | [ Show { node = Table table; _ } ] -> table
  | _ -> Diagnostic.fail ~span:e.span Not_a_table "Expected a table expression"
;;

let table t source =
  match Frontend.read source with
  | [ ({ declaration = Evaluate _; _ } as i) ] ->
    (match check t i with
     | [ Show { node = Table table; _ } ] -> table
     | _ -> Diagnostic.fail Not_a_table "Expected a table expression")
  | _ -> Diagnostic.fail Not_a_table "Expected a table expression"
;;

let open_draft t =
  { preview_session = { t with host = None; results = []; past = []; future = [] }
  ; active_worker = Atomic.make None
  ; cancelled = Atomic.make false
  }
;;

let cancel_draft draft =
  Atomic.set draft.cancelled true;
  Option.iter Worker.cancel (Atomic.get draft.active_worker)
;;

let close_draft draft = stop draft.preview_session

let reset_cancelled_draft draft =
  if Atomic.get draft.cancelled
  then (
    close_draft draft;
    Atomic.set draft.active_worker None;
    Atomic.set draft.cancelled false)
;;

let preview_draft draft source =
  if Atomic.get draft.cancelled then raise Sys.Break;
  let t = draft.preview_session in
  let started worker =
    Atomic.set draft.active_worker (Some worker);
    if Atomic.get draft.cancelled
    then (
      Worker.cancel worker;
      raise Sys.Break)
  in
  let env = { t.current.env with supply = ref !(t.current.env.supply) } in
  let _, expr = Frontend.table_draft source in
  match snd (Elaborate.item env { declaration = Evaluate expr; span = expr.span }) with
  | [ Show { node = Table table; _ } ] ->
    materialize (establish ~started t) ~ordering:(ordering t table) table
  | _ -> Diagnostic.fail ~span:expr.span Not_a_table "Expected a table expression"
;;

let release_draft = discard

type code =
  | Definition
  | Full
  | Expression

let code t ?(mode = Definition) target source =
  let target = Option.value target ~default:(Project.require_target t.project) in
  let module B = (val Backend.of_target target) in
  let names =
    match t.host with
    | Some h when h.target.language = target.language -> h.names
    | _ -> B.names ()
  in
  let items = items t in
  let defined name =
    List.find_index
      (function
        | TransformIR.Define (id, _) -> id.name = name
        | _ -> false)
      (List.rev items)
    |> Option.map (fun i -> List.length items - 1 - i)
  in
  match mode, defined (String.trim source) with
  | Definition, Some i -> B.statements names [ List.nth items i ]
  | Full, Some i -> fst (B.script (List.filteri (fun j _ -> j <= i) items))
  | (Definition | Full | Expression), _ -> B.table names (table t source)
;;

let set_target t target ~origin =
  t.project
  <- { t.project with target = { value = Some target; origin = Project.Session origin } }
;;

let undo t =
  match t.past with
  | [] -> false
  | previous :: rest ->
    t.future <- t.current :: t.future;
    t.past <- rest;
    t.current <- previous;
    true
;;

let redo t =
  match t.future with
  | [] -> false
  | next :: rest ->
    t.past <- t.current :: t.past;
    t.future <- rest;
    t.current <- next;
    true
;;

let source t = Format.program (List.map (fun a -> a.source) t.current.accepted)

let save t path =
  Out_channel.with_open_text path (fun ch -> output_string ch (source t ^ "\n"));
  t.path <- Some path;
  t.saved <- Some t.current.number
;;

let lock t =
  match t.path, t.saved with
  | Some source, Some saved when saved = t.current.number ->
    Lock.establish ~project:t.project ~source (items t)
  | _ -> Diagnostic.fail Unsaved_analysis "Save the analysis with :save before locking it"
;;

let unsaved t = t.saved <> Some t.current.number
