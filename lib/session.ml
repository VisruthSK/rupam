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
  ; id : string
  ; total : int
  ; session : t
  }

and t =
  { directory : string
  ; mutable project : Project.t
  ; mutable path : string option
  ; mutable current : revision
  ; mutable past : revision list
  ; mutable future : revision list
  ; mutable host : host option
  ; mutable saved : int option
  ; mutable results : (string * result) list
  ; mutable serial : int
  ; mutable counter : int
  }

type event =
  | Bound of string * string
  | Output of string
  | Value of string * Types.t
  | Table of Page.t

let history_limit = 100
let result_limit = 8
let remove path = if Sys.file_exists path then Sys.remove path

let create ~project ~directory =
  { directory
  ; project
  ; path = None
  ; current =
      { number = 0
      ; env = Scope.empty ~directory ~strict:project.Project.strict.value
      ; accepted = []
      }
  ; past = []
  ; future = []
  ; host = None
  ; saved = None
  ; results = []
  ; serial = 0
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
  t.results <- []
;;

let stop t =
  close t;
  t.host <- None
;;

let host t =
  let target = Project.require_target t.project in
  match t.host with
  | Some h when h.target.language = target.language ->
    if h.target <> target
    then (
      ignore (Worker.evaluate h.worker (Dialect.preamble target));
      h.target <- target);
    h
  | _ ->
    stop t;
    let worker =
      Worker.start (Runtime.resolve t.project target.language) target.language
    in
    let h =
      { worker
      ; names = Dialect.names target.language
      ; established = Hashtbl.create 64
      ; target
      }
    in
    t.host <- Some h;
    ignore (Worker.evaluate worker (Dialect.preamble target));
    h
;;

let run h items =
  match items with
  | [] -> ""
  | _ -> Worker.evaluate h.worker (Dialect.statements h.target h.names items)
;;

let defined = function
  | TransformIR.Define (id, _) -> Some id.TransformIR.stamp
  | _ -> None
;;

let establish t =
  let h = host t in
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

let release t (r : result) =
  match t.host with
  | Some h -> ignore (Worker.evaluate h.worker (Dialect.release h.target ~id:r.id))
  | None -> ()
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
    t.serial <- t.serial + 1;
    let id = Printf.sprintf "result_%d" t.serial in
    let total = Worker.evaluate h.worker (Dialect.result h.target h.names table ~id) in
    let r = { table; id; total = int_of_string (String.trim total); session = t } in
    let kept = List.filteri (fun i _ -> i < result_limit - 1) t.results in
    List.iteri (fun i (_, old) -> if i >= result_limit - 1 then release t old) t.results;
    t.results <- (key, r) :: kept;
    r
;;

let page r ~offset ~limit =
  let t = r.session in
  if not (List.exists (fun (_, x) -> x == r) t.results)
  then Diagnostic.fail Unreadable_input "This result was released; evaluate it again";
  let h = establish t in
  let path = Filename.temp_file "rupam" ".parquet" in
  Fun.protect
    ~finally:(fun () -> remove path)
    (fun () ->
       ignore
         (Worker.evaluate h.worker (Dialect.page h.target ~id:r.id ~offset ~limit ~path));
       Page.read
         ~path
         ~total:r.total
         ~ordered:(r.table.order <> None)
         ~offset
         ~limit
         r.table.schema)
;;

let total r = r.total
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

let submit t declarations =
  let env, accepted, events =
    List.fold_left
      (declare t)
      (t.current.env, List.rev t.current.accepted, [])
      declarations
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

let table t source =
  match Frontend.read source with
  | [ ({ declaration = Evaluate _; _ } as i) ] ->
    (match check t i with
     | [ Show { node = Table table; _ } ] -> table
     | _ -> Diagnostic.fail Not_a_table "Expected a table expression")
  | _ -> Diagnostic.fail Not_a_table "Expected a table expression"
;;

let code t target table =
  let target = Option.value target ~default:(Project.require_target t.project) in
  let names =
    match t.host with
    | Some h when h.target.language = target.language -> h.names
    | _ -> Dialect.names target.language
  in
  Dialect.table target names table
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
