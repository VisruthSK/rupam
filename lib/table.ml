open Shape

type step =
  | Arrange of (expr * bool) list
  | Summarize of (string * expr) list * (string * expr option) list
  | Take of int
  | Distinct of expr list
  | Filter of expr
  | Bind of string * expr
  | Select of string list

type t = {
  context : int;
  schema : (string * expr) list;
  steps : step list;
  next : int;
  ordered : bool;
}

let source context fields =
  let schema = List.mapi (fun id (name, primitive, nullable) ->
    name, input id { primitive; nullable; shape = Column context }) fields in
  { context; schema; steps = []; ordered = false; next = List.length fields }

let names table = List.map fst table.schema

let select table arguments =
  let name = function
    | { Syntax.form = Name ((Bare | Column), n); _ } -> n
    | _ -> raise (Error "Expected named column selector") in
  let resolve x =
    let n = name x in
    ignore (Check.lookup n table.schema); n in
  let fields = names table in
  let range first last =
    let rec from = function
      | [] -> raise (Error "Invalid schema range")
      | n :: rest -> if n = first then through (n :: rest) else from rest
    and through = function
      | [] -> raise (Error "Range ends before its start")
      | n :: rest -> if n = last then [n] else n :: through rest in
    from fields in
  let selected = List.concat_map (function
    | None, { Syntax.form = Binary (":", a, b); _ } -> range (resolve a) (resolve b)
    | None, x -> [resolve x]
    | _ -> raise (Error "select expects unnamed selectors")) arguments in
  if List.length selected <> List.length (List.sort_uniq String.compare selected) then
    raise (Error "Duplicate selected column");
  { table with schema = List.map (fun n -> n, List.assoc n table.schema) selected;
    steps = Select selected :: table.steps }

let stage env table verb arguments = match verb, arguments with
  | "take", [None,{Syntax.form=Literal n;_}] ->
      if not table.ordered then raise (Error "take requires arrange with explicit ordering keys");
      let count = match int_of_string_opt n with Some i when i >= 0 -> i | _ -> raise (Error "Invalid take count") in
      {table with steps=Take count :: table.steps}
  | "distinct", _ ->
      let projected = if arguments=[] then table else select table arguments in
      {projected with steps=Distinct (List.map snd projected.schema)::projected.steps; ordered=false}
  | "arrange", [] -> raise (Error "arrange requires at least one ordering key")
  | "arrange", _ ->
      let key = function
        | None, { Syntax.form = Call ({form = Name (Bare, "desc"); _}, [None, x]); _ } ->
            Check.expression env table.schema x, true
        | None, x -> Check.expression env table.schema x, false
        | _ -> raise (Error "Expected unnamed order key") in
      { table with ordered=true; steps = Arrange (List.map key arguments) :: table.steps }
  | "summarize", _ ->
      let groups, values = List.partition (fun (label, _) -> label = Some "by") arguments in
      let keys = List.map (function
        | _, {Syntax.form = Name ((Bare | Column), n); _} -> n, Check.lookup n table.schema
        | _ -> raise (Error "Expected named grouping column")) groups in
      let measures = List.map (function
        | Some n, {Syntax.form = Call ({form = Name (Bare, "count"); _}, []); _} -> n,None
        | Some n, x ->
            let value = Check.expression env table.schema x in
            if value.typ.shape <> Scalar then raise (Error "Summary must yield a scalar");
            n,Some value
        | _ -> raise (Error "Summary requires named expressions")) values in
      let fields = List.map (fun (n,x) -> n,x.typ) keys @ List.map (fun (n,x) ->
        n, Option.fold ~none:{primitive=Int64; nullable=false; shape=Scalar} ~some:(fun e -> e.typ) x) measures in
      let labels = List.map fst fields in
      if List.length labels <> List.length (List.sort_uniq String.compare labels) then raise (Error "Duplicate summary label");
      let schema = List.mapi (fun i (n,t) -> n,input (table.next + i) {t with shape=Column table.context}) fields in
      {table with schema; ordered=false; next=table.next + List.length schema; steps=Summarize (keys,measures)::table.steps}
  | "select", _ -> select table arguments
  | "filter", [None, x] ->
      let predicate = Check.expression env table.schema x in
      Shape.filter table.context predicate;
      { table with steps = Filter predicate :: table.steps }
  | ("derive" | "replace"), _ ->
      List.fold_left (fun table (name, x) ->
        let n = match name with Some n -> n | None -> raise (Error "Expected named binding") in
        let exists = List.mem_assoc n table.schema in
        if exists && verb = "derive" then raise (Error ("Column " ^ n ^ " exists. Use replace."));
        if not exists && verb = "replace" then raise (Error ("Unknown column " ^ n));
        let value = broadcast (Column table.context) (Check.expression env table.schema x) in
        let field = n, input table.next value.typ in
        let schema = if exists then List.map (fun ((k, _) as f) ->
          if k = n then field else f) table.schema else table.schema @ [field] in
        { table with schema; next = table.next + 1; steps = Bind (n,value) :: table.steps }
      ) table arguments
  | _ -> raise (Error ("Unsupported table operation: " ^ verb))

let execute plan ~rows inputs =
  let open Column_eval in
  let buffers = Array.make plan.next [||] in
  List.iteri (fun i values ->
    if Array.length values <> rows then raise (Error "Source column length mismatch");
    buffers.(i) <- values) inputs;
  let size = ref rows and next = ref (List.length inputs) in
  let eval x = match evaluate ~rows:!size ~input:(fun i -> Many buffers.(i)) x with
    | Many vs -> vs | One _ -> raise (Error "Expected column expression") in
  let reorder indices =
    for i = 0 to !next - 1 do
      if Array.length buffers.(i) = !size then buffers.(i) <- Array.map (Array.get buffers.(i)) indices
    done;
    size := Array.length indices in
  let apply = function
    | Take n -> reorder (Array.init (min !size n) Fun.id)
    | Distinct keys ->
        let module Seen = Set.Make(struct type t = value list let compare = compare end) in
        let seen = ref Seen.empty in
        let vs = List.map eval keys in
        let rows = Seq.ints 0 |> Seq.take !size |> Seq.filter (fun i ->
          let key = List.map (fun a -> a.(i)) vs in
          if Seen.mem key !seen then false else (seen := Seen.add key !seen; true)) in
        reorder (Array.of_seq rows)
    | Arrange keys ->
        let keys = List.map (fun (x,d) -> eval x,d) keys in
        let order = Array.init !size Fun.id in
        let rec compare_keys i j = function
          | [] -> 0
          | (vs,descending)::rest ->
              let c = match vs.(i),vs.(j) with
                | Missing,Missing -> 0 | Missing,_ -> 1 | _,Missing -> -1
                | a,b -> if descending then compare b a else compare a b in
              if c = 0 then compare_keys i j rest else c in
        Array.stable_sort (fun i j -> compare_keys i j keys) order;
        reorder order
    | Summarize (keys,measures) ->
        let key_values = List.map (fun (_,x) -> eval x) keys in
        let module Groups = Map.Make(struct type t = value list let compare = compare end) in
        let groups = ref (if keys = [] then Groups.singleton [] [] else Groups.empty) in
        for i = 0 to !size - 1 do
          let key = List.map (fun a -> a.(i)) key_values in
          groups := Groups.update key (fun rows -> Some (i :: Option.value rows ~default:[])) !groups
        done;
        let total = Groups.cardinal !groups in
        let outputs = Array.init (List.length keys + List.length measures) (fun _ -> Array.make total Missing) in
        Groups.bindings !groups |> List.iteri (fun row (labels,indices) ->
          let indices = Array.of_list (List.rev indices) in
          let measure = function
            | _,None -> I (Int64.of_int (Array.length indices))
            | _,Some x ->
                let input i = Many (Array.map (Array.get buffers.(i)) indices) in
                match evaluate ~rows:(Array.length indices) ~input x with One v -> v | _ -> assert false in
          List.iteri (fun col v -> outputs.(col).(row) <- v) (labels @ List.map measure measures));
        Array.iter (fun a -> buffers.(!next) <- a; incr next) outputs;
        size := total
    | Select _ -> ()
    | Bind (_, x) -> buffers.(!next) <- eval x; incr next
    | Filter x ->
        let keep = Array.map (function B b -> b | _ -> raise (Error "Expected Bool")) (eval x) in
        let indices = Array.to_seqi keep |> Seq.filter_map (fun (i, b) -> if b then Some i else None)
          |> Array.of_seq in
        reorder indices in
  List.iter apply (List.rev plan.steps);
  !size, List.map (fun (name, x) -> name, eval x) plan.schema

let rec pipeline env tables x = match x.Syntax.form with
  | Syntax.Name (Bare, n) -> Check.lookup n tables
  | Binary ("|>", base, { form = Call ({ form = Name (Bare, verb); _ }, args); _ }) ->
      stage env (pipeline env tables base) verb args
  | _ -> raise (Error "Expected a table pipeline")



