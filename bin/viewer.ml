open Rupam

type move =
  | Rows of int
  | Columns of int
  | Pan_rows of int
  | Pan_columns of int
  | Cell of int * int
  | Top
  | Bottom

type action =
  | Sort of TransformIR.direction
  | Keep_value
  | Undo
  | Distribution
  | Missing_policy of TransformIR.policy

type t =
  { text : string
  ; generation : int
  ; displayed : int
  ; settled : int
  ; edited_at : float
  ; base : Session.result
  ; context : Session.draft
  ; result : Session.result
  ; names : string array
  ; pinned : string list
  ; row : int
  ; column : int
  ; top : int
  ; left : int
  ; width : int
  ; height : int
  ; window : Page.t
  ; history : string list
  ; distribution : (string * Arrow_result.profile) option
  ; profiles : (string * Arrow_result.profile) list
  ; policy : Syntax.expr option
  ; diagnostic : Diagnostic.t option
  ; binding : string
  }

let source t = t.text
let panel_height = 12
let grid_width t = t.width
let rows t = max 1 (t.height - panel_height - 6)
let index_width t = String.length (string_of_int (Session.total t.result))
let capacity t = max 1 ((grid_width t - index_width t - 5) / 16)
let cell_width t = max 1 (((grid_width t - index_width t - 5) / capacity t) - 1)
let current t = if Array.length t.names = 0 then None else Some t.names.(t.column)
let clamp high n = max 0 (min high n)
let take n xs = List.filteri (fun i _ -> i < n) xs
let fixed_count t = min (capacity t - 1) (List.length t.pinned)

let column_window t =
  let fixed = fixed_count t in
  let start = max fixed t.left in
  let count = min (capacity t - fixed) (Array.length t.names - start) in
  fixed, start, count
;;

let visible_names t =
  let fixed, start, count = column_window t in
  List.init fixed (Array.get t.names) @ List.init count (fun i -> t.names.(start + i))
;;

let frozen t =
  let count = fixed_count t in
  if count > 0 && count < Array.length t.names then Some count else None
;;

let align t =
  let last = max 0 (Session.total t.result - 1) in
  let row = clamp last t.row in
  let column = clamp (Array.length t.names - 1) t.column in
  let top = clamp (max 0 (last + 1 - rows t)) t.top in
  let top = if row < top then row else max top (row - rows t + 1) in
  let fixed = fixed_count t in
  let moving = capacity t - fixed in
  let left = clamp (Array.length t.names - moving) t.left in
  let t = { t with row; column; top; left } in
  if column < fixed
  then t
  else (
    let start = max fixed left in
    let left = if column < start then column else max start (column - moving + 1) in
    { t with left })
;;

let refresh ?(force = false) t =
  let t = align t in
  let columns = visible_names t in
  let limit = rows t in
  let length =
    match t.window.columns with
    | c :: _ -> Array.length c.cells
    | [] -> 0
  in
  if
    (not force)
    && columns = List.map (fun (c : Page.column) -> c.name) t.window.columns
    && t.top >= t.window.offset
    && (t.top + limit <= t.window.offset + length
        || t.window.offset + length >= Session.total t.result)
  then t
  else (
    let offset = max 0 (t.top - limit) in
    { t with window = Session.page t.result ~columns ~offset ~limit:(3 * limit) })
;;

let layout result =
  let table = Session.table_of_result result in
  let names =
    List.filter
      (fun name -> not (List.mem name table.restricted))
      (TransformIR.names table)
  in
  let pinned =
    match table.keys with
    | key :: _ -> List.filter (fun name -> List.mem name names) key
    | [] -> []
  in
  let names = pinned @ List.filter (fun name -> not (List.mem name pinned)) names in
  Array.of_list names, pinned
;;

let open_ session base result ~width ~height =
  let names, pinned = layout result in
  let rec binding i =
    let name = if i = 1 then "view_result" else "view_result_" ^ string_of_int i in
    match Scope.lookup (Session.env session) name with
    | None -> name
    | Some _ -> binding (i + 1)
  in
  refresh
    ~force:true
    { text = String.trim (Format.expression base)
    ; generation = 0
    ; displayed = 0
    ; settled = 0
    ; edited_at = 0.
    ; base = result
    ; context = Session.open_draft session
    ; result
    ; names
    ; pinned
    ; row = 0
    ; column = 0
    ; top = 0
    ; left = 0
    ; width
    ; height
    ; window = Session.page result ~columns:[] ~offset:0 ~limit:0
    ; history = []
    ; distribution = None
    ; profiles = []
    ; policy = None
    ; diagnostic = None
    ; binding = binding 1
    }
;;

let resize t ~width ~height = refresh { t with width; height }

let move t step =
  refresh
    (match step with
     | Rows n -> { t with row = t.row + n }
     | Columns n -> { t with column = t.column + n; distribution = None; policy = None }
     | Pan_rows n ->
       let top = clamp (Session.total t.result - rows t) (t.top + n) in
       { t with top; row = max top (min t.row (top + rows t - 1)); policy = None }
     | Pan_columns n ->
       let fixed, _, _ = column_window t in
       let width = capacity t - fixed in
       let left = max fixed (clamp (Array.length t.names - width) (t.left + n)) in
       let column =
         if t.column < fixed then t.column else max left (min t.column (left + width - 1))
       in
       { t with left; column; policy = None; distribution = None }
     | Cell (row, index) ->
       let fixed, start, count = column_window t in
       if index < 0 || index >= fixed + count
       then t
       else (
         let column = if index < fixed then index else start + index - fixed in
         { t with row; column; policy = None; distribution = None })
     | Top -> { t with row = 0 }
     | Bottom -> { t with row = Session.total t.result - 1 })
;;

let release t = if t.result != t.base then Session.release_draft t.result

let close t =
  release t;
  Session.close_draft t.context
;;

let publish t generation = function
  | Error diagnostic -> { t with settled = generation; diagnostic = Some diagnostic }
  | Ok result ->
    let names, pinned = layout result in
    let column =
      match current t with
      | Some name ->
        Option.value (Array.find_index (String.equal name) names) ~default:t.column
      | None -> t.column
    in
    let next =
      refresh
        ~force:true
        { t with
          result
        ; names
        ; pinned
        ; column
        ; displayed = generation
        ; settled = generation
        ; diagnostic = None
        ; distribution = None
        ; profiles = []
        }
    in
    release t;
    next
;;

let edit t text =
  if text = source t
  then t
  else
    { t with
      text
    ; generation = t.generation + 1
    ; edited_at = Unix.gettimeofday ()
    ; policy = None
    }
;;

let replace t (expression : Syntax.expr) =
  let binding, _ = Frontend.table_draft (source t) in
  let text =
    match binding with
    | None -> String.trim (Format.expression expression)
    | Some name ->
      Format.program
        [ { declaration = Let (name, None, expression); span = expression.span } ]
  in
  let next = edit t text in
  if next.generation = t.generation
  then t
  else { next with history = source t :: t.history }
;;

let node form = { Syntax.form; span = Lexing.dummy_pos, Lexing.dummy_pos }
let name n = node (Syntax.Name (Bare, n))
let column n = node (Syntax.Name (Column, n))
let call f args = node (Syntax.Call (name f, args))
let arg value = None, value
let pipe left right = node (Syntax.Pipe (left, right))
let expression t = snd (Frontend.table_draft (source t))
let named t = fst (Frontend.table_draft (source t)) <> None

let submission t name =
  let binding, expr = Frontend.table_draft (source t) in
  match binding with
  | Some _ -> source t
  | None -> Format.program [ { declaration = Let (name, None, expr); span = expr.span } ]
;;

let cell_literal typ value =
  let lit value = node (Syntax.Literal value) in
  match Types.repr (Types.present typ), value with
  | _, Arrow_result.Missing -> lit Absent
  | Bool, Bool b -> lit (Bool b)
  | Int, Int i -> lit (Int (Int64.to_string i))
  | Float, Float f -> lit (Float (Display.float f))
  | String, Text s -> lit (Text s)
  | Date, Date d -> lit (Date (Calendar.to_iso d))
  | Variant _, Text tag -> node (Syntax.Name (Lexical, tag))
  | _ -> Diagnostic.fail Unsupported_column_type "This cell has no Rupam scalar literal"
;;

let selected t =
  match current t with
  | Some name when Session.total t.result > 0 ->
    let typ, value = Session.cell t.result ~row:t.row name in
    Some (name, typ, value)
  | _ -> None
;;

let ordering session t =
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
           (Session.items session)
         |> Option.value ~default:[]
       | Read _ | Load _ | Join _ -> [])
  in
  let table = Session.table_of_result t.result in
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
        (take (List.length order) names)
        (take (List.length names) order)
      |> List.filter_map Fun.id
    in
    (match keys with
     | [] -> "ordered"
     | _ -> "ordered by (" ^ String.concat ", " keys ^ ")")
;;

let filter t condition policy =
  let args =
    arg condition
    :: Option.to_list
         (Option.map (fun p -> Some "missing", name (TransformIR.policy_name p)) policy)
  in
  let base = expression t in
  let step = call "filter" args in
  match base.form with
  | Pipe (_, last) when Format.expression last = Format.expression step ->
    { t with policy = None }
  | _ -> replace t (pipe base step)
;;

let sort t col direction =
  let base = expression t in
  let key = column col in
  let sorted =
    arg
      (match direction with
       | TransformIR.Ascending -> key
       | Descending -> call "desc" [ arg key ])
  in
  let matches = function
    | None, { Syntax.form = Name (_, n); _ }
    | ( None
      , { form =
            Call ({ form = Name (Bare, "desc"); _ }, [ (None, { form = Name (_, n); _ }) ])
        ; _
        } ) -> n = col
    | _ -> false
  in
  let left, keys =
    match base.form with
    | Pipe (left, { form = Call ({ form = Name (Bare, "arrange"); _ }, keys); _ }) ->
      let keys =
        if List.exists matches keys
        then List.map (fun item -> if matches item then sorted else item) keys
        else keys @ [ sorted ]
      in
      left, keys
    | _ -> base, [ sorted ]
  in
  replace t (pipe left (call "arrange" keys))
;;

let act t action =
  let action () =
    match action, current t with
    | Sort direction, Some col -> sort t col direction
    | Keep_value, _ ->
      (match selected t with
       | Some (name, _, Arrow_result.Missing) ->
         filter t (call "none" [ arg (column name) ]) None
       | Some (name, typ, value) ->
         let condition =
           node (Syntax.Binary (Equal, column name, cell_literal typ value))
         in
         if Types.is_optional typ
         then { t with policy = Some condition }
         else filter t condition None
       | None -> t)
    | Missing_policy policy, _ ->
      Option.fold
        ~none:t
        ~some:(fun condition -> filter t condition (Some policy))
        t.policy
    | Undo, _ ->
      (match t.history with
       | previous :: history -> { (edit t previous) with history }
       | [] -> t)
    | Distribution, _ | Sort _, None -> t
  in
  match action () with
  | t -> t
  | exception exn -> { t with diagnostic = Some (Diagnostic.of_exn exn) }
;;

let visible t =
  let skip = t.top - t.window.offset in
  let columns =
    List.map
      (fun (c : Page.column) ->
         let start = min skip (Array.length c.cells) in
         { c with
           cells = Array.sub c.cells start (min (rows t) (Array.length c.cells - start))
         })
      t.window.columns
  in
  { t.window with columns; offset = t.top }
;;

let bars ~digits t =
  match t.distribution with
  | None -> []
  | Some (name, Frequencies profile) ->
    let most = List.fold_left (fun best (_, n) -> max best n) 1 profile.top in
    [ name ^ " · Top values"
    ; Printf.sprintf "%d distinct" profile.distinct
    ; Printf.sprintf
        "n = %d   Missing %d"
        (Session.total t.result - profile.missing)
        profile.missing
    ]
    @ List.map
        (fun (value, n) ->
           let bar =
             String.concat "" (List.init (max 1 (12 * n / most)) (fun _ -> "█"))
           in
           Printf.sprintf "%s  %d  %S" bar n value)
        profile.top
  | Some (name, Numeric summary) ->
    let h = summary.histogram in
    let most = Array.fold_left max 1 h.counts in
    let glyphs = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] in
    let chart =
      Array.map
        (fun n -> glyphs.(int_of_float (7. *. float_of_int n /. float_of_int most)))
        h.counts
    in
    let number = function
      | None -> "None"
      | Some n -> Display.rounded ~digits n
    in
    let range n = if summary.mean = None then None else Some n in
    [ name ^ "  " ^ String.concat "" (Array.to_list chart)
    ; Printf.sprintf "%d distinct" summary.distinct
    ; Printf.sprintf "n = %d   Missing %d" (Session.total t.result - h.missing) h.missing
    ; Printf.sprintf "Min %s   Q1 %s" (number (range h.low)) (number summary.q1)
    ; Printf.sprintf "Median %s   Q3 %s" (number summary.median) (number summary.q3)
    ; "Max " ^ number (range h.high)
    ; Printf.sprintf "Mean %s   SD %s" (number summary.mean) (number summary.sd)
    ; "Skewness " ^ number summary.skewness
    ]
;;

let show_profile t name profile =
  { t with
    distribution = Some (name, profile)
  ; profiles = (name, profile) :: take 7 (List.remove_assoc name t.profiles)
  }
;;
