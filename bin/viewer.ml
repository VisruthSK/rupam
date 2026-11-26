open Rupam

type t =
  { session : Session.t
  ; base : string
  ; steps : string list
  ; result : Session.result
  ; names : string list
  ; pinned : string list
  ; row : int
  ; column : int
  ; width : int
  ; height : int
  ; window : Page.t
  ; distribution : (string * Arrow_result.histogram) option
  ; message : string option
  }

type move =
  | Rows of int
  | Columns of int
  | Top
  | Bottom

type action =
  | Sort of bool
  | Keep_value
  | Undo
  | Distribution

let rows t = max 1 (t.height - 6)
let source t = String.concat " |> " (t.base :: List.rev t.steps)
let current t = List.nth_opt t.names t.column

let visible_names t =
  t.pinned
  @ List.filteri (fun i name -> i >= t.column && not (List.mem name t.pinned)) t.names
;;

let fetch t =
  let offset = max 0 (t.row - rows t) in
  { t with
    window = Session.page t.result ~columns:(visible_names t) ~offset ~limit:(3 * rows t)
  }
;;

let covered t =
  let length =
    match t.window.columns with
    | c :: _ -> Array.length c.cells
    | [] -> 0
  in
  t.window.offset <= t.row
  && (t.row + rows t <= t.window.offset + length
      || t.window.offset + length >= Session.total t.result)
;;

let load session ~base ~steps ~width ~height =
  let table = Session.table session (String.concat " |> " (base :: List.rev steps)) in
  let result = Session.result session table in
  let pinned =
    match table.keys with
    | key :: _ -> List.filter (fun k -> List.mem_assoc k table.schema) key
    | [] -> []
  in
  fetch
    { session
    ; base
    ; steps
    ; result
    ; names = List.map fst table.schema
    ; pinned
    ; row = 0
    ; column = 0
    ; width
    ; height
    ; window = Session.page result ~offset:0 ~limit:0
    ; distribution = None
    ; message = None
    }
;;

let open_ session ~title ~width ~height =
  load session ~base:title ~steps:[] ~width ~height
;;

let resize t ~width ~height = fetch { t with width; height }

let move t step =
  let last_row = max 0 (Session.total t.result - rows t) in
  let clamp high x = max 0 (min high x) in
  let t =
    match step with
    | Rows n -> { t with row = clamp last_row (t.row + n) }
    | Columns n -> { t with column = clamp (List.length t.names - 1) (t.column + n) }
    | Top -> { t with row = 0 }
    | Bottom -> { t with row = last_row }
  in
  let t = { t with distribution = None; message = None } in
  match step with
  | Columns _ -> fetch t
  | _ -> if covered t then t else fetch t
;;

let literal (column : Page.column) value =
  match Types.repr (Types.present column.typ) with
  | String -> Printf.sprintf "%S" value
  | Bool -> String.uppercase_ascii value
  | _ -> value
;;

let refine t steps =
  match load t.session ~base:t.base ~steps ~width:t.width ~height:t.height with
  | v -> { v with column = min t.column (List.length v.names - 1) }
  | exception exn -> { t with message = Some (Diagnostic.render (Diagnostic.of_exn exn)) }
;;

let act t action =
  match action, current t with
  | Sort ascending, Some name ->
    let key = Identifier.quote name in
    refine
      t
      ((if ascending then "arrange(" ^ key ^ ")" else "arrange(desc(" ^ key ^ "))")
       :: t.steps)
  | Keep_value, Some name ->
    let column = List.find (fun (c : Page.column) -> c.name = name) t.window.columns in
    let i = t.row - t.window.offset in
    (match column.cells.(i) with
     | Some value ->
       refine
         t
         (Printf.sprintf "filter(%s == %s)" (Identifier.quote name) (literal column value)
          :: t.steps)
     | None ->
       refine t (Printf.sprintf "filter(none(%s))" (Identifier.quote name) :: t.steps)
     | exception Invalid_argument _ -> t)
  | Undo, _ ->
    (match t.steps with
     | _ :: steps -> refine t steps
     | [] -> t)
  | Distribution, Some name ->
    (match Session.histogram t.result name with
     | h -> { t with distribution = Some (name, h) }
     | exception exn ->
       { t with message = Some (Diagnostic.render (Diagnostic.of_exn exn)) })
  | _, None -> t
;;

let visible t =
  let skip = t.row - t.window.offset in
  let columns =
    List.map
      (fun (c : Page.column) ->
         { c with
           cells =
             Array.sub c.cells skip (max 0 (min (rows t) (Array.length c.cells - skip)))
         })
      t.window.columns
  in
  { t.window with columns; offset = t.row }
;;

let bars t =
  match t.distribution with
  | None -> []
  | Some (name, h) ->
    let most = Array.fold_left max 1 h.counts in
    let step = (h.high -. h.low) /. float_of_int (Array.length h.counts) in
    Printf.sprintf "distribution of %s" name
    :: (Array.to_list h.counts
        |> List.mapi (fun i count ->
          Printf.sprintf
            "%12.4g  %s %d"
            (h.low +. (step *. float_of_int i))
            (String.concat "" (List.init (count * 40 / most) (fun _ -> "\xe2\x96\x88")))
            count))
;;

let status t =
  Option.value
    t.message
    ~default:
      (Printf.sprintf
         "%s  rows %d-%d of %d  %s  s/S sort  f keep value  u undo  d distribution  \
          Enter to prompt  q close"
         (match t.steps with
          | [] -> t.base
          | steps -> t.base ^ " |> " ^ String.concat " |> " (List.rev steps))
         (t.row + 1)
         (min (Session.total t.result) (t.row + rows t))
         (Session.total t.result)
         (Option.value (current t) ~default:""))
;;
