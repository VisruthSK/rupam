open Rupam

type t =
  { result : Session.result
  ; title : string
  ; row : int
  ; column : int
  ; width : int
  ; height : int
  ; window : Page.t
  }

type move =
  | Rows of int
  | Columns of int
  | Top
  | Bottom

let rows t = max 1 (t.height - 5)

let fetch t =
  let covered =
    t.window.offset <= t.row
    && t.row + rows t <= t.window.offset + Array.length (List.hd t.window.columns).cells
  in
  if covered || t.window.columns = []
  then t
  else (
    let offset = max 0 (t.row - rows t) in
    { t with window = Session.page t.result ~offset ~limit:(3 * rows t) })
;;

let open_ ~title ~width ~height result =
  let t =
    { result
    ; title
    ; row = 0
    ; column = 0
    ; width
    ; height
    ; window = Session.page result ~offset:0 ~limit:(3 * max 1 (height - 5))
    }
  in
  t
;;

let resize t ~width ~height = fetch { t with width; height }

let move t step =
  let total = Session.total t.result in
  let last_row = max 0 (total - rows t) in
  let clamp low high x = max low (min high x) in
  let t =
    match step with
    | Rows n -> { t with row = clamp 0 last_row (t.row + n) }
    | Columns n ->
      { t with column = clamp 0 (List.length t.window.columns - 1) (t.column + n) }
    | Top -> { t with row = 0 }
    | Bottom -> { t with row = last_row }
  in
  fetch t
;;

let visible t =
  let skip = t.row - t.window.offset in
  let columns =
    List.filteri (fun i _ -> i >= t.column) t.window.columns
    |> List.map (fun (c : Page.column) ->
      { c with
        cells =
          Array.sub c.cells skip (max 0 (min (rows t) (Array.length c.cells - skip)))
      })
  in
  { t.window with columns; offset = t.row }
;;

let status t =
  Printf.sprintf
    "%s  rows %d-%d of %d  column %d of %d  arrows, PgUp/PgDn, g/G, q"
    t.title
    (t.row + 1)
    (min (Session.total t.result) (t.row + rows t))
    (Session.total t.result)
    (t.column + 1)
    (List.length t.window.columns)
;;
