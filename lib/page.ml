type column =
  { name : string
  ; typ : Shape.typ
  ; cells : string option array
  }

type t =
  { columns : column list
  ; offset : int
  ; total : int
  ; ordered : bool
  }

type part =
  | Plain
  | Heading
  | Types
  | Missing
  | Index
  | Note

let abbreviation (t : Shape.typ) =
  (match t.primitive with
   | Int64 -> "int"
   | Float64 -> "dbl"
   | Bool -> "lgl"
   | String -> "chr")
  ^ if t.nullable then "?" else ""
;;

let cell (t : Shape.typ) = function
  | None -> None
  | Some text ->
    Some
      (match t.primitive with
       | Float64 -> Display.float (float_of_string text)
       | Bool -> Display.bool (String.lowercase_ascii text = "true")
       | Int64 | String -> text)
;;

let read ~path ~total ~ordered ~offset fields =
  let names, rows = Data_file.query ("SELECT * FROM read_parquet(" ^ Data_file.literal path ^ ")") in
  let columns =
    Array.to_list names
    |> List.mapi (fun i name ->
      let typ = List.assoc name fields in
      { name; typ; cells = Array.map (fun row -> cell typ row.(i)) rows })
  in
  { columns; offset; total; ordered }
;;

let width = Display.width

let pad right size text =
  let gap = String.make (max 0 (size - width text)) ' ' in
  if right then gap ^ text else text ^ gap
;;

let lines ?(limit = 100) page =
  let shown = match page.columns with [] -> 0 | c :: _ -> Array.length c.cells in
  let numeric (c : column) = c.typ.primitive = Int64 || c.typ.primitive = Float64 in
  let index = string_of_int (page.offset + shown) |> String.length in
  let measured =
    List.map
      (fun c ->
         let cells = Array.map (Option.value ~default:"None") c.cells in
         let size =
           Array.fold_left (fun w s -> max w (width s)) (max (width c.name) (width (abbreviation c.typ) + 2)) cells
         in
         c, size)
      page.columns
  in
  let rec fit used = function
    | (c, size) :: rest when used + size + 2 <= limit || used = index -> (c, size) :: fit (used + size + 2) rest
    | _ -> []
  in
  let visible = fit index measured in
  let hidden = List.filteri (fun i _ -> i >= List.length visible) page.columns in
  let row cells = (Note, String.make index ' ') :: cells in
  let separated parts = List.concat_map (fun p -> [ Plain, "  "; p ]) parts in
  let header =
    row (separated (List.map (fun (c, size) -> Heading, pad (numeric c) size c.name) visible))
  in
  let types =
    row
      (separated
         (List.map (fun (c, size) -> Types, pad (numeric c) size ("<" ^ abbreviation c.typ ^ ">")) visible))
  in
  let body =
    List.init shown (fun r ->
      (Index, pad true index (string_of_int (page.offset + r + 1)))
      :: separated
           (List.map
              (fun (c, size) ->
                 match c.cells.(r) with
                 | None -> Missing, pad (numeric c) size "None"
                 | Some s -> Plain, pad (numeric c) size s)
              visible))
  in
  let summary =
    Printf.sprintf
      "%d %s × %d %s, %s"
      page.total
      (if page.total = 1 then "row" else "rows")
      (List.length page.columns)
      (if List.length page.columns = 1 then "column" else "columns")
      (if page.ordered then "ordered" else "unordered")
  in
  let remaining = page.total - page.offset - shown in
  let more =
    (if remaining > 0
     then [ [ Note, Printf.sprintf "… %d more %s" remaining (if remaining = 1 then "row" else "rows") ] ]
     else [])
    @
    match hidden with
    | [] -> []
    | hidden ->
      [ [ Note
        , "… and "
          ^ String.concat
              ", "
              (List.map (fun c -> c.name ^ " <" ^ abbreviation c.typ ^ ">") hidden)
        ]
      ]
  in
  ([ Note, summary ] :: header :: types :: body) @ more
;;

let plain page =
  List.map (fun parts -> String.concat "" (List.map snd parts)) (lines page)
;;
