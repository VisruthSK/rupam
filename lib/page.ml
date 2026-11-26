type column =
  { name : string
  ; typ : Types.t
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

let element (c : column) = Types.repr (Types.present c.typ)

let cell t = function
  | None -> None
  | Some text ->
    Some
      (match Types.repr (Types.present t) with
       | Bool -> Display.bool (String.lowercase_ascii text = "true")
       | _ -> text)
;;

let of_arrow arrow ~ordered ~offset ~limit fields =
  let names = Arrow_result.names arrow in
  let cells = Arrow_result.cells arrow ~offset ~limit in
  { columns =
      Array.to_list
        (Array.mapi
           (fun i name ->
              let typ = List.assoc name fields in
              { name; typ; cells = Array.map (cell typ) cells.(i) })
           names)
  ; offset
  ; total = Arrow_result.rows arrow
  ; ordered
  }
;;

let read ~path ~total ~ordered ~offset ~limit fields =
  let names, rows =
    Data_file.query
      (Printf.sprintf
         "SELECT * FROM read_parquet(%s) LIMIT %d OFFSET %d"
         (Data_file.literal path)
         limit
         offset)
  in
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

let lines ?(limit = 100) ~digits page =
  let shown =
    match page.columns with
    | [] -> 0
    | c :: _ -> Array.length c.cells
  in
  let numeric (c : column) = Types.numeric (element c) in
  let index = string_of_int (page.offset + shown) |> String.length in
  let rec fit first used = function
    | [] -> [], []
    | c :: rest as remaining ->
      let minimum = max (width c.name) (width (Types.display c.typ) + 2) in
      if (not first) && used + minimum + 2 > limit
      then [], remaining
      else (
        let c =
          match element c with
          | Float ->
            let format s = Printf.sprintf "%.*f" digits (float_of_string s) in
            { c with cells = Array.map (Option.map format) c.cells }
          | _ -> c
        in
        let size =
          Array.fold_left
            (fun size cell -> max size (width (Option.value cell ~default:"None")))
            minimum
            c.cells
        in
        if (not first) && used + size + 2 > limit
        then [], remaining
        else (
          let visible, hidden = fit false (used + size + 2) rest in
          (c, size) :: visible, hidden))
  in
  let visible, hidden = fit true index page.columns in
  let row cells = (Note, String.make index ' ') :: cells in
  let separated parts = List.concat_map (fun p -> [ Plain, "  "; p ]) parts in
  let header =
    row
      (separated
         (List.map (fun (c, size) -> Heading, pad (numeric c) size c.name) visible))
  in
  let types =
    row
      (separated
         (List.map
            (fun (c, size) ->
               Types, pad (numeric c) size ("<" ^ Types.display c.typ ^ ">"))
            visible))
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
     then
       [ [ ( Note
           , Printf.sprintf
               "… %d more %s"
               remaining
               (if remaining = 1 then "row" else "rows") )
         ]
       ]
     else [])
    @
    match hidden with
    | [] -> []
    | hidden ->
      [ [ ( Note
          , "… and "
            ^ String.concat
                ", "
                (List.map (fun c -> c.name ^ " <" ^ Types.display c.typ ^ ">") hidden) )
        ]
      ]
  in
  ([ Note, summary ] :: header :: types :: body) @ more
;;

let plain ~digits page =
  List.map (fun parts -> String.concat "" (List.map snd parts)) (lines ~digits page)
;;
