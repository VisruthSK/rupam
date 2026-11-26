module C = TransformIR
module T = Types

type names =
  { reserved : string list
  ; bound : (int, string) Hashtbl.t
  ; used : (string, unit) Hashtbl.t
  }

let names reserved = { reserved; bound = Hashtbl.create 64; used = Hashtbl.create 64 }

let identifier text =
  let ascii c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
    | _ -> '_'
  in
  let s = String.map ascii text in
  if s = "" || (s.[0] >= '0' && s.[0] <= '9') || s.[0] = '_' then "v" ^ s else s
;;

let name names (id : C.id) =
  match Hashtbl.find_opt names.bound id.stamp with
  | Some n -> n
  | None ->
    let base = identifier id.name in
    let base = if List.mem base names.reserved then base ^ "_" else base in
    let rec pick k =
      let candidate = if k = 1 then base else base ^ "_" ^ string_of_int k in
      if Hashtbl.mem names.used candidate then pick (k + 1) else candidate
    in
    let chosen = pick 1 in
    Hashtbl.replace names.used chosen ();
    Hashtbl.replace names.bound id.stamp chosen;
    chosen
;;

let rec descriptor t : Yojson.Basic.t =
  match T.repr t with
  | Int -> `String "int"
  | Float -> `String "float"
  | Bool -> `String "bool"
  | String -> `String "string"
  | List t -> `List [ `String "list"; descriptor t ]
  | Option t -> `List [ `String "option"; descriptor t ]
  | Tuple ts -> `List (`String "tuple" :: List.map descriptor ts)
  | Record row ->
    `List
      [ `String "record"
      ; `List
          (List.map
             (fun (n, t) -> `List [ `String n; descriptor t ])
             (T.resolve_row row).fields)
      ]
  | Variant _ | Var _ | Table _ -> `String "any"
  | Arrow _ -> `String "function"
;;

let float e = T.repr (T.present e.C.typ) = Float
let integer e = T.repr (T.present e.C.typ) = Int

let risky =
  C.exists (fun e ->
    match e.node with
    | Binary ((Add | Sub | Mul | Div | Power), _, _) -> float e || integer e
    | Unary (Negate, _) -> integer e
    | Builtin ((Exp | Log | Log2 | Log10 | Sqrt), _) -> true
    | Reduce ((Sum | Mean | Var | Sd), _, _) -> true
    | _ -> false)
;;

let nonportable items =
  List.concat_map
    (fun (item : C.item) ->
       let found = ref [] in
       let visit e =
         ignore
           (C.exists
              (fun e ->
                 (match e.node with
                  | Foreign (m, f, _) ->
                    let n = m ^ "." ^ f in
                    if not (List.mem n !found) then found := n :: !found
                  | _ -> ());
                 false)
              e)
       in
       (match item with
        | Define (_, e) | Show e | Write (e, _) -> visit e
        | Variant _ -> ());
       List.rev !found)
    items
  |> List.sort_uniq compare
;;
