type kind =
  | Any
  | Number

type t =
  | Int
  | Float
  | Bool
  | String
  | Date
  | Duration
  | Period
  | Option of t
  | List of t
  | Tuple of t list
  | Record of row
  | Table of row
  | Variant of string * t list
  | Arrow of t list * t
  | Var of var ref

and row =
  { fields : (string * t) list
  ; rest : t option
  }

and var =
  | Unbound of
      { level : int
      ; kind : kind
      }
  | Link of t

type scheme =
  { quantified : var ref list
  ; body : t
  }

exception Mismatch

let fresh ?(kind = Any) level = Var (ref (Unbound { level; kind }))
let mono body = { quantified = []; body }
let closed fields = { fields; rest = None }

let rec repr = function
  | Var { contents = Link t } -> repr t
  | t -> t
;;

let optional t =
  match repr t with
  | Option _ -> t
  | t -> Option t
;;

let present t =
  match repr t with
  | Option inner -> inner
  | t -> t
;;

let is_optional t =
  match repr t with
  | Option _ -> true
  | _ -> false
;;

let rec iter_vars f t =
  match repr t with
  | Var v -> f v
  | Option t | List t -> iter_vars f t
  | Tuple ts | Variant (_, ts) -> List.iter (iter_vars f) ts
  | Record row | Table row -> iter_row f row
  | Arrow (args, result) -> List.iter (iter_vars f) (result :: args)
  | Int | Float | Bool | String | Date | Duration | Period -> ()

and iter_row f { fields; rest } =
  List.iter (fun (_, t) -> iter_vars f t) fields;
  Option.iter (iter_vars f) rest
;;

let rec resolve t =
  match repr t with
  | Option t -> optional (resolve t)
  | List t -> List (resolve t)
  | Tuple ts -> Tuple (List.map resolve ts)
  | Variant (n, ts) -> Variant (n, List.map resolve ts)
  | Record row -> Record (resolve_row row)
  | Table row -> Table (resolve_row row)
  | Arrow (args, result) -> Arrow (List.map resolve args, resolve result)
  | t -> t

and resolve_row row =
  let rec flatten fields = function
    | None -> fields, None
    | Some t ->
      (match repr t with
       | Record { fields = more; rest } | Table { fields = more; rest } ->
         flatten (fields @ more) rest
       | t -> fields, Some t)
  in
  let fields, rest = flatten row.fields row.rest in
  { fields = List.map (fun (n, t) -> n, resolve t) fields
  ; rest = Option.map resolve rest
  }
;;

let default_numbers t =
  match repr t with
  | Arrow _ -> ()
  | _ ->
    iter_vars
      (fun v ->
         match !v with
         | Unbound { kind = Number; _ } -> v := Link Int
         | _ -> ())
      t
;;

let display t =
  let names = ref [] in
  let name v kind =
    match List.assq_opt v !names with
    | Some (_, n) -> n
    | None ->
      let same = List.length (List.filter (fun (_, (k, _)) -> k = kind) !names) in
      let n =
        match kind, same with
        | Number, 0 -> "Number"
        | Number, n -> "Number" ^ string_of_int (n + 1)
        | Any, n when n < 26 -> String.make 1 (Char.chr (Char.code 'a' + n))
        | Any, n -> "t" ^ string_of_int n
      in
      names := (v, (kind, n)) :: !names;
      n
  in
  let rec show t =
    match repr t with
    | Int -> "Int"
    | Float -> "Float"
    | Bool -> "Bool"
    | String -> "String"
    | Date -> "Date"
    | Duration -> "Duration"
    | Period -> "Period"
    | Option t -> show t ^ "?"
    | List t -> "[" ^ show t ^ "]"
    | Tuple ts -> "(" ^ String.concat ", " (List.map show ts) ^ ")"
    | Record row -> show_row row
    | Table row -> "Table<" ^ show_row row ^ ">"
    | Variant (n, []) -> n
    | Variant (n, ts) -> n ^ "<" ^ String.concat ", " (List.map show ts) ^ ">"
    | Arrow (args, result) ->
      "(" ^ String.concat ", " (List.map show args) ^ ") -> " ^ show result
    | Var ({ contents = Unbound { kind; _ } } as v) -> name v kind
    | Var _ -> assert false
  and show_row row =
    let row = resolve_row row in
    "{"
    ^ String.concat
        ", "
        (List.map (fun (n, t) -> Identifier.quote n ^ ": " ^ show t) row.fields
         @ Option.to_list (Option.map (fun t -> ".." ^ show t) row.rest))
    ^ "}"
  in
  show t
;;

let occurs_and_adjust var level t =
  iter_vars
    (fun v ->
       if v == var then raise Mismatch;
       match !v with
       | Unbound u when u.level > level -> v := Unbound { u with level }
       | _ -> ())
    t
;;

let numeric t =
  match repr t with
  | Int | Float -> true
  | _ -> false
;;

let rec unify a b =
  match repr a, repr b with
  | Var x, Var y when x == y -> ()
  | Var ({ contents = Unbound { level; kind } } as v), t
  | t, Var ({ contents = Unbound { level; kind } } as v) ->
    (match kind, t with
     | Number, Var ({ contents = Unbound u } as w) ->
       w := Unbound { u with kind = Number }
     | Number, t when not (numeric t) -> raise Mismatch
     | _ -> ());
    occurs_and_adjust v level t;
    v := Link t
  | Int, Int
  | Float, Float
  | Bool, Bool
  | String, String
  | Date, Date
  | Duration, Duration
  | Period, Period -> ()
  | Option a, Option b | List a, List b -> unify a b
  | Tuple xs, Tuple ys when List.length xs = List.length ys -> List.iter2 unify xs ys
  | Variant (m, xs), Variant (n, ys) when m = n && List.length xs = List.length ys ->
    List.iter2 unify xs ys
  | Arrow (xs, x), Arrow (ys, y) when List.length xs = List.length ys ->
    List.iter2 unify xs ys;
    unify x y
  | Record r, Record s | Table r, Table s -> unify_rows r s
  | _ -> raise Mismatch

and unify_rows r s =
  let r = resolve_row r
  and s = resolve_row s in
  let missing xs ys = List.filter (fun (n, _) -> not (List.mem_assoc n ys)) xs in
  List.iter (fun (n, t) -> Option.iter (unify t) (List.assoc_opt n s.fields)) r.fields;
  let only_r = missing r.fields s.fields
  and only_s = missing s.fields r.fields in
  let open_row fields rest = Record { fields; rest } in
  match r.rest, s.rest with
  | None, None -> if only_r <> [] || only_s <> [] then raise Mismatch
  | Some a, None ->
    if only_r <> [] then raise Mismatch;
    unify a (open_row only_s None)
  | None, Some b ->
    if only_s <> [] then raise Mismatch;
    unify b (open_row only_r None)
  | Some a, Some b when only_r = [] && only_s = [] -> unify a b
  | Some a, Some b ->
    if repr a == repr b then raise Mismatch;
    let level =
      match repr a, repr b with
      | Var { contents = Unbound x }, Var { contents = Unbound y } -> min x.level y.level
      | _ -> max_int
    in
    let rest = fresh level in
    unify a (open_row only_s (Some rest));
    unify b (open_row only_r (Some rest))
;;

let generalize level t =
  let quantified = ref [] in
  iter_vars
    (fun v ->
       match !v with
       | Unbound u when u.level > level && not (List.memq v !quantified) ->
         quantified := v :: !quantified
       | _ -> ())
    t;
  { quantified = List.rev !quantified; body = t }
;;

let instantiate level { quantified; body } =
  let fresh_vars =
    List.map
      (fun v ->
         match !v with
         | Unbound { kind; _ } -> v, fresh ~kind level
         | Link _ -> v, Var v)
      quantified
  in
  let rec copy t =
    match repr t with
    | Var v -> Option.value (List.assq_opt v fresh_vars) ~default:t
    | Option t -> Option (copy t)
    | List t -> List (copy t)
    | Tuple ts -> Tuple (List.map copy ts)
    | Variant (n, ts) -> Variant (n, List.map copy ts)
    | Record row -> Record (copy_row row)
    | Table row -> Table (copy_row row)
    | Arrow (args, result) -> Arrow (List.map copy args, copy result)
    | t -> t
  and copy_row { fields; rest } =
    { fields = List.map (fun (n, t) -> n, copy t) fields; rest = Option.map copy rest }
  in
  if quantified = [] then body else copy body
;;

let field level row name =
  let rec through = function
    | None -> None
    | Some tail ->
      (match repr tail with
       | Record r | Table r ->
         (match List.assoc_opt name r.fields with
          | Some t -> Some t
          | None -> through r.rest)
       | Var { contents = Unbound _ } as v ->
         let t = fresh level in
         unify v (Record { fields = [ name, t ]; rest = Some (fresh level) });
         Some t
       | _ -> None)
  in
  match List.assoc_opt name row.fields with
  | Some t -> Some t
  | None -> through row.rest
;;

let known row = (resolve_row row).fields
let sexp_of_t t = Sexplib0.Sexp.Atom (display t)
