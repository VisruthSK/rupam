open Syntax
module T = Types
module C = TransformIR
open Scope

let expect span expected actual =
  try T.unify expected actual with
  | T.Mismatch ->
    fail
      span
      Type_mismatch
      (Printf.sprintf "Expected %s, found %s" (T.display expected) (T.display actual))
;;

let make span shape typ node = { C.node; typ; shape; span }

let align span a b =
  match a, b with
  | C.Scalar, s | s, C.Scalar -> s
  | Columnar, Columnar ->
    ignore span;
    C.Columnar
;;

let shape_of span xs =
  List.fold_left (fun s (x : C.expr) -> align span s x.shape) C.Scalar xs
;;

let closest name candidates =
  List.filter (( <> ) name) candidates
  |> List.map (fun n -> Distance.between n name, n)
  |> List.sort compare
  |> function
  | (d, n) :: _ when d <= 2 -> Some n
  | _ -> None
;;

let did_you_mean = function
  | Some n -> ". Did you mean " ^ n ^ "?"
  | None -> ""
;;

let column level span row name =
  match T.field level row name with
  | Some t -> t
  | None ->
    fail
      span
      Unknown_column
      ("No column named "
       ^ name
       ^ did_you_mean (closest name (List.map fst (T.known row))))
;;

let exact_float span s =
  match Int64.of_string_opt s with
  | Some i when Int64.to_float i < 2. ** 63. && Int64.of_float (Int64.to_float i) = i ->
    Int64.to_float i
  | _ -> fail span Inexact_float (s ^ " is not exactly representable as Float64")
;;

let constant span = function
  | Syntax.Int s ->
    (match Int64.of_string_opt s with
     | Some _ -> C.Number s
     | None -> fail span Int_range "Integer literal is outside the Int range")
  | Float s -> Float (float_of_string s)
  | Text s -> Text s
  | Bool b -> Bool b
  | Date s ->
    (match Calendar.of_iso s with
     | Some d -> Date d
     | None -> fail span Syntax_error ("Invalid date " ^ s ^ "; write d\"YYYY-MM-DD\""))
  | Absent -> assert false
;;

let to_float (x : C.expr) =
  let typ = if T.is_optional x.typ then T.Option T.Float else T.Float in
  match T.repr (T.present x.typ), x.node with
  | Float, _ -> x
  | Var _, Constant (Number _) ->
    expect x.span T.Float (T.present x.typ);
    x
  | Int, Constant (Int i) ->
    { x with node = Constant (Float (exact_float x.span (Int64.to_string i))); typ }
  | (Int | Var _), _ ->
    expect x.span (T.fresh ~kind:Number max_int) (T.present x.typ);
    { x with node = Convert x; typ }
  | _ ->
    expect x.span T.Float (T.present x.typ);
    x
;;

let common span (a : C.expr) (b : C.expr) =
  let ea = T.present a.typ
  and eb = T.present b.typ in
  match T.repr ea, T.repr eb with
  | Int, Float -> to_float a, b, T.Float
  | Float, Int -> a, to_float b, T.Float
  | _ ->
    expect span ea eb;
    a, b, ea
;;

let lift env span shape operands build =
  let optional = List.exists (fun (x : C.expr) -> T.is_optional x.typ) operands in
  if (not optional) || shape = C.Columnar
  then (
    let body : C.expr = build operands in
    if optional then { body with typ = T.optional body.typ } else body)
  else (
    let bound, values =
      List.map
        (fun (x : C.expr) ->
           if T.is_optional x.typ
           then (
             let id = fresh_id env "v" in
             Some (id, x), make x.span x.shape (T.present x.typ) (C.Variable id))
           else None, x)
        operands
      |> List.split
    in
    let body : C.expr = build values in
    make span shape (T.optional body.typ) (Lift (List.filter_map Fun.id bound, body)))
;;

let row_signature level (row : Prelude.row) =
  let number () = T.fresh ~kind:Number level in
  match row with
  | Abs ->
    let a = number () in
    [ a ], a
  | Sign -> [ number () ], T.Int
  | Sqrt | Exp | Log | Log2 | Log10 | Floor | Ceiling | Trunc | Round ->
    [ T.Float ], T.Float
  | Round_digits -> [ T.Float; T.Int ], T.Float
  | Pmin | Pmax ->
    let a = number () in
    [ a; a ], a
  | Present | Absent -> [ T.Option (T.fresh level) ], T.Bool
  | Length -> [ T.String ], T.Int
  | Lower | Upper | Trim -> [ T.String ], T.String
  | Starts | Ends | Detect -> [ T.String; T.String ], T.Bool
  | Year_of | Month_of | Day_of | Wday | Yday -> [ T.Date ], T.Int
  | Floor_year | Floor_month | Floor_week -> [ T.Date ], T.Date
  | Days -> [ T.Int ], T.Duration
  | Months | Years -> [ T.Int ], T.Period
  | Parse_date -> [ T.String; T.String ], T.Option T.Date
  | Size -> [ T.List (T.fresh level) ], T.Int
  | Reverse | Sorted | Deduplicate ->
    let a = T.List (T.fresh level) in
    [ a ], a
  | Map_each ->
    let a = T.fresh level
    and b = T.fresh level in
    [ T.List a; T.Arrow ([ a ], b) ], T.List b
  | Retain ->
    let a = T.fresh level in
    [ T.List a; T.Arrow ([ a ], T.Bool) ], T.List a
  | Fold ->
    let a = T.fresh level
    and b = T.fresh level in
    [ T.List a; b; T.Arrow ([ b; a ], b) ], b
  | Sequence ->
    let n = number () in
    [ n; n; n ], T.List n
;;

let builtin_type level = function
  | Prelude.Row row ->
    let args, result = row_signature level row in
    T.Arrow (args, result)
  | Reduction (Sum | Min | Max) ->
    let a = T.fresh ~kind:Number level in
    T.Arrow ([ T.Option a ], T.Option a)
  | Reduction (Mean | Median | Var | Sd) ->
    T.Arrow ([ T.Option (T.fresh ~kind:Number level) ], T.Option T.Float)
  | Reduction Distinct -> T.Arrow ([ T.Option (T.fresh level) ], T.Int)
  | Reduction (First | Last) ->
    let a = T.fresh level in
    T.Arrow ([ T.Option a ], T.Option a)
  | Reduction Count -> T.Arrow ([], T.Int)
  | Coalesce ->
    let a = T.fresh level in
    T.Arrow ([ T.Option a; a ], a)
;;

let rec annotation env params level = function
  | Arow (fields, rest) ->
    T.Record
      { fields = List.map (fun (n, a) -> n, annotation env params level a) fields
      ; rest =
          Option.map
            (fun n -> Option.value (List.assoc_opt n params) ~default:(T.fresh level))
            rest
      }
  | Atuple ts -> T.Tuple (List.map (annotation env params level) ts)
  | Alist t -> T.List (annotation env params level t)
  | Aarrow (a, b) ->
    T.Arrow ([ annotation env params level a ], annotation env params level b)
  | Optional a -> T.optional (annotation env params level a)
  | Named (n, args) ->
    let args = List.map (annotation env params level) args in
    (match n, args with
     | "Int", [] -> T.Int
     | "Float", [] -> T.Float
     | "String", [] -> T.String
     | "Bool", [] -> T.Bool
     | "List", [ t ] -> T.List t
     | "Option", [ t ] -> T.optional t
     | "Table", [ T.Record row ] -> T.Table row
     | n, [] when List.mem_assoc n params -> List.assoc n params
     | n, args ->
       (match List.assoc_opt n env.types with
        | Some scheme ->
          (match T.instantiate level scheme with
           | T.Arrow (ps, t) when List.length ps = List.length args ->
             List.iter2 T.unify ps args;
             t
           | t when args = [] -> t
           | _ -> Diagnostic.fail Unknown_type ("Wrong number of type arguments to " ^ n))
        | None -> Diagnostic.fail Unknown_type ("Unknown type " ^ n)))
;;
