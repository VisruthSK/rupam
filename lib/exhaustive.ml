open Syntax
module T = Types
module C = TransformIR
open Scope

let uncovered env level subject (arms : Syntax.arm list) =
  let heads t =
    match T.repr t with
    | Bool -> [ `Constant (Syntax.Bool true), []; `Constant (Syntax.Bool false), [] ]
    | Option inner -> [ `Constant Syntax.Absent, []; `Constructor "Some", [ inner ] ]
    | Tuple fields -> [ `Product, fields ]
    | Variant (name, _) ->
      List.assoc name env.variants
      |> List.map (fun (tag, scheme) ->
        let fields, result =
          match T.instantiate level scheme with
          | Arrow (fields, result) -> fields, result
          | result -> [], result
        in
        T.unify t result;
        `Constructor tag, fields)
    | _ -> []
  in
  let rec catchall = function
    | Wildcard | Bind _ | Precord _ -> true
    | Pas (_, p) -> catchall p
    | Palt ps -> List.exists catchall ps
    | Tag _ | Ptuple _ | Pliteral _ | Prange _ -> false
  in
  let rec expand rows =
    List.concat_map
      (function
        | Pas (_, p) :: ps -> expand [ p :: ps ]
        | Palt alts :: ps -> expand (List.map (fun p -> p :: ps) alts)
        | row -> [ row ])
      rows
  in
  let covers t rows =
    let integral = T.repr t = Int in
    let number = function
      | Syntax.Int s | Float s -> float_of_string s
      | _ -> nan
    in
    let interval = function
      | Prange { low; high; inclusive } ->
        let low = Option.fold ~none:neg_infinity ~some:number low in
        let high, closed =
          match high with
          | None -> infinity, false
          | Some h when integral && inclusive -> number h +. 1., false
          | Some h -> number h, inclusive
        in
        Some (low, high, closed)
      | Pliteral ((Syntax.Int _ | Float _) as l) ->
        let x = number l in
        Some (if integral then x, x +. 1., false else x, x, true)
      | _ -> None
    in
    let reach, _ =
      List.filter_map
        (function
          | p :: rest when List.for_all catchall rest -> interval p
          | _ -> None)
        rows
      |> List.sort compare
      |> List.fold_left
           (fun (reach, closed) (low, high, high_closed) ->
              if low <= reach
              then
                if high > reach
                then high, high_closed
                else reach, closed || (high = reach && high_closed)
              else reach, closed)
           (neg_infinity, false)
    in
    reach = infinity
  in
  let specialize head arity = function
    | p :: ps when catchall p -> Some (List.init arity (fun _ -> Wildcard) @ ps)
    | p :: ps ->
      (match head, p with
       | `Constructor n, Tag (m, vs) when n = m -> Some (vs @ ps)
       | `Constant l, Pliteral m when l = m -> Some ps
       | `Product, Ptuple vs -> Some (vs @ ps)
       | _ -> None)
    | [] -> None
  in
  let rebuild head arity witness =
    let args = List.filteri (fun i _ -> i < arity) witness
    and rest = List.filteri (fun i _ -> i >= arity) witness in
    (match head with
     | `Constructor n -> Tag (n, args)
     | `Constant l -> Pliteral l
     | `Product -> Ptuple args)
    :: rest
  in
  let rec missing types rows =
    let rows = expand rows in
    match types with
    | [] -> if rows = [] then Some [] else None
    | t :: rest ->
      let wildcard () =
        List.filter_map
          (function
            | p :: ps when catchall p -> Some ps
            | _ -> None)
          rows
        |> missing rest
        |> Option.map (List.cons Wildcard)
      in
      let all_catchall =
        List.for_all
          (function
            | p :: _ -> catchall p
            | [] -> false)
          rows
      in
      (match heads t with
       | [] when (not all_catchall) && covers t rows ->
         missing rest [ List.map (fun _ -> Wildcard) rest ]
       | [] -> wildcard ()
       | _ when all_catchall -> wildcard ()
       | cases ->
         List.find_map
           (fun (head, fields) ->
              let arity = List.length fields in
              missing (fields @ rest) (List.filter_map (specialize head arity) rows)
              |> Option.map (rebuild head arity))
           cases)
  in
  let unguarded = List.filter (fun (arm : Syntax.arm) -> arm.guard = None) arms in
  match
    missing [ subject ] (List.map (fun (arm : Syntax.arm) -> [ arm.pattern ]) unguarded)
  with
  | Some [ p ] -> Some p
  | _ -> None
;;
