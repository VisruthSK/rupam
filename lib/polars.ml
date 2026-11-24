open Shape

let quoted s = Yojson.Basic.to_string (`String s)
let col n = "pl.col(" ^ quoted n ^ ")"

let lower ~fields table =
  let symbols = Array.make table.Table.next "" in
  List.iteri (fun i n -> symbols.(i) <- n) fields;
  let next = ref (List.length fields) in
  let rec emit x = match x.node with
    | Input i -> col symbols.(i)
    | Broadcast (_, x) -> emit x
    | Constant (`Int i) -> Printf.sprintf "pl.lit(%Ld, dtype=pl.Int64)" i
    | Constant (`Float f) -> Printf.sprintf "pl.lit(float(%s))" (quoted (Printf.sprintf "%.17g" f))
    | Constant (`Bool b) -> if b then "pl.lit(True)" else "pl.lit(False)"
    | Coalesce (a,b) -> "pl.coalesce(" ^ emit a ^ ", " ^ emit b ^ ")"
    | Abs a when a.typ.primitive = Float64 -> emit a ^ ".abs()"
    | Abs _ -> raise (Error "Polars Int64 abs requires checked overflow")
    | Binary (op,a,b) ->
        if a.typ.primitive = Int64 && op <> Greater then
          raise (Error "Polars Int64 arithmetic requires checked overflow");
        let op_text = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Greater -> ">" in
        let l = emit a and r = emit b in
        let value = "(" ^ l ^ " " ^ op_text ^ " " ^ r ^ ")" in
        if op = Greater && a.typ.primitive = Float64 then
          "pl.when(" ^ l ^ ".is_null() | " ^ r ^ ".is_null()).then(None).otherwise(" ^
          value ^ " & ~" ^ l ^ ".is_nan() & ~" ^ r ^ ".is_nan())"
        else value
    | Reduce (r, a) ->
        let a = emit a in
        (match r with Mean -> a ^ ".mean()" | Sd -> a ^ ".std(ddof=1)"
         | Sum -> "pl.when(" ^ a ^ ".count() > 0).then(" ^ a ^ ".sum()).otherwise(None)") in
  let stage = function
    | Table.Take n -> Printf.sprintf ".head(%d)" n
    | Distinct _ -> ".unique()"
    | Table.Arrange keys ->
        let xs,ds = List.split keys in
        ".sort([" ^ String.concat ", " (List.map emit xs) ^ "], descending=[" ^
        String.concat ", " (List.map (fun d -> if d then "True" else "False") ds) ^ "], nulls_last=True)"
    | Summarize (keys, measures) ->
        let measure (n,x) = Option.fold ~none:"pl.len().cast(pl.Int64)" ~some:emit x ^ ".alias(" ^ quoted n ^ ")" in
        let aggs = String.concat ", " (List.map measure measures) in
        let groups = String.concat ", " (List.map (fun (n,_) -> quoted n) keys) in
        List.iter (fun n -> symbols.(!next) <- n; incr next) (List.map fst keys @ List.map fst measures);
        if keys = [] then ".select([" ^ aggs ^ "])" else ".group_by([" ^ groups ^ "]).agg([" ^ aggs ^ "])"
    | Table.Select fields -> ".select([" ^ String.concat ", " (List.map quoted fields) ^ "])"
    | Filter x -> ".filter(" ^ emit x ^ ")"
    | Bind (name,x) ->
        let value = emit x in
        symbols.(!next) <- name; incr next;
        ".with_columns(" ^ value ^ ".alias(" ^ quoted name ^ "))" in
  List.rev table.steps |> List.map stage |> String.concat "\n    "


