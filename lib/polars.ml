open Shape

let quoted s = Yojson.Basic.to_string (`String s)
let col n = "pl.col(" ^ quoted n ^ ")"

let lower ~fields table =
  let symbols = Array.make table.Table.next "" in
  List.iteri (fun i n -> symbols.(i) <- n) fields;
  let next = ref (List.length fields) in
  let callback x =
    let inputs = ref [] in
    let rec scalar v = match v.node with
      | Input i ->
          if not (List.mem i !inputs) then inputs := i :: !inputs;
          "row[" ^ quoted symbols.(i) ^ "]"
      | Broadcast (_,e) -> scalar e
      | Constant (`Bool b) -> if b then "True" else "False"
      | Constant (`Float f) -> "float(" ^ quoted (Printf.sprintf "%.17g" f) ^ ")"
      | Constant (`Int i) -> Int64.to_string i
      | Choose (c,t,f) -> "(" ^ scalar t ^ " if " ^ scalar c ^ " else " ^ scalar f ^ ")"
      | Polars_sqrt _ -> raise (Error "polars.sqrt cannot be lowered inside a scalar callback")
      | Abs e -> "rp_abs(" ^ scalar e ^ ")"
      | Coalesce (a,b) -> "(lambda v: " ^ scalar b ^ " if v is None else v)(" ^ scalar a ^ ")"
      | Binary (op,a,b) ->
          let name = match op with Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Greater -> "gt" in
          "rp_binary(" ^ quoted name ^ ", " ^ scalar a ^ ", " ^ scalar b ^ ")"
      | Reduce _ -> raise (Error "A checked callback cannot evaluate a whole-column reducer per row") in
    let body = scalar x in
    let ty = match x.typ.primitive with Int64 -> "Int64" | Float64 -> "Float64" | Bool -> "Boolean" | String -> "String" in
    match !inputs with
    | [] -> "pl.lit(" ^ body ^ ", dtype=pl." ^ ty ^ ")"
    | ids -> "pl.struct([" ^ String.concat ", " (List.rev_map (fun i -> col symbols.(i)) ids) ^
        "]).map_elements(lambda row: " ^ body ^ ", return_dtype=pl." ^ ty ^ ")" in
  let rec checked e = match e.node with
    | Binary (op,a,b) -> (a.typ.primitive=Int64 && op<>Greater) || checked a || checked b
    | Abs a -> a.typ.primitive=Int64 || checked a
    | Polars_sqrt a | Broadcast (_,a) | Reduce (_,a) -> checked a
    | Choose (a,b,c) -> checked a || checked b || checked c
    | Coalesce (a,b) -> checked a || checked b
    | Constant _ | Input _ -> false in
  let rec emit x = match x.node with
    | Choose _ when checked x -> callback x
    | Input i -> col symbols.(i)
    | Broadcast (_, x) -> emit x
    | Constant (`Int i) -> Printf.sprintf "pl.lit(%Ld, dtype=pl.Int64)" i
    | Constant (`Float f) -> Printf.sprintf "pl.lit(float(%s))" (quoted (Printf.sprintf "%.17g" f))
    | Constant (`Bool b) -> if b then "pl.lit(True)" else "pl.lit(False)"
    | Choose (c,t,f) ->
        (* Each supported native branch is total. Checked Int64 arithmetic below rejects lowering. *)
        "pl.when(" ^ emit c ^ ").then(" ^ emit t ^ ").otherwise(" ^ emit f ^ ")"
    | Coalesce (a,b) -> "pl.coalesce(" ^ emit a ^ ", " ^ emit b ^ ")"
    | Polars_sqrt a -> emit a ^ ".sqrt()"
    | Abs a when a.typ.primitive = Float64 -> emit a ^ ".abs()"
    | Abs _ -> callback x
    | Binary (op,a,_) when a.typ.primitive = Int64 && op <> Greater -> callback x
    | Binary (op,a,b) ->
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
    | Table.Pivot p ->
        let ids=List.map fst p.ids in
        let ls xs="[" ^ String.concat ", " (List.map quoted xs) ^ "]" in
        List.iter (fun n -> symbols.(!next)<-n; incr next) (ids @ (if p.wider then p.labels else [p.key;p.value]));
        if p.wider then Printf.sprintf ".pipe(rp_wider, %s, %s, %s, %s)" (ls ids) (quoted p.key) (quoted p.value) (ls p.labels)
        else Printf.sprintf ".unpivot(%s, index=%s, variable_name=%s, value_name=%s)" (ls p.labels) (ls ids) (quoted p.key) (quoted p.value)
    | Table.Rename changes ->
        let pair (a,b,i) = symbols.(i) <- b; quoted a ^ ": " ^ quoted b in
        ".rename({" ^ String.concat ", " (List.map pair changes) ^ "})"
    | Window (n,x,keys) ->
        let v = emit x in symbols.(!next) <- n; incr next;
        ".with_columns(" ^ v ^ ".over([" ^ String.concat ", " (List.map (fun (k,_) -> quoted k) keys) ^ "]).alias(" ^ quoted n ^ "))"
    | Table.Take n -> Printf.sprintf ".head(%d)" n
    | Distinct keys -> ".unique(subset=[" ^ String.concat ", " (List.map emit keys) ^ "])"
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
