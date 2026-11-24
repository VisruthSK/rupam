open Rupam.Shape

let rejects f = match f () with
  | exception Error _ -> ()
  | _ -> failwith "expected an elaboration error"

let () =
  let x = input 0 { primitive = Float64; nullable = false; shape = Column 1 } in
  let square = binary Mul x x in
  assert (square.typ = x.typ);
  let centered = binary Sub x (reduce Mean x) in
  (match centered.node with
   | Binary (Sub, _, { node = Broadcast (1, _); _ }) -> ()
   | _ -> failwith "mean must broadcast over its input rows");
  let z = binary Div centered (reduce Sd x) in
  assert (z.typ = { x.typ with nullable = true });
  rejects (fun () -> filter 1 (binary Greater z x));
  filter 1 (binary Greater square x);
  rejects (fun () -> binary Mul x (input 1 { x.typ with shape = Column 2 }))

let () =
  let open Rupam in
  let source = "let square(x) = x*x; let normalize(y) = (y-mean(y))/sd(y); derive(a=square(revenue), b=normalize(a))" in
  match Frontend.parse (Lexing.from_string source) with
  | [f; g; Syntax.Evaluate { form = Call (_, args); _ }] ->
      let env = Check.define (Check.define [] f) g in
      let revenue = input 0 { primitive = Float64; nullable = false; shape = Column 1 } in
      let schema = Check.derive env ["revenue", revenue] args in
      assert ((List.assoc "a" schema).typ = revenue.typ);
      assert ((List.assoc "b" schema).typ.nullable)
  | _ -> failwith "unexpected program"

let () =
  let run s =
    let _, value = Rupam.Eval.run [] (Rupam.Frontend.parse (Lexing.from_string s)) in
    Option.map Rupam.Eval.display value in
  assert (run "let square(x) = x*x; square(3)" = Some "9");
  assert (run "if false then 9223372036854775807 + 1 else 3" = Some "3");
  assert (run "let a=2; let f(x)=x*a; let a=3; 4 |> f" = Some "8");
  match run "9223372036854775807 + 1" with
  | exception Rupam.Eval.Error (_, "Int64 overflow") -> ()
  | _ -> failwith "expected Int64 overflow"

let () =
  let open Rupam in
  let open Column_eval in
  let source = "let margin(x, y) = x - coalesce(y, 0.0); derive(m=margin(revenue,cost), z=(m-mean(m))/sd(m))" in
  let ty nullable = { primitive = Float64; nullable; shape = Column 1 } in
  let columns = ["revenue", Shape.input 0 (ty false); "cost", Shape.input 1 (ty true)] in
  match Frontend.parse (Lexing.from_string source) with
  | [f; Syntax.Evaluate { form = Call (_, args); _ }] ->
      let schema = Check.derive (Check.define [] f) columns args in
      let input = function 0 -> Many [| F 3.; F 5.; F 9. |]
        | 1 -> Many [| Missing; F 0.; F 2. |] | _ -> assert false in
      let result = evaluate ~rows:3 ~input (List.assoc "z" schema) in
      assert (result = Many [| F (-1.); F 0.; F 1. |]);
      assert (aggregate Sum [| Missing |] = Missing);
      assert (aggregate Sd [| F 3. |] = Missing)
  | _ -> assert false

let () =
  let open Rupam in
  let source = Table.source 1 ["id", Int64, false; "cost", Float64, true] in
  let plan s = match Frontend.parse (Lexing.from_string s) with
    | [Syntax.Evaluate x] -> Table.pipeline [] ["orders", source] x | _ -> assert false in
  let open Column_eval in
  let p = plan "orders |> derive(net = coalesce(cost, 0.0), tax = net * 0.5) |> filter(id > 1) |> select(net:tax)" in
  assert (Table.execute p ~rows:3 [ [| I 1L; I 2L; I 3L |]; [| F 8.; Missing; F 4. |] ]
    = (2, ["net", [| F 0.; F 4. |]; "tax", [| F 0.; F 2. |]]));
  rejects (fun () -> plan "orders |> filter(cost > 0.0)");
  rejects (fun () -> plan "orders |> derive(id = 2)");
  rejects (fun () -> plan "orders |> select(typo)")

let () =
  let open Rupam in
  let open Column_eval in
  let s = Table.source 1 ["x", Float64, true] in
  let plan code = match Frontend.parse (Lexing.from_string code) with
    | [Syntax.Evaluate x] -> Table.pipeline [] ["t",s] x | _ -> assert false in
  rejects (fun () -> plan "t |> take(1)");
  let rows,_ = Table.execute (plan "t |> distinct(x)") ~rows:5
    [[|Missing;F nan;Missing;F nan;F 3.|]] in
  assert (rows=3);
  assert (Table.execute (plan "t |> arrange(desc(x)) |> take(1)") ~rows:3
    [[|F 1.;Missing;F 5.|]] = (1,["x",[|F 5.|]]))


let () =
  let open Rupam in
  let ds = Frontend.parse (Lexing.from_string "let id(x)=x; let r={a=Some(3), b=[true,false]}; (r.a, id(r.b))") in
  ignore (List.fold_left (fun env d -> fst (Infer.declaration env d)) [] ds);
  let _,value = Eval.run [] ds in
  assert (Option.map Eval.display value = Some "(Some(3), [true, false])")

let () =
  let open Rupam in
  let ds = Frontend.parse (Lexing.from_string "let get(r)=r.x; (get({x=1}), get({y=2,x=true}))") in
  ignore (List.fold_left (fun env d -> fst (Infer.declaration env d)) [] ds);
  assert (Option.map Eval.display (snd (Eval.run [] ds)) = Some "(1, true)")

let () =
  let open Rupam in
  let src = "type Maybe<a> = | Empty | Just(value: a); let get(x)=match x { Empty => 0, Just(n) => n }; let answer: Int64=get(Just(42)); answer" in
  let ds = Frontend.parse (Lexing.from_string src) in
  ignore (List.fold_left (fun env d -> fst (Infer.declaration env d)) [] ds);
  assert (Option.map Eval.display (snd (Eval.run [] ds)) = Some "42")

let () =
  let open Rupam in
  let infer s =
    Frontend.parse (Lexing.from_string s)
    |> List.fold_left (fun env d -> fst (Infer.declaration env d)) [] in
  ignore (infer "let décaler(δ: Float64): Float64 = δ + 1.0; décaler(3.0)");
  (match infer "let bad(x: Int64): String=x" with
   | exception Infer.Error _ -> () | _ -> failwith "inconsistent annotations");
  let code = "let `price per unit` = 3; `price per unit`" in
  let pretty = Format.program (Frontend.parse (Lexing.from_string code)) in
  ignore (infer pretty)

let () =
  let open Rupam in
  let incomplete = "let t = sales |> filter(revenue > 0.0) |> derive(z = fn(" in
  match Frontend.valid_prefix incomplete with
  | Some (text,[Syntax.Let ("t",None,_)]) -> assert (text = "let t = sales |> filter(revenue > 0.0)")
  | _ -> failwith "Expected inspectable complete pipeline prefix"

let () =
  let open Rupam in
  let ds = Frontend.parse (Lexing.from_string "type Pair<a> = {x: a, y: a}; let f(p: Pair<Int64>)=match p { {x,y} => x+y }; f({x=2,y=3})") in
  ignore (List.fold_left (fun env d -> fst (Infer.declaration env d)) [] ds);
  assert (Option.map Eval.display (snd (Eval.run [] ds)) = Some "5")

let () =
  let open Rupam in
  let open Column_eval in
  let sales=Table.source 1 ["revenue",Float64,false;"id",Int64,false] in
  let plan src = match Frontend.parse (Lexing.from_string src) with
    | [Syntax.Evaluate e] -> Table.pipeline [] ["sales",sales] e | _ -> assert false in
  let p=plan "sales |> derive(a=revenue+1,b=a*2) |> select(b)" in
  assert (Table.execute p ~rows:1 [[|F 3.|];[|I 1L|]] = (1,["b",[|F 8.|]]));
  rejects (fun () -> plan "sales |> derive(b=revenue+id)");
  rejects (fun () -> plan "sales |> derive(b=revenue+9007199254740993)")

let () =
  let open Rupam in
  let src="let a = 3\n\nlet b = if a > 1\nthen 2\nelse 3\nb" in
  (match Frontend.parse (Lexing.from_string src) with
   | [_;Syntax.Let (_,_,{span=(a,b);_});Syntax.Evaluate {span=(c,_);_}] ->
       assert (a.pos_lnum=3 && b.pos_lnum=5 && c.pos_lnum=6)
   | _ -> assert false);
  match Frontend.parse (Lexing.from_string "let f(x)=x; f(1;2)") with
  | exception Parser.Error -> () | _ -> failwith "semicolon inside call must fail"
