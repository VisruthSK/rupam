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
