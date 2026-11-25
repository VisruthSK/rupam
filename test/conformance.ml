open Rupam_fixture

let () =
  let results =
    List.map (fun (f : Fixture.t) -> f, Fixture.check f) (Fixture.all "conformance")
  in
  let count p = List.length (List.filter p results) in
  let failures =
    List.filter_map
      (function
        | { Fixture.id; status = Active; _ }, Fixture.Failed problems ->
          Some (id ^ ": " ^ String.concat "; " problems)
        | { id; status = Active; _ }, Unspecified ->
          Some (id ^ ": no program or expectation")
        | { id; status = Pending; _ }, Passed -> Some (id ^ ": now passes; mark it active")
        | { id; status = Pending; _ }, Failed problems when Array.mem "--pending" Sys.argv
          -> Some (id ^ " (pending): " ^ String.concat "; " problems)
        | _ -> None)
      results
  in
  Printf.printf
    "conformance: %d active, %d pending (%d runnable)\n"
    (count (fun (f, _) -> f.Fixture.status = Active))
    (count (fun (f, _) -> f.status = Pending))
    (count (fun (f, r) -> f.status = Pending && r <> Fixture.Unspecified));
  List.iter prerr_endline failures;
  if
    List.exists
      (fun ((f : Fixture.t), r) ->
         (f.status = Active && r <> Fixture.Passed) || (f.status = Pending && r = Passed))
      results
  then exit 1
;;
