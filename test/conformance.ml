open Rupam_fixture

let targets = Fixture.targets
let on_target = Fixture.on_target

let triage fixtures =
  let groups = Hashtbl.create 16 in
  List.iter
    (fun (f : Fixture.t) ->
       match f.status, f.source with
       | Pending, Some source ->
         let outcome = Fixture.evaluate source in
         let key = Option.value outcome.diagnostic ~default:"wrong or missing value" in
         Hashtbl.replace
           groups
           key
           (f.id :: Option.value (Hashtbl.find_opt groups key) ~default:[])
       | Pending, None ->
         let key = "needs a program" in
         Hashtbl.replace
           groups
           key
           (f.id :: Option.value (Hashtbl.find_opt groups key) ~default:[])
       | _ -> ())
    fixtures;
  Hashtbl.to_seq groups
  |> List.of_seq
  |> List.sort (fun (_, a) (_, b) -> compare (List.length b) (List.length a))
  |> List.iter (fun (key, ids) ->
    Printf.printf
      "%3d  %-28s %s\n"
      (List.length ids)
      key
      (String.concat " " (List.rev ids)));
  []
;;

let reference fixtures =
  let results = List.map (fun (f : Fixture.t) -> f, Fixture.check f) fixtures in
  let count p = List.length (List.filter p results) in
  Printf.printf
    "conformance: %d active, %d pending (%d runnable)\n"
    (count (fun (f, _) -> f.Fixture.status <> Pending))
    (count (fun (f, _) -> f.status = Pending))
    (count (fun (f, r) -> f.status = Pending && r <> Fixture.Unspecified));
  List.filter_map
    (function
      | { Fixture.id; status = Active | Dialect _; _ }, Fixture.Failed problems ->
        Some (id ^ ": " ^ String.concat "; " problems)
      | { id; status = Active | Dialect _; _ }, Unspecified ->
        Some (id ^ ": no program or expectation")
      | { id; status = Pending; _ }, Passed -> Some (id ^ ": now passes; mark it active")
      | { id; status = Pending; _ }, Failed problems when Array.mem "--pending" Sys.argv
        ->
        print_endline (id ^ " (pending): " ^ String.concat "; " problems);
        None
      | _ -> None)
    results
;;

let on_targets fixtures =
  let runs =
    List.concat_map
      (fun (f : Fixture.t) ->
         List.filter_map
           (fun name ->
              match f.status with
              | Active -> Some (name, f)
              | Dialect listed when List.mem name listed -> Some (name, f)
              | Dialect _ | Pending -> None)
           targets)
      fixtures
  in
  let failures =
    List.filter_map
      (fun (name, (f : Fixture.t)) ->
         match on_target name f with
         | Failed problems ->
           Some (Printf.sprintf "%s on %s: %s" f.id name (String.concat "; " problems))
         | Passed | Unspecified -> None)
      runs
  in
  Printf.printf "conformance on targets: %d fixture runs\n" (List.length runs);
  failures
;;

let () =
  let fixtures = Fixture.all "conformance" in
  let failures =
    if Array.mem "--triage" Sys.argv
    then triage fixtures
    else if Array.mem "--targets" Sys.argv
    then on_targets fixtures
    else reference fixtures
  in
  List.iter prerr_endline failures;
  if failures <> [] then exit 1
;;
