open Rupam

type status =
  | Active
  | Pending

type outcome =
  { typ : string option
  ; value : string option
  ; diagnostic : string option
  }

type fixture =
  { id : string
  ; status : status
  ; source : string option
  ; expect : outcome
  }

type result =
  | Passed
  | Failed of string list
  | Unspecified

let load file =
  let document =
    match Toml.Parser.from_filename file with
    | `Ok document -> document
    | `Error (message, _) -> failwith (file ^ ": " ^ message)
  in
  let get path = Toml.Lenses.get document path in
  let expected name = get Toml.Lenses.(key "expect" |-- table |-- key name |-- string) in
  { id = Option.get (get Toml.Lenses.(key "id" |-- string))
  ; status =
      (match get Toml.Lenses.(key "status" |-- string) with
       | Some "active" -> Active
       | Some "pending" -> Pending
       | _ -> failwith (file ^ ": status must be active or pending"))
  ; source = get Toml.Lenses.(key "run" |-- table |-- key "source" |-- string)
  ; expect =
      { typ = expected "type"
      ; value = expected "value"
      ; diagnostic = expected "diagnostic"
      }
  }
;;

let evaluate source =
  match
    let declarations = Frontend.parse (Lexing.from_string source) in
    let _, typ =
      List.fold_left
        (fun (env, _) d ->
           let env, t = Infer.declaration env d in
           env, Some t)
        (Infer.empty, None)
        declarations
    in
    let _, value = Eval.run [] declarations in
    Option.map Infer.display typ, Option.map Eval.display value
  with
  | typ, value -> { typ; value; diagnostic = None }
  | exception exn ->
    let d = Diagnostic.of_exn exn in
    { typ = None; value = None; diagnostic = Some (Diagnostic.name d.code) }
;;

let check fixture =
  match fixture.source, fixture.expect with
  | None, _ | Some _, { typ = None; value = None; diagnostic = None } -> Unspecified
  | Some source, expect ->
    let outcome = evaluate source in
    let compare what expected actual =
      match expected with
      | Some e when Some e <> actual ->
        Some
          (Printf.sprintf
             "%s: expected %s, got %s"
             what
             e
             (match actual, outcome.diagnostic with
              | Some a, _ -> a
              | None, Some code -> "diagnostic " ^ code
              | None, None -> "nothing"))
      | _ -> None
    in
    (match
       List.filter_map
         Fun.id
         [ compare "type" expect.typ outcome.typ
         ; compare "value" expect.value outcome.value
         ; compare "diagnostic" expect.diagnostic outcome.diagnostic
         ]
     with
     | [] -> Passed
     | problems -> Failed problems)
;;

let () =
  let fixtures =
    Sys.readdir "conformance"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".toml")
    |> List.sort compare
    |> List.map (fun f -> load (Filename.concat "conformance" f))
  in
  let results = List.map (fun f -> f, check f) fixtures in
  let count p = List.length (List.filter p results) in
  let failures =
    List.filter_map
      (function
        | { id; status = Active; _ }, Failed problems ->
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
    (count (fun (f, _) -> f.status = Active))
    (count (fun (f, _) -> f.status = Pending))
    (count (fun (f, r) -> f.status = Pending && r <> Unspecified));
  List.iter prerr_endline failures;
  if
    List.exists
      (fun (f, r) ->
         (f.status = Active && r <> Passed) || (f.status = Pending && r = Passed))
      results
  then exit 1
;;
