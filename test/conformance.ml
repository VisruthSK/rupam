open Rupam

type status =
  | Active
  | Pending

type expectation =
  { typ : string option
  ; value : string option
  }

type fixture =
  { id : string
  ; status : status
  ; source : string
  ; expect : expectation
  }

let load file =
  let document =
    match Toml.Parser.from_filename file with
    | `Ok document -> document
    | `Error (message, _) -> failwith (file ^ ": " ^ message)
  in
  let get path = Toml.Lenses.get document path in
  { id = Option.get (get Toml.Lenses.(key "id" |-- string))
  ; status =
      (match get Toml.Lenses.(key "status" |-- string) with
       | Some "active" -> Active
       | Some "pending" -> Pending
       | _ -> failwith (file ^ ": status must be active or pending"))
  ; source =
      Option.value
        ~default:""
        (get Toml.Lenses.(key "run" |-- table |-- key "source" |-- string))
  ; expect =
      { typ = get Toml.Lenses.(key "expect" |-- table |-- key "type" |-- string)
      ; value = get Toml.Lenses.(key "expect" |-- table |-- key "value" |-- string)
      }
  }
;;

let evaluate source =
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
;;

let check fixture =
  match evaluate fixture.source with
  | exception exn -> Error (Printexc.to_string exn)
  | typ, value ->
    let mismatch what expected actual =
      match expected with
      | Some e when Some e <> actual ->
        Some
          (Printf.sprintf
             "%s: expected %s, got %s"
             what
             e
             (Option.value actual ~default:"nothing"))
      | _ -> None
    in
    (match
       List.filter_map
         Fun.id
         [ mismatch "type" fixture.expect.typ typ
         ; mismatch "value" fixture.expect.value value
         ]
     with
     | [] -> Ok ()
     | problems -> Error (String.concat "; " problems))
;;

let () =
  let fixtures =
    Sys.readdir "conformance"
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".toml")
    |> List.sort compare
    |> List.map (fun f -> load (Filename.concat "conformance" f))
  in
  let active = List.filter (fun f -> f.status = Active) fixtures in
  let failures =
    List.filter_map
      (fun f ->
         match check f with
         | Ok () -> None
         | Error message -> Some (f.id ^ ": " ^ message))
      active
  in
  Printf.printf
    "conformance: %d active, %d passing, %d pending\n"
    (List.length active)
    (List.length active - List.length failures)
    (List.length fixtures - List.length active);
  List.iter prerr_endline failures;
  if failures <> [] then exit 1
;;
