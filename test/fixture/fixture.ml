open Rupam

type status =
  | Active
  | Pending

type outcome =
  { typ : string option
  ; value : string option
  ; diagnostic : string option
  ; message : string option
  }

type t =
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
      ; message = None
      }
  }
;;

let all directory =
  Sys.readdir directory
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".toml")
  |> List.sort compare
  |> List.map (fun f -> load (Filename.concat directory f))
;;

let evaluate source =
  match
    let declarations = Frontend.parse (Lexing.from_string source) in
    let _, items =
      Elaborate.program (Scope.empty ~directory:"." ~strict:true) declarations
    in
    let env, shown = Reference.run [] items in
    match List.rev items with
    | TransformIR.Define (id, e) :: _ ->
      let render v =
        match Reference.rows v with
        | Some (names, rows) ->
          String.concat "\n" (List.map (String.concat ",") (names :: rows))
        | None -> Reference.display v
      in
      Some (Types.display e.typ), Option.map render (List.assoc_opt id.stamp env)
    | (Show e | Write (e, _)) :: _ ->
      ( Some (Types.display e.typ)
      , Option.map (fun (_, v) -> Reference.display v) (List.nth_opt (List.rev shown) 0) )
    | Variant _ :: _ | [] -> None, None
  with
  | typ, value -> { typ; value; diagnostic = None; message = None }
  | exception exn ->
    let d = Diagnostic.of_exn exn in
    { typ = None
    ; value = None
    ; diagnostic = Some (Diagnostic.name d.code)
    ; message = Some d.message
    }
;;

let check fixture =
  match fixture.source, fixture.expect with
  | None, _ | Some _, { typ = None; value = None; diagnostic = None; _ } -> Unspecified
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
              | None, Some code ->
                "diagnostic " ^ code ^ Option.fold ~none:"" ~some:(( ^ ) ": ") outcome.message
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

let quote text =
  let escape = function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | c -> String.make 1 c
  in
  "\"" ^ String.concat "" (List.map escape (List.of_seq (String.to_seq text))) ^ "\""
;;

let observed outcome =
  let field name = Option.map (fun v -> name ^ " = " ^ quote v ^ "\n") in
  "[observed]\n"
  ^ String.concat
      ""
      (List.filter_map
         Fun.id
         [ field "type" outcome.typ
         ; field "value" outcome.value
         ; field "diagnostic" outcome.diagnostic
         ])
;;

let delimiter = String.make 3 '\''

let capture ~directory ~id source =
  let path = Filename.concat directory (id ^ ".toml") in
  let header =
    if Sys.file_exists path
    then (
      if (load path).source <> None then failwith (path ^ " already has a program");
      String.trim (In_channel.with_open_text path In_channel.input_all) ^ "\n")
    else "id = " ^ quote id ^ "\nstatus = \"pending\"\n"
  in
  if Astring.String.is_infix ~affix:delimiter source
  then failwith ("a fixture program cannot contain " ^ delimiter);
  let source = if String.ends_with ~suffix:"\n" source then source else source ^ "\n" in
  let outcome = evaluate source in
  Out_channel.with_open_text path (fun channel ->
    output_string
      channel
      (String.concat
         ""
         [ header
         ; "\n[run]\nsource = "
         ; delimiter
         ; "\n"
         ; source
         ; delimiter
         ; "\n\n"
         ; observed outcome
         ]));
  outcome
;;
