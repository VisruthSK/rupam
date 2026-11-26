open Rupam
open Rupam_native

type status =
  | Active
  | Pending
  | Dialect of string list

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
       | Some "dialect" ->
         (match get Toml.Lenses.(key "dialects" |-- array |-- strings), get Toml.Lenses.(key "reason" |-- string) with
          | Some dialects, Some _ -> Dialect dialects
          | _ -> failwith (file ^ ": a dialect fixture lists dialects and a reason"))
       | _ -> failwith (file ^ ": status must be active, pending or dialect"))
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

let csv (names, rows) = String.concat "\n" (List.map (String.concat ",") (names :: rows))

let evaluate source =
  match
    let declarations = Frontend.parse (Lexing.from_string source) in
    let _, items =
      Elaborate.program (Scope.empty ~inputs:Native.inputs ~directory:"." ~strict:true ()) declarations
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

let cell = function
  | Arrow_result.Missing -> "None"
  | Unsupported -> "?"
  | Bool b -> Display.bool b
  | Int i -> Int64.to_string i
  | Float f -> Display.float f
  | Text s -> Printf.sprintf "%S" s
  | Date d -> Printf.sprintf {|d"%s"|} (Calendar.to_iso d)
;;

let rows session name =
  let result = Session.result session (Session.table session name) in
  let names = List.map fst (Session.table session name).schema in
  ( names
  , List.init (Session.total result) (fun row ->
      List.map (fun n -> cell (snd (Session.cell result ~row n))) names) )
;;


let on_session session source =
  match
    let items = Frontend.read source in
    let events = Session.submit session items in
    match List.rev items with
    | { declaration = Let (name, _, _); _ } :: _ ->
      (match Session.table session name with
       | _ -> Some (csv (rows session name))
       | exception Diagnostic.Error _ -> None)
    | _ ->
      List.fold_left
        (fun last -> function
           | Session.Value (text, _) -> Some text
           | _ -> last)
        None
        events
  with
  | value -> { typ = None; value; diagnostic = None; message = None }
  | exception exn ->
    let d = Diagnostic.of_exn exn in
    { typ = None
    ; value = None
    ; diagnostic = Some (Diagnostic.name d.code)
    ; message = Some d.message
    }
;;

let check ?session fixture =
  match fixture.source, fixture.expect with
  | None, _ | Some _, { typ = None; value = None; diagnostic = None; _ } -> Unspecified
  | Some source, expect ->
    let outcome =
      match session with
      | None -> evaluate source
      | Some session -> on_session session source
    in
    let expect = if session = None then expect else { expect with typ = None } in
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

let targets = [ "r-dplyr"; "python-polars" ]

let on_target name fixture =
  let session = Session.create ~project:Project.standalone ~directory:"." in
  Session.set_target session (List.assoc name Target.targets) ~origin:"fixture";
  Fun.protect ~finally:(fun () -> Session.close session) (fun () -> check ~session fixture)
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
