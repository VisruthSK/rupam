module P = Protocol

let expect session revision =
  let current = Session.revision session in
  if revision <> current
  then
    raise
      (P.Rejected
         ( Superseded
         , Printf.sprintf "Expected revision %d, the session is at %d" revision current ))
;;

let bindings session =
  `List
    (List.map
       (fun (name, _, description) ->
          `Assoc [ "name", `String name; "type", `String description ])
       (Scope.bindings (Session.env session)))
;;

let options session =
  `List
    (List.map
       (fun (name, value, origin) ->
          `Assoc
            [ "name", `String name; "value", `String value; "origin", `String origin ])
       (Project.options (Session.project session)))
;;

let capabilities () =
  `Assoc
    [ "protocol", `Int Version.protocol
    ; "version", `String Version.version
    ; "methods", `List (List.map (fun (name, _) -> `String name) P.methods)
    ; "targets", `List (List.map (fun (name, _) -> `String name) Target.targets)
    ; ( "diagnostics"
      , `List (List.map (fun c -> `String (Diagnostic.name c)) Diagnostic.all) )
    ]
;;

let handle session : P.request -> Yojson.Safe.t = function
  | Capabilities -> capabilities ()
  | Options -> options session
  | Version -> `String Version.version
  | Bindings -> bindings session
  | Check { source } ->
    `List
      (List.filter_map
         P.item
         (List.concat_map (Session.check session) (Frontend.read source)))
  | Complete { source } ->
    let start, suggestions = Completion.complete session source in
    `Assoc
      [ "start", `Int start
      ; ( "suggestions"
        , `List
            (List.map
               (fun (s : Completion.suggestion) ->
                  `Assoc [ "text", `String s.text; "detail", `String s.detail ])
               suggestions) )
      ]
  | Submit { revision; source } ->
    expect session revision;
    `List (List.map P.event (Session.submit session (Frontend.read source)))
  | Preview { source; offset; limit } ->
    P.page (Session.preview session (Session.table session source) ~offset ~limit)
  | Code { source; target } ->
    `String (Session.code session target (Session.table session source))
  | Retarget { target } ->
    Session.set_target session target ~origin:"session protocol";
    `String (Target.name target)
  | Save { path } ->
    (match path, Session.path session with
     | Some path, _ | None, Some path ->
       Session.save session path;
       `String path
     | None, None -> raise (P.Rejected (Protocol, "save needs a path the first time")))
  | Undo { revision } ->
    expect session revision;
    `Bool (Session.undo session)
  | Redo { revision } ->
    expect session revision;
    `Bool (Session.redo session)
  | Lock -> Lock.yojson_of_entry (Session.lock session)
;;

let error id category message data =
  `Assoc
    [ "jsonrpc", `String "2.0"
    ; "id", id
    ; ( "error"
      , `Assoc
          [ "code", `Int (if category = P.Protocol then -32602 else -32000)
          ; "message", `String message
          ; "data", data
          ] )
    ]
;;

let respond session line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error message ->
    error `Null Protocol message (`Assoc [ "category", `String "protocol" ])
  | json ->
    let field name =
      match json with
      | `Assoc fields -> Option.value (List.assoc_opt name fields) ~default:`Null
      | _ -> `Null
    in
    let id = field "id" in
    (match
       match field "method" with
       | `String name -> handle session (P.decode name (field "params"))
       | _ -> raise (P.Rejected (Protocol, "A request needs a method"))
     with
     | result ->
       `Assoc
         [ "jsonrpc", `String "2.0"
         ; "id", id
         ; ( "result"
           , `Assoc [ "revision", `Int (Session.revision session); "value", result ] )
         ]
     | exception P.Rejected (category, message) ->
       error
         id
         category
         message
         (`Assoc
             [ "category", `String (P.category_name category)
             ; "revision", `Int (Session.revision session)
             ])
     | exception Sys.Break ->
       error id Cancelled "Cancelled" (`Assoc [ "category", `String "cancelled" ])
     | exception exn ->
       let d = Diagnostic.of_exn exn in
       let category, data = P.diagnostic d in
       error id category d.message data)
;;

let run session input output =
  let rec loop () =
    match In_channel.input_line input with
    | None -> ()
    | Some line when String.trim line = "" -> loop ()
    | Some line ->
      Yojson.Safe.to_channel output (respond session line);
      output_char output '\n';
      flush output;
      loop ()
  in
  Fun.protect ~finally:(fun () -> Session.close session) loop
;;
