open Syntax
module J = Yojson.Basic
module U = J.Util

let send value =
  let message = J.to_string value in
  Printf.printf "Content-Length: %d\r\n\r\n%s%!" (String.length message) message
;;

let read () =
  let rec headers length =
    let line = String.trim (input_line stdin) in
    if line = ""
    then length
    else (
      match String.index_opt line ':' with
      | Some i when String.lowercase_ascii (String.sub line 0 i) = "content-length" ->
        headers
          (Some
             (int_of_string
                (String.trim (String.sub line (i + 1) (String.length line - i - 1)))))
      | _ -> headers length)
  in
  match headers None with
  | Some n -> J.from_string (really_input_string stdin n)
  | None -> failwith "Missing Content-Length"
;;

let utf16 text first last =
  let rec count i units =
    if i >= last
    then units
    else (
      let u = String.get_utf_8_uchar text i in
      count
        (i + Uchar.utf_decode_length u)
        (units + if Uchar.to_int (Uchar.utf_decode_uchar u) > 0xffff then 2 else 1))
  in
  count first 0
;;

let position text (p : Lexing.position) =
  `Assoc
    [ "line", `Int (max 0 (p.pos_lnum - 1))
    ; "character", `Int (utf16 text p.pos_bol (min p.pos_cnum (String.length text)))
    ]
;;

let inspect text =
  let buf = Lexing.from_string text in
  let symbols = ref [] in
  let issue (a, b) message =
    [ `Assoc
        [ "range", `Assoc [ "start", position text a; "end", position text b ]
        ; "severity", `Int 1
        ; "source", `String "rupam"
        ; "message", `String message
        ]
    ]
  in
  let diagnostics =
    try
      let ds = Frontend.parse buf in
      let env =
        List.fold_left
          (fun env d -> fst (Elaborate.item env d))
          (Scope.empty ~directory:"." ~strict:false)
          ds
      in
      symbols
      := List.filter_map
           (fun (i : Syntax.item) ->
              match i.declaration with
              | Let (n, _, _) ->
                Option.map
                  (fun b -> n, Option.value (Scope.describe b) ~default:"")
                  (Scope.lookup env n)
              | _ -> None)
           ds;
      []
    with
    | Diagnostic.Error { span = Some span; message; _ } -> issue span message
    | Diagnostic.Error { span = None; message; _ } ->
      issue (buf.lex_start_p, buf.lex_curr_p) message
  in
  !symbols, diagnostics
;;

let run () =
  let docs = Hashtbl.create 8 in
  let publish uri version text =
    let symbols, ds = inspect text in
    Hashtbl.replace docs uri (text, symbols);
    send
      (`Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "textDocument/publishDiagnostics"
          ; ( "params"
            , `Assoc [ "uri", `String uri; "version", version; "diagnostics", `List ds ] )
          ])
  in
  let uri p = U.member "textDocument" p |> U.member "uri" |> U.to_string in
  let symbols p =
    match Hashtbl.find_opt docs (uri p) with
    | Some (_, xs) -> xs
    | None -> []
  in
  let rec loop () =
    let m = read () in
    let id = U.member "id" m
    and p = U.member "params" m in
    let reply result =
      send (`Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", result ])
    in
    match U.member "method" m |> U.to_string with
    | "exit" -> ()
    | method_ ->
      (match method_ with
       | "initialize" ->
         reply
           (`Assoc
               [ ( "capabilities"
                 , `Assoc [ "textDocumentSync", `Int 1; "completionProvider", `Assoc [] ]
                 )
               ])
       | "shutdown" -> reply `Null
       | "textDocument/didOpen" ->
         let d = U.member "textDocument" p in
         publish (uri p) (U.member "version" d) (U.member "text" d |> U.to_string)
       | "textDocument/didChange" ->
         (match List.rev (U.member "contentChanges" p |> U.to_list) with
          | c :: _ ->
            publish
              (uri p)
              (U.member "version" (U.member "textDocument" p))
              (U.member "text" c |> U.to_string)
          | [] -> ())
       | "textDocument/didClose" -> Hashtbl.remove docs (uri p)
       | "textDocument/completion" ->
         reply
           (`List
               (List.map
                  (fun (n, t) -> `Assoc [ "label", `String n; "detail", `String t ])
                  (symbols p)))
       | _ when id <> `Null ->
         send
           (`Assoc
               [ "jsonrpc", `String "2.0"
               ; "id", id
               ; ( "error"
                 , `Assoc [ "code", `Int (-32601); "message", `String "Unknown method" ] )
               ])
       | _ -> ());
      loop ()
  in
  try loop () with
  | End_of_file -> ()
;;
