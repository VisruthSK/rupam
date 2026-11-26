open Rupam
open Rupam_native

type parameter =
  | Text of string
  | Choice of string * string list

type tool =
  { name : string
  ; description : string
  ; parameters : parameter list
  ; call : (string * string) list -> Yojson.Safe.t
  }

exception Invalid_params

let version = "2026-07-28"
let version_key = "io.modelcontextprotocol/protocolVersion"
let supported = `List [ `String version ]
let text s = `String s
let obj fields = `Assoc fields
let empty = obj []

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None
;;

let complete fields =
  obj
    (("resultType", text "complete")
     :: ( "_meta"
        , obj
            [ ( "io.modelcontextprotocol/serverInfo"
              , obj [ "name", text "rupam-dev"; "version", text Version.version ] )
            ] )
     :: fields)
;;

let ok id body = obj [ "jsonrpc", text "2.0"; "id", id; "result", body ]

let error id code message data =
  obj
    [ "jsonrpc", text "2.0"
    ; "id", id
    ; "error", obj ([ "code", `Int code; "message", text message ] @ data)
    ]
;;

let schema tool =
  let properties =
    List.map
      (function
        | Text key -> key, obj [ "type", text "string" ]
        | Choice (key, options) ->
          key, obj [ "type", text "string"; "enum", `List (List.map text options) ])
      tool.parameters
  in
  obj
    [ "name", text tool.name
    ; "description", text tool.description
    ; "annotations", obj [ "readOnlyHint", `Bool true ]
    ; ( "inputSchema"
      , obj
          [ "type", text "object"
          ; "properties", obj properties
          ; "required", `List (List.map (fun (key, _) -> text key) properties)
          ; "additionalProperties", `Bool false
          ] )
    ]
;;

let arguments tool = function
  | `Assoc fields when List.length fields = List.length tool.parameters ->
    List.map
      (fun parameter ->
         let key, valid =
           match parameter with
           | Text key -> key, fun _ -> true
           | Choice (key, choices) -> key, fun value -> List.mem value choices
         in
         match List.assoc_opt key fields with
         | Some (`String value) when valid value -> key, value
         | _ -> raise Invalid_params)
      tool.parameters
  | _ -> raise Invalid_params
;;

let result ~is_error content =
  let readable =
    match content with
    | `String s -> s
    | _ -> Yojson.Safe.pretty_to_string content
  in
  complete
    [ "content", `List [ obj [ "type", text "text"; "text", text readable ] ]
    ; "structuredContent", obj [ "value", content ]
    ; "isError", `Bool is_error
    ]
;;

let route tools method_ params =
  match method_ with
  | "server/discover" ->
    Some
      (complete
         [ "supportedVersions", supported; "capabilities", obj [ "tools", empty ] ])
  | "ping" -> Some (complete [])
  | "tools/list" -> Some (complete [ "tools", `List (List.map schema tools) ])
  | "tools/call" ->
    let tool =
      match member "name" params with
      | Some (`String name) ->
        (match List.find_opt (fun t -> t.name = name) tools with
         | Some tool -> tool
         | None -> raise Invalid_params)
      | _ -> raise Invalid_params
    in
    let args = arguments tool (Option.value (member "arguments" params) ~default:empty) in
    let reply =
      match tool.call args with
      | value -> result ~is_error:false value
      | exception exn ->
        let _, diagnostic = Protocol.diagnostic (Diagnostic.of_exn exn) in
        result ~is_error:true diagnostic
    in
    Some reply
  | _ -> None
;;

let respond tools line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error _ -> Some (error `Null (-32700) "Parse error" [])
  | request ->
    let method_ = member "method" request in
    let params = Option.value (member "params" request) ~default:empty in
    let id = Option.value (member "id" request) ~default:`Null in
    (match member "jsonrpc" request, method_, id with
     | Some (`String "2.0"), Some (`String _), `Null when member "id" request = None ->
       None
     | ( Some (`String "2.0")
       , Some (`String method_)
       , ((`Int _ | `Intlit _ | `String _) as id) ) ->
       let meta = member "_meta" params in
       let requested = Option.bind meta (member version_key) in
       let capabilities =
         Option.bind meta (member "io.modelcontextprotocol/clientCapabilities")
       in
       (match requested, capabilities with
        | Some (`String requested), _ when requested <> version ->
          Some
            (error
               id
               (-32022)
               ("Unsupported protocol version. Supported: " ^ version)
               [ "data", obj [ "supported", supported; "requested", text requested ] ])
        | Some (`String _), Some (`Assoc _) ->
          (match route tools method_ params with
           | Some response -> Some (ok id response)
           | None -> Some (error id (-32601) "Method not found" [])
           | exception Invalid_params ->
             Some (error id (-32602) "Invalid tool name or arguments" []))
        | _ ->
          Some
            (error
               id
               (-32602)
               ("Expected protocol version "
                ^ version
                ^ " and client capabilities in _meta")
               []))
     | _ -> Some (error `Null (-32600) "Invalid request" []))
;;

let run tools input output =
  let rec serve () =
    match In_channel.input_line input with
    | None -> ()
    | Some line ->
      Option.iter
        (fun reply ->
           Yojson.Safe.to_channel output reply;
           output_char output '\n';
           flush output)
        (respond tools line);
      serve ()
  in
  serve ()
;;
