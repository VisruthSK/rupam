module Json = Yojson.Basic
module J = Json.Util

let list key json =
  match J.member key json with
  | `Null -> []
  | value -> J.to_list value
;;

let import path =
  let command = "data-dict" in
  let channel = Unix.open_process_args_in command [| command; "export-spec"; path |] in
  let result =
    try Ok (In_channel.input_all channel) with
    | exn -> Error exn
  in
  match Unix.close_process_in channel, result with
  | Unix.WEXITED 0, Ok text -> Contract.decode path (Json.from_string text)
  | _, Error exn -> raise exn
  | _ -> Diagnostic.fail Contract_failure "data-dict export-spec failed"
;;

let validate path =
  List.iter
    (fun level ->
       let command = "data-dict" in
       let channel =
         Unix.open_process_args_in command [| command; level; "--json"; path |]
       in
       let report = In_channel.input_all channel in
       ignore (Unix.close_process_in channel);
       let json =
         try Json.from_string report with
         | Yojson.Json_error _ ->
           Diagnostic.fail Contract_failure ("data-dict " ^ level ^ " failed on " ^ path)
       in
       let problems =
         list "problems" json
         |> List.filter_map (fun p -> J.member "message" p |> J.to_string_option)
       in
       let failed =
         list "steps" json
         |> List.filter (fun s -> J.member "outcome" s <> `String "pass")
         |> List.filter_map (fun s -> J.member "name" s |> J.to_string_option)
       in
       if J.member "status" json <> `String "ok" || failed <> []
       then
         Diagnostic.fail
           Contract_failure
           (Printf.sprintf
              "%s does not match its data: %s"
              (Filename.basename path)
              (String.concat "; " (problems @ failed))))
    [ "validate-meta"; "validate-data" ]
;;
