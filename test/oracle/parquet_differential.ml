let describe (c : Input.column) =
  match c.typ with
  | Supported t ->
    Printf.sprintf "%s %s%s" c.name (Types.display t) (if c.nullable then "?" else "")
  | Unsupported raw -> Printf.sprintf "%s unsupported(%s)" c.name raw
;;

let agrees (c : Input.column) expected =
  let field name = Yojson.Safe.Util.member name expected in
  String.equal c.name (Yojson.Safe.Util.to_string (field "name"))
  &&
  match c.typ, field "type" with
  | Supported t, `String kind ->
    String.equal (Types.display t) kind
    && Bool.equal c.nullable (Yojson.Safe.Util.to_bool (field "nullable"))
  | Unsupported _, `Null -> true
  | _ -> false
;;

let () =
  let dir = Sys.argv.(1) in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".parquet")
    |> List.sort String.compare
  in
  let failures = ref 0 in
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let ours = Rupam_native.Data_file.columns ~missing:Input.default_missing path in
       let expected =
         Yojson.Safe.Util.to_list
           (Yojson.Safe.from_file (Filename.remove_extension path ^ ".json"))
       in
       if
         List.length ours <> List.length expected
         || not (List.for_all2 agrees ours expected)
       then (
         incr failures;
         Printf.printf
           "%s\n  ours:    %s\n  pyarrow: %s\n"
           f
           (String.concat ", " (List.map describe ours))
           (String.concat ", " (List.map Yojson.Safe.to_string expected))))
    files;
  if !failures > 0
  then (
    Printf.printf
      "parquet differential: %d of %d files disagree\n"
      !failures
      (List.length files);
    exit 1)
  else
    Printf.printf
      "parquet differential: %d files agree with pyarrow\n"
      (List.length files)
;;
