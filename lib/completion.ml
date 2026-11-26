type suggestion =
  { text : string
  ; detail : string
  }

let verbs =
  [ "filter", "condition, missing = skip | keep | error", [ "missing" ]
  ; "select", "columns to keep, in order", []
  ; "mutate", "name = value, ..., by = key", [ "by"; "missing"; "group_missing" ]
  ; "rename", "new = old", []
  ; "summarize", "name = reduction, ..., by = key", [ "by"; "missing"; "group_missing" ]
  ; "arrange", "key, desc(key)", []
  ; "slice_head", "n = rows of an arranged table", [ "n" ]
  ; "count", "columns to count rows by", []
  ; "distinct", "columns", []
  ; "left_join", "other, by = key, relationship = many_to_one", [ "by"; "relationship" ]
  ; "inner_join", "other, by = key, relationship = many_to_one", [ "by"; "relationship" ]
  ; "right_join", "other, by = key, relationship = one_to_many", [ "by"; "relationship" ]
  ; "full_join", "other, by = key, relationship = one_to_one", [ "by"; "relationship" ]
  ; "semi_join", "other, by = key", [ "by" ]
  ; "anti_join", "other, by = key", [ "by" ]
  ; ( "pivot_longer"
    , "cols = (a, b), names_to = k, values_to = v"
    , [ "cols"; "names_to"; "values_to" ] )
  ; ( "pivot_wider"
    , "names_from = k, values_from = v, levels = [..]"
    , [ "names_from"; "values_from"; "levels" ] )
  ; "print", "show the table", []
  ; "write_parquet", "path", []
  ]
;;

let verb name =
  List.find_map (fun (v, signature, _) -> if v = name then Some signature else None) verbs
;;


let choices = function
  | "missing" | "group_missing" -> [ "skip"; "keep"; "error" ]
  | "relationship" -> [ "one_to_one"; "many_to_one"; "one_to_many"; "many_to_many" ]
  | _ -> []
;;

let keywords = [ "let"; "match"; "type"; "fn"; "if"; "else"; "use"; "contract" ]

let identifier c =
  c = '_'
  || c = '$'
  || c = '^'
  || Char.lowercase_ascii c <> Char.uppercase_ascii c
  || (c >= '0' && c <= '9')
;;

let word_start line =
  let rec go i = if i > 0 && identifier line.[i - 1] then go (i - 1) else i in
  go (String.length line)
;;

let backward line from stop =
  let rec go i depth =
    if i < 0
    then None
    else (
      match line.[i] with
      | ')' | ']' | '}' -> go (i - 1) (depth + 1)
      | ('(' | '[' | '{') when depth > 0 -> go (i - 1) (depth - 1)
      | c when depth = 0 && stop line i c -> Some i
      | _ -> go (i - 1) depth)
  in
  go from 0
;;

let opening line from = backward line from (fun _ _ c -> c = '(' || c = '[' || c = '{')

let expression_start line from =
  match
    backward line from (fun line i c ->
      match c with
      | '(' | '[' | '{' | ',' -> true
      | '=' ->
        let before = if i > 0 then line.[i - 1] else ' ' in
        let after = if i + 1 < String.length line then line.[i + 1] else ' ' in
        after <> '=' && not (String.contains "=!<>" before)
      | _ -> false)
  with
  | Some i -> i + 1
  | None -> 0
;;

let trim_right s =
  let rec go n =
    if n > 0 && (s.[n - 1] = ' ' || s.[n - 1] = '\n') then go (n - 1) else n
  in
  String.sub s 0 (go (String.length s))
;;

let ends_with_pipe s = String.ends_with ~suffix:"|>" (trim_right s)

let piped session line pipe =
  let start = expression_start line (pipe - 1) in
  match Session.table session (String.trim (String.sub line start (pipe - start))) with
  | t -> Some t
  | exception _ -> None
;;

let columns (t : TransformIR.table) =
  List.map (fun (n, typ) -> { text = n; detail = Types.display typ }) t.schema
;;

let bindings session =
  List.map
    (fun (n, (b : Scope.binding), detail) ->
       let detail =
         match b with
         | Table_value _ -> "table"
         | _ -> detail
       in
       n, b, { text = n; detail })
    (Scope.bindings (Session.env session))
;;

let functions session =
  List.map (fun (text, detail) -> { text; detail }) (Elaborate.builtins ())
  @ List.filter_map
      (fun (_, (b : Scope.binding), s) ->
         match b with
         | Function _ -> Some s
         | _ -> None)
      (bindings session)
;;

let values session =
  List.filter_map
    (fun (_, (b : Scope.binding), s) ->
       match b with
       | Function _ -> None
       | _ -> Some s)
    (bindings session)
;;

let tables session =
  List.filter_map
    (fun (_, (b : Scope.binding), s) ->
       match b with
       | Table_value _ | Contract _ -> Some s
       | _ -> None)
    (bindings session)
;;

let files session prefix =
  let directory = Filename.concat (Session.directory session) (Filename.dirname prefix) in
  match Sys.readdir directory with
  | entries ->
    Array.to_list entries
    |> List.sort compare
    |> List.map (fun e ->
      let path =
        if Filename.dirname prefix = "." && not (String.starts_with ~prefix:"./" prefix)
        then e
        else Filename.concat (Filename.dirname prefix) e
      in
      { text = path
      ; detail =
          (if Sys.is_directory (Filename.concat directory e) then "directory" else "file")
      })
  | exception Sys_error _ -> []
;;

let option_name argument =
  if String.ends_with ~suffix:"=" argument
  then String.trim (String.sub argument 0 (String.length argument - 1))
  else ""
;;

let complete session line =
  let start = word_start line in
  let word = String.sub line start (String.length line - start) in
  let quotes = List.length (String.split_on_char '"' line) - 1 in
  let candidates, start, word =
    if quotes mod 2 = 1
    then (
      let quote = String.rindex line '"' + 1 in
      let prefix = String.sub line quote (String.length line - quote) in
      files session prefix, quote, prefix)
    else if start > 0 && line.[start - 1] = '.'
    then (
      let base = String.sub line 0 (start - 1) in
      let owner =
        String.sub base (word_start base) (String.length base - word_start base)
      in
      match Scope.lookup (Session.env session) owner with
      | Some (Contract contract) ->
        ( List.map
            (fun (t : Contract.table) -> { text = t.name; detail = "table" })
            contract.tables
        , start
        , word )
      | _ -> [], start, word)
    else if ends_with_pipe (String.sub line 0 start)
    then
      ( List.map (fun (v, d, _) -> { text = v; detail = d }) verbs
        @ List.filter (fun s -> String.contains s.detail '>') (functions session)
      , start
      , word )
    else (
      let inside =
        match opening line (start - 1) with
        | Some paren ->
          let callee = String.sub line 0 paren in
          let callee_start = word_start callee in
          let verb = String.sub callee callee_start (paren - callee_start) in
          let before = String.sub line 0 callee_start in
          (match List.find_opt (fun (v, _, _) -> v = verb) verbs with
           | Some (_, _, options) when ends_with_pipe before ->
             let pipe = String.length (trim_right before) - 2 in
             Some (options, piped session line pipe, paren)
           | _ -> None)
        | None -> None
      in
      let argument =
        match inside with
        | Some (_, _, paren) ->
          let from =
            match backward line (start - 1) (fun _ _ c -> c = ',') with
            | Some comma when comma > paren -> comma + 1
            | _ -> paren + 1
          in
          String.trim (String.sub line from (start - from))
        | None -> ""
      in
      match inside with
      | Some (options, _, _) when List.mem (option_name argument) options ->
        let option = option_name argument in
        List.map (fun c -> { text = c; detail = option }) (choices option), start, word
      | Some (options, table, _) ->
        ( Option.fold ~none:[] ~some:columns table
          @ List.map (fun o -> { text = o ^ " = "; detail = "option" }) options
          @ functions session
          @ values session
        , start
        , word )
      | None ->
        ( values session
          @ functions session
          @ List.map (fun k -> { text = k; detail = "keyword" }) keywords
        , start
        , word ))
  in
  let seen = Hashtbl.create 16 in
  ( start
  , List.filter
      (fun c ->
         let fresh = not (Hashtbl.mem seen c.text) in
         Hashtbl.replace seen c.text ();
         fresh && String.starts_with ~prefix:word c.text && c.text <> word)
      candidates )
;;
