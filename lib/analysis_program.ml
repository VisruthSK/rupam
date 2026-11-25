open Syntax

type join_kind =
  | Inner
  | Left
  | Right
  | Full
  | Semi
  | Anti

let join_name = function
  | Inner -> "inner"
  | Left -> "left"
  | Right -> "right"
  | Full -> "full"
  | Semi -> "semi"
  | Anti -> "anti"
;;

let join_kinds =
  List.map (fun k -> join_name k ^ "_join", k) [ Inner; Left; Right; Full; Semi; Anti ]
;;

type relationship =
  | One_to_one
  | Many_to_one
  | One_to_many
  | Many_to_many

let relationships =
  [ "one_to_one", One_to_one
  ; "many_to_one", Many_to_one
  ; "one_to_many", One_to_many
  ; "many_to_many", Many_to_many
  ]
;;

type dataset =
  { origin : origin
  ; fields : string list
  ; plan : Table.t
  }

and origin =
  | Contracted of contracted
  | File of source
  | Joined of join

and contracted =
  { dictionary : Contract.t
  ; table : Contract.table
  ; source : source
  }

and source =
  { path : string
  ; digest : string
  ; format : Data_file.format
  ; columns : (string * Shape.primitive * bool) list
  }

and join =
  { left : dataset
  ; right : dataset
  ; key : string
  ; kind : join_kind
  ; relationship : relationship option
  ; covered : bool
  }

type binding =
  | Dataset of dataset
  | Dictionary of Contract.t
  | Function of string list * expr * environment * Check.env

and environment = (string * binding) list

let fail x msg = raise (Infer.Error (x.span, msg))
let readers = [ "read_csv"; "read_tsv"; "read_parquet" ]

let rec uses table x =
  let any = List.exists (uses table) in
  match x.form with
  | Name (_, n) -> table n
  | Call ({ form = Name (Bare, f); _ }, _) when List.mem f readers -> true
  | Call (f, args) -> uses table f || any (List.map snd args)
  | Pipe (a, b) | Binary (_, a, b) | Range (a, b) -> any [ a; b ]
  | Local (n, a, b) -> uses table a || uses (fun m -> m <> n && table m) b
  | Formula (a, b) -> any (Option.to_list a @ [ b ])
  | If (a, b, c) -> any [ a; b; c ]
  | Match (a, arms) ->
    any (a :: List.concat_map (fun arm -> arm.body :: Option.to_list arm.guard) arms)
  | Member (a, b) -> any [ a; b ]
  | Interpolate pieces ->
    any
      (List.filter_map
         (function
           | Slot s -> Some s.value
           | Text_piece _ -> None)
         pieces)
  | Tuple xs | List xs -> any xs
  | Record fs -> any (List.map snd fs)
  | Field (a, _) | Unary (_, a) | Annotated (a, _) -> uses table a
  | Lambda (ps, a) -> uses (fun m -> (not (List.mem m ps)) && table m) a
  | Literal _ -> false
;;

let reads = uses (fun _ -> false)

let tabular =
  List.exists (function
    | Contract _ -> true
    | Let (_, _, x) | Evaluate x -> reads x
    | Alias _ | Type _ | Import _ | Setting _ -> false)
;;

let physical path =
  List.map
    (fun (c : Data_file.column) ->
       ( c.name
       , match c.physical with
         | "BIGINT" -> Some Shape.Int64
         | "DOUBLE" -> Some Float64
         | "BOOLEAN" -> Some Bool
         | "VARCHAR" -> Some String
         | _ -> None ))
    (Data_file.columns path)
;;

let kind = function
  | Shape.Int64 -> "Int64"
  | Float64 -> "Float64"
  | Bool -> "Boolean"
  | String -> "String"
;;

let rec dataset ~directory env scalars x =
  match x.form with
  | Name (Bare, n) ->
    (match List.assoc_opt n env with
     | Some (Dataset t) -> t
     | _ -> fail x ("Unknown table " ^ n))
  | Call
      ( { form = Name (Bare, "load"); _ }
      , [ (None, { form = Field ({ form = Name (Bare, d); _ }, n); _ }) ] ) ->
    let dict =
      match List.assoc_opt d env with
      | Some (Dictionary d) -> d
      | _ -> fail x "Expected contract"
    in
    let table =
      match List.find_opt (fun (t : Contract.table) -> t.name = n) dict.tables with
      | Some t -> t
      | None -> fail x ("Unknown contracted table " ^ n)
    in
    let path =
      match table.parquet with
      | Some p -> p
      | None -> fail x "Missing Parquet source"
    in
    let types = physical path in
    let fields =
      List.map
        (fun (c : Contract.column) ->
           let t =
             match List.assoc_opt c.name types with
             | Some (Some t) -> t
             | Some None -> fail x ("Unsupported physical type for " ^ c.name)
             | None -> fail x ("Contract column " ^ c.name ^ " is missing from the file")
           in
           c.name, t, not c.required)
        table.columns
    in
    { origin =
        Contracted
          { dictionary = dict
          ; table
          ; source =
              { path; digest = Data_file.digest path; format = Parquet; columns = fields }
          }
    ; fields = List.map (fun (n, _, _) -> n) fields
    ; plan =
        (let plan = Table.source 0 fields in
         let primary =
           List.filter_map
             (fun (c : Contract.column) -> if c.primary then Some c.name else None)
             table.columns
         in
         let unique =
           List.filter_map
             (fun (c : Contract.column) ->
                if c.unique then Some (Table.key plan [ c.name ]) else None)
             table.columns
         in
         { plan with
           keys = (if primary = [] then [] else [ Table.key plan primary ]) @ unique
         })
    }
  | Call
      ( { form = Name (Bare, (("read_csv" | "read_parquet" | "read_tsv") as reader)); _ }
      , [ (None, { form = Literal (Text path); _ }) ] ) ->
    let path =
      if Filename.is_relative path then Filename.concat directory path else path
    in
    let format = Data_file.format path in
    let expected =
      match reader with
      | "read_csv" -> Data_file.Csv
      | "read_tsv" -> Data_file.Tsv
      | _ -> Data_file.Parquet
    in
    if format <> expected then fail x (reader ^ " cannot read " ^ Filename.basename path);
    let columns =
      List.map
        (fun (c : Data_file.column) ->
           let primitive =
             match c.physical with
             | "BIGINT" | "INTEGER" -> Shape.Int64
             | "DOUBLE" -> Float64
             | "BOOLEAN" -> Bool
             | "VARCHAR" -> String
             | other ->
               fail x (Printf.sprintf "Column %s has unsupported type %s" c.name other)
           in
           c.name, primitive, c.nullable)
        (try Data_file.columns path with
         | Failure message -> fail x message)
    in
    { origin = File { path; digest = Data_file.digest path; format; columns }
    ; fields = List.map (fun (n, _, _) -> n) columns
    ; plan = Table.source 0 columns
    }
  | Pipe
      ( a
      , { form = Call ({ form = Name (Bare, verb); _ }, (None, b) :: options); _ } )
    when List.mem_assoc verb join_kinds ->
    let left = dataset ~directory env scalars a
    and right = dataset ~directory env scalars b in
    let kind = List.assoc verb join_kinds in
    let key, right_key =
      match List.assoc_opt (Some "by") options with
      | Some { form = Name (_, k); _ } -> k, k
      | Some
          { form = Binary (Equal, { form = Name (_, l); _ }, { form = Name (_, r); _ })
          ; _
          } -> l, r
      | _ -> fail x (verb ^ " needs by = key, or by = (left_key == right_key)")
    in
    let covered =
      match List.assoc_opt (Some "unmatched") options, kind with
      | None, _ | Some { form = Name (_, "allow"); _ }, _ -> false
      | Some { form = Name (_, "error"); _ }, (Semi | Anti) ->
        fail x ("unmatched = error is not available on " ^ verb)
      | Some { form = Name (_, "error"); _ }, _ -> true
      | Some _, _ -> fail x "unmatched is allow or error"
    in
    let declared =
      match List.assoc_opt (Some "relationship") options with
      | Some { form = Name (_, name); _ } when List.mem_assoc name relationships ->
        Some (List.assoc name relationships)
      | Some _ ->
        fail x "relationship is one_to_one, many_to_one, one_to_many or many_to_many"
      | None -> None
    in
    (match
       List.find_opt
         (fun (n, _) -> n <> Some "by" && n <> Some "relationship" && n <> Some "unmatched")
         options
     with
     | Some (Some n, _) -> fail x (verb ^ " takes no option named " ^ n)
     | Some (None, _) -> fail x (verb ^ " takes one table")
     | None -> ());
    let nullable_left = kind = Right || kind = Full
    and nullable_right = kind = Left || kind = Full in
    let right =
      if key = right_key
      then right
      else
        { right with
          plan =
            Table.stage
              scalars
              right.plan
              "rename"
              [ Some key, { x with form = Name (Bare, right_key) } ]
        }
    in
    let left_unique = Table.unique left.plan [ key ]
    and right_unique = Table.unique right.plan [ key ] in
    let relationship =
      match declared, kind with
      | Some r, _ -> Some r
      | None, (Semi | Anti) -> None
      | None, (Left | Inner) when right_unique -> Some Many_to_one
      | None, Right when left_unique -> Some One_to_many
      | None, Full when left_unique && right_unique -> Some One_to_one
      | None, _ ->
        fail
          x
          (verb
           ^ " needs relationship = one_to_one, many_to_one, one_to_many or \
              many_to_many: no key proves how many rows match on "
           ^ key)
    in
    let at_most_one_right =
      match relationship with
      | Some (One_to_one | Many_to_one) -> true
      | _ -> kind = Semi || kind = Anti
    and at_most_one_left =
      match relationship with
      | Some (One_to_one | One_to_many) -> true
      | _ -> false
    in
    let lk = Check.lookup key left.plan.schema
    and rk = Check.lookup key right.plan.schema in
    if lk.typ.primitive <> rk.typ.primitive then fail x "Join key types differ";
    let fields =
      List.map
        (fun (n, (e : Shape.expr)) ->
           ( n
           , e.typ.primitive
           , e.typ.nullable
             || if n = key then rk.typ.nullable && nullable_left else nullable_left ))
        left.plan.schema
      @ List.filter_map
          (fun (n, (e : Shape.expr)) ->
             if n = key || kind = Semi || kind = Anti
             then None
             else (
               if List.mem_assoc n left.plan.schema
               then fail x ("Join column collision: " ^ n);
               Some (n, e.typ.primitive, e.typ.nullable || nullable_right)))
          right.plan.schema
    in
    let plan = Table.source 0 fields in
    let keys =
      (if at_most_one_right && kind <> Right && kind <> Full then left.plan.keys else [])
      @ if at_most_one_left && (kind = Right || kind = Inner) then right.plan.keys else []
    in
    { origin = Joined { left; right; key; kind; relationship; covered }
    ; fields = List.map (fun (n, _, _) -> n) fields
    ; plan =
        { plan with
          keys =
            List.filter_map
              (fun (k : Table.key) ->
                 if List.for_all (fun c -> List.mem_assoc c plan.schema) k.columns
                 then Some (Table.key plan k.columns)
                 else None)
              keys
        }
    }
  | Pipe (a, ({ form = Call ({ form = Name (Bare, verb); _ }, args); _ } as stage)) ->
    let t = dataset ~directory env scalars a in
    (try { t with plan = Table.stage ~span:x.span scalars t.plan verb args } with
     | Shape.Error message -> fail stage message)
  | Call ({ form = Name (Bare, f); _ }, [ (None, arg) ]) ->
    (match List.assoc_opt f env with
     | Some (Function ([ p ], body, closure, captured)) ->
       dataset
         ~directory
         ((p, Dataset (dataset ~directory env scalars arg)) :: closure)
         captured
         body
     | _ -> fail x ("Unsupported table function " ^ f))
  | _ -> fail x "Expected table expression"
;;

type output =
  | Write of string
  | Print

let printed = function
  | Syntax.Evaluate
      { form =
          ( Call ({ form = Name (Bare, "print"); _ }, [ (None, x) ])
          | Pipe (x, { form = Call ({ form = Name (Bare, "print"); _ }, []); _ }) )
      ; _
      } -> Some x
  | _ -> None
;;

let extend ?(strict = true) ~directory env scalars declarations =
  let rec loop env scalars outputs = function
    | [] -> env, scalars, List.rev outputs
    | Import ("polars", _) :: _ when strict ->
      failwith
        "import polars is target-specific. Set strict = false under [check] in \
         rupam.toml, or pass --no-strict"
    | Import ("polars", alias) :: ds ->
      loop env ((alias, Check.Module "polars") :: scalars) outputs ds
    | Import (n, _) :: _ -> failwith ("Unknown module " ^ n)
    | Contract (path, n) :: ds ->
      let path =
        if Filename.is_relative path then Filename.concat directory path else path
      in
      loop ((n, Dictionary (Contract.import path)) :: env) scalars outputs ds
    | (Let (n, Some ps, body) as d) :: ds ->
      loop
        ((n, Function (ps, body, env, scalars)) :: env)
        (Check.define scalars d)
        outputs
        ds
    | (Let (_, None, { form = Literal _; _ }) as d) :: ds ->
      loop env (Check.define scalars d) outputs ds
    | Let (n, None, x) :: ds ->
      loop ((n, Dataset (dataset ~directory env scalars x)) :: env) scalars outputs ds
    | Evaluate
        { form =
            Call
              ( { form = Name (Bare, "write_parquet"); _ }
              , [ (None, x); (None, { form = Literal (Text path); _ }) ] )
        ; _
        }
      :: ds ->
      loop env scalars ((dataset ~directory env scalars x, Write path) :: outputs) ds
    | d :: ds when Option.is_some (printed d) ->
      let x = Option.get (printed d) in
      loop env scalars ((dataset ~directory env scalars x, Print) :: outputs) ds
    | Alias _ :: ds -> loop env scalars outputs ds
    | Setting (name, value) :: ds ->
      let setting =
        match name with
        | "group_missing" -> Check.Group_missing
        | _ -> Missing
      in
      let policy = Check.policy_of_name value in
      loop env ((Check.setting_key setting, Check.Default (setting, policy)) :: scalars) outputs ds
    | Type _ :: _ -> failwith "Table backend does not support variant declarations"
    | Evaluate x :: _ ->
      fail
        x
        "A table program's top-level expressions are print(table) or \
         write_parquet(table, path)"
  in
  loop env scalars [] declarations
;;

let analyze ?strict ~directory declarations =
  let env, _, outputs = extend ?strict ~directory [] [] declarations in
  env, outputs
;;

let compile ?strict ~directory ds = snd (analyze ?strict ~directory ds)

let rec restrictions d =
  let source =
    match d.origin with
    | Contracted { table; _ } ->
      List.filter_map
        (fun (c : Contract.column) -> if c.restricted then Some c.name else None)
        table.columns
    | File _ -> []
    | Joined { left; right; key; _ } ->
      let hidden = restrictions left @ restrictions right in
      if List.mem key hidden then d.fields else hidden
  in
  let private_ids = Array.make d.plan.next false in
  List.iteri (fun i n -> private_ids.(i) <- List.mem n source) d.fields;
  let next = ref (List.length d.fields)
  and private_rows = ref false in
  let rec private_expr (v : Shape.expr) =
    match v.node with
    | Input i -> private_ids.(i)
    | Constant _ -> false
    | Choose (c, l, r) -> private_expr c || private_expr l || private_expr r
    | Null -> false
    | Binary (_, l, r) | Coalesce (l, r) -> private_expr l || private_expr r
    | Apply (_, xs) -> List.exists private_expr xs
    | Member (x, xs) -> List.exists private_expr (x :: xs)
    | Convert x -> private_expr x
    | Text parts ->
      List.exists
        (function
          | Shape.Shown (x, _) -> private_expr x
          | Fixed _ -> false)
        parts
    | Count -> false
    | Broadcast (_, x) | Reduce (_, _, x) | Polars_sqrt x | Unary (_, x) | Required x ->
      private_expr x
  in
  let bind private_ =
    private_ids.(!next) <- private_;
    incr next
  in
  List.iter
    (function
      | Table.Pivot p ->
        List.iter (fun (_, x) -> bind (private_expr x)) p.ids;
        let hidden = List.exists (fun (_, x) -> private_expr x) p.values in
        if p.wider
        then List.iter (fun _ -> bind hidden) p.labels
        else (
          bind false;
          bind hidden)
      | Table.Rename _ -> ()
      | Window (_, v, keys) ->
        bind (private_expr v || List.exists (fun (_, x) -> private_expr x) keys)
      | Table.Bind (_, v) -> bind (private_expr v)
      | Summarize (keys, vs) ->
        if List.exists (fun (_, x) -> private_expr x) keys then private_rows := true;
        List.iter (fun (_, x) -> bind (private_expr x)) keys;
        List.iter (fun (_, x) -> bind (Option.fold ~none:false ~some:private_expr x)) vs
      | Arrange keys ->
        if List.exists (fun (x, _) -> private_expr x) keys then private_rows := true
      | Filter x -> if private_expr x then private_rows := true
      | Distinct xs -> if List.exists private_expr xs then private_rows := true
      | Select _ | Take _ | Assert_key _ -> ())
    (List.rev d.plan.steps);
  List.filter_map
    (fun (n, x) -> if !private_rows || private_expr x then Some n else None)
    d.plan.schema
;;

let public_view d =
  let hidden = restrictions d in
  if hidden = []
  then d
  else (
    let schema = List.filter (fun (n, _) -> not (List.mem n hidden)) d.plan.schema in
    if schema = [] then failwith "No unrestricted fields to display";
    { d with
      plan =
        { d.plan with schema; steps = Table.Select (List.map fst schema) :: d.plan.steps }
    })
;;

module P = Python_syntax

let rec python_stages d =
  let base =
    match d.origin with
    | File source | Contracted { source; _ } ->
      P.call
        "read_locked"
        [ P.Str source.path
        ; P.Str source.digest
        ; P.Str (Data_file.format_name source.format)
        ; P.Dict (List.map (fun (n, t, _) -> P.Str n, P.Str (kind t)) source.columns)
        ]
    | Joined { left; right; key; kind; relationship; covered } ->
      P.Chain
        ( (if covered
           then
             P.call
               "rp_covered"
               [ python_table left; python_table right; P.Str key; P.Str (join_name kind) ]
           else python_table left)
        , [ P.link
              "join"
              ([ P.Positional (python_table right)
               ; P.Keyword ("on", P.Str key)
               ; P.Keyword ("how", P.Str (join_name kind))
               ]
               @ (match relationship with
                  | None -> []
                  | Some r ->
                    [ P.Keyword
                        ( "validate"
                        , P.Str
                            (match r with
                             | One_to_one -> "1:1"
                             | Many_to_one -> "m:1"
                             | One_to_many -> "1:m"
                             | Many_to_many -> "m:m") )
                    ])
               @ [ P.Keyword ("nulls_equal", P.Bool false)
                 ; P.Keyword ("coalesce", P.Bool true)
                 ])
          ; P.link "select" [ P.Positional (P.names d.fields) ]
          ] )
  in
  base, Polars.lower ~fields:d.fields d.plan

and python_table d =
  let base, links = python_stages d in
  P.Chain (base, links)
;;

let python ?strict ~directory declarations =
  P.Verbatim Adapters.polars
  :: List.map
       (fun (d, output) ->
          let base, links = python_stages (public_view d) in
          let collected = P.Chain (base, links @ [ P.link "collect" [] ]) in
          P.Expression
            (match output with
             | Write path ->
               P.Chain
                 ( base
                 , links
                   @ [ P.link "collect" []
                     ; P.link "write_parquet" [ P.Positional (P.Str path) ]
                     ] )
             | Print -> P.call "print" [ collected ]))
       (compile ?strict ~directory declarations)
;;

let bound env name =
  match List.assoc_opt name env with
  | Some (Dataset d) -> d
  | _ -> failwith ("Unknown bound table " ^ name)
;;

let rec origin d =
  match d.origin with
  | Contracted { dictionary; table; _ } ->
    Printf.sprintf "%s in %s" table.name (Filename.basename dictionary.path)
  | File { path; _ } -> Filename.basename path ^ ", schema read from the file"
  | Joined { left; right; key; kind; _ } ->
    Printf.sprintf
      "%s join of %s and %s on %s"
      (join_name kind)
      (origin left)
      (origin right)
      key
;;

let schema d =
  let names = Table.names d.plan in
  let width = List.fold_left (fun w n -> max w (String.length n)) 0 names in
  List.iter
    (fun n ->
       Printf.printf
         "%-*s  %s\n"
         width
         n
         (Shape.type_name (List.assoc n d.plan.schema).typ))
    names;
  Printf.printf "%d columns from %s\n" (List.length names) (origin d)
;;

let preview_page ~runtime d ~offset ~limit =
  let d = public_view d in
  let page =
    P.meth
      (P.meth
         (python_table d)
         "slice"
         [ P.Positional (P.Int (string_of_int offset))
         ; P.Positional (P.Int (string_of_int limit))
         ])
      "collect"
      []
  in
  print_endline (if d.plan.order <> None then "Ordered preview" else "Unordered preview");
  Worker.run
    runtime
    Runtime.Python
    (fst
       (P.program [ P.Verbatim Adapters.polars; P.Expression (P.call "print" [ page ]) ]))
;;

let explain d =
  let line = ref 0 in
  let say text =
    incr line;
    Printf.printf "%2d  %s\n" !line text
  in
  let rec visit d =
    (match d.origin with
     | Contracted { dictionary; table; _ } ->
       say
         (Printf.sprintf
            "load %s from %s, checking the bytes rupam lock validated"
            table.name
            (Filename.basename dictionary.path))
     | File { path; format; _ } ->
       say
         (Printf.sprintf
            "read %s as %s, checking the bytes rupam lock recorded"
            (Filename.basename path)
            (Data_file.format_name format))
     | Joined { left; right; key; kind; _ } ->
       visit left;
       visit right;
       say (Printf.sprintf "%s join on %s" (join_name kind) key));
    List.rev d.plan.steps
    |> List.iter (fun step ->
      say
        (match step with
         | Table.Pivot p -> if p.wider then "pivot wider" else "pivot longer"
         | Window (n, _, keys) ->
           Printf.sprintf "mutate %s by %s" n (String.concat ", " (List.map fst keys))
         | Rename changes ->
           "rename "
           ^ String.concat ", " (List.map (fun (a, b, _) -> a ^ " to " ^ b) changes)
         | Filter _ -> "filter rows"
         | Bind (n, _) -> "mutate " ^ n
         | Select ns -> "select " ^ String.concat ", " ns
         | Take n -> Printf.sprintf "take the first %d rows" n
         | Summarize (keys, measures) ->
           "summarize "
           ^ String.concat ", " (List.map fst measures)
           ^ if keys = [] then "" else " by " ^ String.concat ", " (List.map fst keys)
         | Arrange _ -> "arrange"
         | Distinct _ -> "distinct rows"
         | Assert_key columns -> "check that " ^ String.concat ", " columns ^ " is a key"))
  in
  visit d
;;

let rec sources d =
  match d.origin with
  | Contracted { dictionary; source; _ } -> [ dictionary.path; source.path ]
  | File source -> [ source.path ]
  | Joined { left; right; _ } -> sources left @ sources right
;;

let rec files d =
  match d.origin with
  | File source -> [ source ]
  | Contracted _ -> []
  | Joined { left; right; _ } -> files left @ files right
;;

let rec contracts d =
  match d.origin with
  | Contracted { dictionary; _ } -> [ dictionary.path ]
  | File _ -> []
  | Joined { left; right; _ } -> contracts left @ contracts right
;;

let validate d =
  List.iter Contract.validate (List.sort_uniq compare (contracts d));
  print_endline "The data matches its contract"
;;
