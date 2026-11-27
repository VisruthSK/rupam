module T = Types
module C = TransformIR

type template =
  { params : string list
  ; body : Syntax.expr
  ; closure : env
  }

and binding =
  | Value of
      { id : C.id
      ; scheme : T.scheme
      }
  | Table_value of
      { id : C.id
      ; table : C.table
      }
  | Function of
      { id : C.id option
      ; scheme : T.scheme option
      ; template : template
      }
  | Constructor of
      { scheme : T.scheme
      ; arity : int
      }
  | Inline of C.expr
  | Contract of Contract.t
  | Module of string

and env =
  { names : (string * binding) list
  ; inputs : inputs
  ; types : (string * T.scheme) list
  ; variants : (string * (string * T.scheme) list) list
  ; missing : C.policy option
  ; group_missing : C.policy option
  ; directory : string
  ; strict : bool
  ; supply : int ref
  }

and inputs =
  { columns : string -> Input.column list
  ; digest : string -> string
  ; contract : string -> Contract.t
  }

let unresolved path =
  Diagnostic.fail
    Unreadable_input
    ("The compiler was given no way to read " ^ path ^ "; resolve inputs before checking")
;;

let offline = { columns = unresolved; digest = unresolved; contract = unresolved }

let empty ?(inputs = offline) ~directory ~strict () =
  { names = []
  ; inputs
  ; types = []
  ; variants = []
  ; missing = None
  ; group_missing = None
  ; directory
  ; strict
  ; supply = ref 0
  }
;;

let fresh_id env name =
  incr env.supply;
  { C.name; stamp = !(env.supply) }
;;

let fail span code message = Diagnostic.fail ~span code message
let bind env name binding = { env with names = (name, binding) :: env.names }

let table_parameters template =
  let piped = ref [] in
  let rec visit (x : Syntax.expr) =
    (match x.form with
     | Pipe ({ form = Name (Bare, p); _ }, _) -> piped := p :: !piped
     | _ -> ());
    ignore
      (Syntax.map
         (fun child ->
            visit child;
            child)
         x)
  in
  visit template.body;
  List.filter (fun p -> List.mem p !piped) template.params
;;

let describe = function
  | Value { scheme; _ } -> Some (T.display scheme.body)
  | Table_value { table; _ } -> Some (T.display (T.Table (T.closed table.schema)))
  | Function { scheme = Some scheme; _ } -> Some (T.display scheme.body)
  | Function { template; _ } ->
    let tables = table_parameters template in
    let role p = p ^ ": " ^ if List.mem p tables then "Table" else "column" in
    Some
      ("("
       ^ String.concat ", " (List.map role template.params)
       ^ ") -> "
       ^ if tables = [] then "column" else "Table")
  | Constructor { scheme; _ } -> Some (T.display scheme.body)
  | Contract _ -> Some "contract"
  | Module m -> Some ("module " ^ m)
  | Inline _ -> None
;;

let bindings env =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (n, b) ->
       if Hashtbl.mem seen n
       then None
       else (
         Hashtbl.add seen n ();
         Option.map (fun d -> n, b, d) (describe b)))
    env.names
  |> List.rev
;;

let lookup env name = List.assoc_opt name env.names

let rec origin env (t : C.table) =
  match t.source with
  | Read f -> Filename.basename f.path ^ ", schema read from the file"
  | Load { table; contract; _ } ->
    Printf.sprintf "%s in %s" table.name (Filename.basename contract.path)
  | Bound id ->
    (match
       List.find_map
         (function
           | _, Table_value { id = bound; table } when bound.stamp = id.stamp ->
             Some table
           | _ -> None)
         env.names
     with
     | Some table -> origin env table
     | None -> id.name)
  | Join j ->
    Printf.sprintf
      "%s join of %s and %s on %s"
      (C.join_name j.kind)
      (origin env j.left)
      (origin env j.right)
      j.key
;;
