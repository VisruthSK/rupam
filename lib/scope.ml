module T = Types
module C = Core

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
  ; types : (string * T.scheme) list
  ; variants : (string * (string * T.scheme) list) list
  ; missing : C.policy option
  ; group_missing : C.policy option
  ; directory : string
  ; strict : bool
  }

let empty ~directory ~strict =
  { names = []
  ; types = []
  ; variants = []
  ; missing = None
  ; group_missing = None
  ; directory
  ; strict
  }
;;

let stamp = ref 0

let fresh_id name =
  incr stamp;
  { C.name; stamp = !stamp }
;;

let fail span code message = Diagnostic.fail ~span code message
let bind env name binding = { env with names = (name, binding) :: env.names }

let describe = function
  | Value { scheme; _ } -> Some (T.display scheme.body)
  | Table_value { table; _ } -> Some (T.display (T.Table (T.closed table.schema)))
  | Function { scheme = Some scheme; _ } -> Some (T.display scheme.body)
  | Function { template; _ } -> Some ("fn(" ^ String.concat ", " template.params ^ ")")
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
