open Syntax

type dataset = { dictionary : Contract.t; table : Contract.table; fields : string list; plan : Table.t; physical_types : Yojson.Basic.t; join : (dataset * dataset * string * string) option }
type binding = Dataset of dataset | Dictionary of Contract.t | Function of string list * expr * environment * Check.env
and environment = (string * binding) list

let fail x msg = raise (Infer.Error (x.span,msg))

let physical path =
  let script = "import json,polars as p,sys;print(json.dumps({k:str(v) for k,v in p.read_parquet_schema(sys.argv[1]).items()}))" in
  let ch = Unix.open_process_args_in "python" [|"python";"-c";script;path|] in
  let output = In_channel.input_all ch in
  match Unix.close_process_in ch with
  | Unix.WEXITED 0 -> Yojson.Basic.from_string output |> Yojson.Basic.Util.to_assoc
  | _ -> failwith "Cannot read Parquet schema"

let analyze ~directory declarations =
  let rec dataset env scalars x = match x.form with
    | Name (Bare,n) -> (match List.assoc_opt n env with Some (Dataset t) -> t | _ -> fail x ("Unknown table " ^ n))
    | Call ({form=Name (Bare,"load");_}, [None,{form=Field ({form=Name (Bare,d);_},n);_}]) ->
        let dict = match List.assoc_opt d env with Some (Dictionary d) -> d | _ -> fail x "Expected contract" in
        let table = match List.find_opt (fun (t : Contract.table) -> t.name=n) dict.tables with
          | Some t -> t | None -> fail x ("Unknown contracted table " ^ n) in
        let path = match table.parquet with Some p -> p | None -> fail x "Missing Parquet source" in
        let types = physical path in
        let fields = List.map (fun (c : Contract.column) ->
          let t = match List.assoc_opt c.name types with
            | Some (`String "Int64") -> Shape.Int64 | Some (`String "Float64") -> Float64
            | Some (`String "Boolean") -> Bool | Some (`String "String") -> String
            | _ -> fail x ("Unsupported physical type for " ^ c.name) in
          c.name,t,not c.required) table.columns in
        {dictionary=dict;table;fields=List.map (fun (n,_,_) -> n) fields;plan=Table.source 0 fields; physical_types=`Assoc types; join=None}
    | Binary ("|>",a,{form=Call ({form=Name (Bare,"join");_},(None,b)::options);_}) ->
        let left = dataset env scalars a and right = dataset env scalars b in
        let option n = match List.assoc_opt (Some n) options with
          | Some {form=Name (Bare,v);_} -> v | _ -> fail x ("join requires " ^ n) in
        let key,right_key = match List.assoc_opt (Some "on") options with
          | Some {form=Name (Bare,k);_} -> k,k
          | Some {form=Binary ("==",{form=Field ({form=Name (Bare,"left");_},l);_},{form=Field ({form=Name (Bare,"right");_},r);_});_} -> l,r
          | _ -> fail x "Expected a key name or left.key == right.key" in
        let how = option "how" in
        let right = if key=right_key then right else
          {right with plan=Table.stage scalars right.plan "rename" [Some right_key,{x with form=Name (Bare,key)}]} in
        if List.length options <> 2 || not (List.mem how ["left";"right";"inner";"full";"semi";"anti"]) then
          fail x "Expected on and how arguments with a supported join type";
        let lk = Check.lookup key left.plan.schema and rk = Check.lookup key right.plan.schema in
        if lk.typ.primitive <> rk.typ.primitive then fail x "Join key types differ";
        let fields = List.map (fun (n,(e : Shape.expr)) ->
          n,e.typ.primitive,(e.typ.nullable || (if n=key then rk.typ.nullable && List.mem how ["right";"full"]
            else List.mem how ["right";"full"]))) left.plan.schema @
          List.filter_map (fun (n,(e : Shape.expr)) -> if n=key || List.mem how ["semi";"anti"] then None else (
            if List.mem_assoc n left.plan.schema then fail x ("Join column collision: " ^ n);
            Some (n,e.typ.primitive,e.typ.nullable || List.mem how ["left";"full"]))) right.plan.schema in
        {left with fields=List.map (fun (n,_,_) -> n) fields;plan=Table.source 0 fields;
          join=Some (left,right,key,how)}
    | Binary ("|>",a,{form=Call ({form=Name (Bare,verb);_},args);_}) ->
        let t = dataset env scalars a in
        (try {t with plan=Table.stage scalars t.plan verb args}
         with Shape.Error message -> fail x message)
    | Call ({form=Name (Bare,f);_},[None,arg]) ->
        (match List.assoc_opt f env with
         | Some (Function ([p],body,closure,captured)) -> dataset ((p,Dataset (dataset env scalars arg))::closure) captured body
         | _ -> fail x ("Unsupported table function " ^ f))
    | _ -> fail x "Expected table expression" in
  let rec loop env scalars outputs = function
    | [] -> env, List.rev outputs
    | Import ("polars",alias)::ds -> loop env ((alias,Check.Module "polars")::scalars) outputs ds
    | Import (n,_)::_ -> failwith ("Unknown module " ^ n)
    | Contract (path,n)::ds ->
        let path = if Filename.is_relative path then Filename.concat directory path else path in
        loop ((n,Dictionary (Contract.import path))::env) scalars outputs ds
    | (Let (n,Some ps,body) as d)::ds ->
        loop ((n,Function (ps,body,env,scalars))::env) (Check.define scalars d) outputs ds
    | (Let (_,None,({form=Literal _;_})) as d)::ds -> loop env (Check.define scalars d) outputs ds
    | Let (n,None,x)::ds -> loop ((n,Dataset (dataset env scalars x))::env) scalars outputs ds
    | Evaluate {form=Call ({form=Name (Bare,"save_parquet");_},[None,x;None,{form=Literal path;_}]);_}::ds ->
        let path = Yojson.Basic.from_string path |> Yojson.Basic.Util.to_string in
        loop env scalars ((dataset env scalars x,path)::outputs) ds
    | Alias _::ds -> loop env scalars outputs ds
    | Type _::_ -> failwith "Table backend does not support variant declarations"
    | Evaluate x::_ -> fail x "Expected save_parquet" in
  loop [] [] [] declarations

let compile ~directory ds = snd (analyze ~directory ds)

let preamble = {|import json
import math
import operator
import shutil
import subprocess
import tempfile
from pathlib import Path

import polars as pl


def rp_checked(value):
    if type(value) is int and not -(1 << 63) <= value < (1 << 63):
        raise OverflowError("Int64 overflow")
    return value


def rp_abs(value):
    return None if value is None else rp_checked(abs(value))


def rp_binary(op, a, b):
    if a is None or b is None:
        return None
    if op == "div":
        if b == 0:
            if a == 0 or math.isnan(a):
                return math.nan
            return math.copysign(math.inf, math.copysign(1, a) * math.copysign(1, b))
        return a / b
    return rp_checked({"add": operator.add, "sub": operator.sub, "mul": operator.mul, "gt": operator.gt}[op](a, b))


def rp_wider(data, ids, key, value, labels):
    frame = data.collect()
    if frame.filter(pl.col(key).is_null() | ~pl.col(key).is_in(labels)).height:
        raise ValueError("Unexpected pivot key")
    return frame.lazy().pivot(key, labels, index=ids, values=value)


def dictionary_json(command, path):
    result = subprocess.run(["data-dict", *command, str(path)], capture_output=True)
    if result.returncode:
        raise ValueError(f"data-dict {command[0]} failed")
    return json.loads(result.stdout)


def validated_source(dictionary, expected, name, physical_types):
    original = Path(dictionary).resolve()
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        snapshot = root / original.name
        shutil.copyfile(original, snapshot)
        model = dictionary_json(["export-spec"], snapshot)
        if model != json.loads(expected):
            raise ValueError("Contract changed since compilation")
        paths = {}
        for table in model["tables"]:
            relative = Path(table["source"]["parquet"])
            if relative.is_absolute() or ".." in relative.parts:
                raise ValueError("Snapshot requires a dictionary-relative source")
            destination = root / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(original.parent / relative, destination)
            paths[table["name"]] = destination
        for level in ["validate-meta", "validate-data"]:
            report = dictionary_json([level, "--json"], snapshot)
            if report["run"]["tool"]["version"] != "0.0.3":
                raise ValueError("Unsupported data-dict validator version")
            if report["status"] != "ok" or any(s["outcome"] != "pass" for s in report["steps"]):
                raise ValueError(f"Incomplete {level} validation")
        schema = pl.read_parquet_schema(paths[name])
        if {k: str(v) for k, v in schema.items()} != physical_types:
            raise ValueError("Physical schema changed since compilation")
        fields = next(t["columns"] for t in model["tables"] if t["name"] == name)
        return pl.read_parquet(paths[name], columns=[c["name"] for c in fields]).lazy()

|}

let rec restrictions d =
  let source = match d.join with
    | None -> List.filter_map (fun (c : Contract.column) -> if c.restricted then Some c.name else None) d.table.columns
    | Some (left,right,key,_) ->
        let hidden = restrictions left @ restrictions right in
        if List.mem key hidden then d.fields else hidden in
  let private_ids = Array.make d.plan.next false in
  List.iteri (fun i n -> private_ids.(i) <- List.mem n source) d.fields;
  let next = ref (List.length d.fields) and private_rows = ref false in
  let rec private_expr (v : Shape.expr) = match v.node with
    | Input i -> private_ids.(i) | Constant _ -> false
    | Choose (c,l,r) -> private_expr c || private_expr l || private_expr r
    | Binary (_,l,r) | Coalesce (l,r) -> private_expr l || private_expr r
    | Broadcast (_,x) | Reduce (_,x) | Abs x | Polars_sqrt x -> private_expr x in
  let bind private_ = private_ids.(!next) <- private_; incr next in
  List.iter (function
    | Table.Pivot p ->
        List.iter (fun (_,x) -> bind (private_expr x)) p.ids;
        let hidden=List.exists (fun (_,x) -> private_expr x) p.values in
        if p.wider then List.iter (fun _ -> bind hidden) p.labels else (bind false; bind hidden)
    | Table.Rename _ -> ()
    | Window (_,v,keys) -> bind (private_expr v || List.exists (fun (_,x) -> private_expr x) keys)
    | Table.Bind (_,v) -> bind (private_expr v)
    | Summarize (keys,vs) ->
        if List.exists (fun (_,x) -> private_expr x) keys then private_rows := true;
        List.iter (fun (_,x) -> bind (private_expr x)) keys;
        List.iter (fun (_,x) -> bind (Option.fold ~none:false ~some:private_expr x)) vs
    | Arrange keys -> if List.exists (fun (x,_) -> private_expr x) keys then private_rows := true
    | Filter x -> if private_expr x then private_rows := true
    | Distinct xs -> if List.exists private_expr xs then private_rows := true
    | Select _ | Take _ -> ()) (List.rev d.plan.steps);
  List.filter_map (fun (n,x) -> if !private_rows || private_expr x then Some n else None) d.plan.schema

let public_view d =
  let hidden = restrictions d in
  if hidden=[] then d else
    let schema = List.filter (fun (n,_) -> not (List.mem n hidden)) d.plan.schema in
    if schema=[] then failwith "No unrestricted fields to display";
    { d with plan={d.plan with schema; steps=Table.Select (List.map fst schema)::d.plan.steps} }
let rec python_table d =
  let q = Polars.quoted in
  let base = match d.join with
    | None -> Printf.sprintf "validated_source(%s, %s, %s, %s)" (q d.dictionary.path)
        (q (Yojson.Basic.to_string d.dictionary.metadata)) (q d.table.name) (Yojson.Basic.to_string d.physical_types)
    | Some (l,r,k,h) -> Printf.sprintf "%s.join(%s, on=%s, how=%s, nulls_equal=False, coalesce=True).select([%s])"
        (python_table l) (python_table r) (q k) (q h) (String.concat ", " (List.map q d.fields)) in
  Printf.sprintf "(%s\n    %s\n)" base (Polars.lower ~fields:d.fields d.plan)

let python ~directory declarations =
  let emit (d,path) =
    let d = public_view d in
    Printf.sprintf "%s.collect().write_parquet(%s)\n" (python_table d) (Polars.quoted path) in
  preamble ^ String.concat "\n" (List.map emit (compile ~directory declarations))
let run_script ~command ~suffix script =
  let file, channel = Filename.open_temp_file "rupam-" suffix in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () ->
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel script);
    let pid = Unix.create_process command [|command;file|] Unix.stdin Unix.stdout Unix.stderr in
    match snd (Unix.waitpid [] pid) with Unix.WEXITED 0 -> () | _ -> failwith (command ^ " execution failed"))


let run_python = run_script ~command:"python" ~suffix:".py"

let bound env name = match List.assoc_opt name env with
  | Some (Dataset d) -> d | _ -> failwith ("Unknown bound table " ^ name)

let schema d =
  Table.names d.plan |> List.iter (fun n ->
    let t = (List.assoc n d.plan.schema).typ in
    let primitive = match t.primitive with Shape.Float64 -> "Float64" | Int64 -> "Int64"
      | Bool -> "Bool" | String -> "String" in
    Printf.printf "%s : %s%s\n" n primitive (if t.nullable then "?" else ""));
  print_endline "Contract valid; values pending"

let preview_page d ~offset ~limit =
  let d = public_view d in
  let script = preamble ^ "\nprint(" ^ python_table d ^ (Printf.sprintf ".slice(%d, %d).collect())" offset limit) in
  print_endline (if d.plan.ordered then "Ordered preview" else "Unordered preview");
  Worker.run "python" script





let preview d = preview_page d ~offset:0 ~limit:20

let explain d =
  print_endline "Validate a private snapshot before materialization (full source scan)";
  let rec visit d =
    (match d.join with None -> Printf.printf "load %s\n" d.table.name
     | Some (l,r,k,h) -> visit l; visit r; Printf.printf "%s join on %s\n" h k);
    List.rev d.plan.steps |> List.iter (fun step ->
      print_endline (match step with Table.Pivot p -> if p.wider then "pivot wider" else "pivot longer" | Table.Window (n,_,_) -> "grouped binding " ^ n | Rename _ -> "rename" | Table.Filter _ -> "filter" | Bind (n,_) -> "bind " ^ n
      | Select ns -> "select " ^ String.concat ", " ns | Take n -> Printf.sprintf "take %d" n
      | Summarize _ -> "summarize" | Arrange _ -> "arrange" | Distinct _ -> "distinct")) in
  visit d

let validate d =
  let rec sources d = match d.join with
    | None -> [d.dictionary.path] | Some (l,r,_,_) -> sources l @ sources r in
  List.sort_uniq compare (sources d) |> List.iter (fun path ->
    let script = preamble ^ "\nfor level in ['validate-meta','validate-data']:\n" ^
      "    report = dictionary_json([level, '--json'], " ^ Polars.quoted path ^ ")\n" ^
      "    if report['status'] != 'ok' or any(s['outcome'] != 'pass' for s in report['steps']):\n" ^
      "        raise ValueError('Validation incomplete')\n" in
    run_python script);
  print_endline "Validation passed. Materialization will validate its own snapshot."

