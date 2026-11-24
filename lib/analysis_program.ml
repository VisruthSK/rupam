open Syntax

type dataset = { dictionary : Contract.t; table : Contract.table; fields : string list; plan : Table.t }
type binding = Dataset of dataset | Dictionary of Contract.t | Function of string list * expr * environment
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
        {dictionary=dict;table;fields=List.map (fun (n,_,_) -> n) fields;plan=Table.source 0 fields}
    | Binary ("|>",a,{form=Call ({form=Name (Bare,verb);_},args);_}) ->
        let t = dataset env scalars a in
        {t with plan=Table.stage scalars t.plan verb args}
    | Call ({form=Name (Bare,f);_},[None,arg]) ->
        (match List.assoc_opt f env with
         | Some (Function ([p],body,closure)) -> dataset ((p,Dataset (dataset env scalars arg))::closure) scalars body
         | _ -> fail x ("Unsupported table function " ^ f))
    | _ -> fail x "Expected table expression" in
  let rec loop env scalars outputs = function
    | [] -> env, List.rev outputs
    | Contract (path,n)::ds ->
        let path = if Filename.is_relative path then Filename.concat directory path else path in
        loop ((n,Dictionary (Contract.import path))::env) scalars outputs ds
    | (Let (n,Some ps,body) as d)::ds ->
        loop ((n,Function (ps,body,env))::env) (Check.define scalars d) outputs ds
    | Let (n,None,x)::ds -> loop ((n,Dataset (dataset env scalars x))::env) scalars outputs ds
    | Evaluate {form=Call ({form=Name (Bare,"save_parquet");_},[None,x;None,{form=Literal path;_}]);_}::ds ->
        let path = Yojson.Basic.from_string path |> Yojson.Basic.Util.to_string in
        loop env scalars ((dataset env scalars x,path)::outputs) ds
    | Evaluate x::_ -> fail x "Expected save_parquet" in
  loop [] [] [] declarations

let compile ~directory ds = snd (analyze ~directory ds)

let preamble = {|import json
import shutil
import subprocess
import tempfile
from pathlib import Path

import polars as pl


def dictionary_json(command, path):
    result = subprocess.run(["data-dict", *command, str(path)], capture_output=True)
    if result.returncode:
        raise ValueError(f"data-dict {command[0]} failed")
    return json.loads(result.stdout)


def validated_source(dictionary, expected, name):
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
            if report["status"] != "ok" or any(s["outcome"] != "pass" for s in report["steps"]):
                raise ValueError(f"Incomplete {level} validation")
        return pl.read_parquet(paths[name]).lazy()

|}

let python ~directory declarations =
  let outputs = compile ~directory declarations in
  let q = Polars.quoted in
  let emit (d,path) =
    if List.exists (fun (c : Contract.column) -> c.restricted) d.table.columns then
      failwith "Export of a source containing restricted fields requires explicit authorization";
    let expected = Yojson.Basic.to_string d.dictionary.metadata in
    let source = Printf.sprintf "validated_source(%s, %s, %s)" (q d.dictionary.path) (q expected) (q d.table.name) in
    Printf.sprintf "(\n    %s\n    %s\n    .collect()\n    .write_parquet(%s)\n)\n" source
      (Polars.lower ~fields:d.fields d.plan) (q path) in
  preamble ^ String.concat "\n" (List.map emit outputs)


let run_python script =
  let file, channel = Filename.open_temp_file "rupam-" ".py" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () ->
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel script);
    let pid = Unix.create_process "python" [|"python";file|] Unix.stdin Unix.stdout Unix.stderr in
    match snd (Unix.waitpid [] pid) with Unix.WEXITED 0 -> () | _ -> failwith "Polars execution failed")


let bound env name = match List.assoc_opt name env with
  | Some (Dataset d) -> d | _ -> failwith ("Unknown bound table " ^ name)

let schema d =
  Table.names d.plan |> List.iter (fun n ->
    let t = (List.assoc n d.plan.schema).typ in
    let primitive = match t.primitive with Shape.Float64 -> "Float64" | Int64 -> "Int64"
      | Bool -> "Bool" | String -> "String" in
    Printf.printf "%s : %s%s\n" n primitive (if t.nullable then "?" else ""));
  print_endline "Contract valid; values pending"

let preview d =
  if List.exists (fun (c : Contract.column) -> c.restricted) d.table.columns then
    failwith "Preview of restricted data requires authorization";
  let q = Polars.quoted in
  let plan = Polars.lower ~fields:d.fields d.plan in
  let script = Printf.sprintf "%s\nprint((validated_source(%s, %s, %s)\n    %s\n).head(20).collect())"
    preamble (q d.dictionary.path) (q (Yojson.Basic.to_string d.dictionary.metadata))
    (q d.table.name) plan in
  print_endline (if d.plan.ordered then "Ordered preview" else "Unordered preview");
  run_python script
