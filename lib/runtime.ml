type language =
  | Python
  | R

type t =
  { command : string array
  ; prelude : string
  }

let of_dialect = function
  | Project.R_dplyr -> R
  | Python_polars -> Python
;;

let has root file = Sys.file_exists (Filename.concat root file)

let resolve (project : Project.t) language =
  match language, project.root with
  | Python, Some root when has root "uv.lock" || has root "pyproject.toml" ->
    { command = [| "uv"; "run"; "--project"; root; "python" |]; prelude = "" }
  | Python, _ -> { command = [| "python" |]; prelude = "" }
  | R, Some root when has root "renv.lock" ->
    { command = [| "Rscript" |]
    ; prelude = Printf.sprintf "renv::load(%s)\n" (Yojson.Basic.to_string (`String root))
    }
  | R, _ -> { command = [| "Rscript"; "--vanilla" |]; prelude = "" }
;;

let suffix = function
  | Python -> ".py"
  | R -> ".R"
;;

let run runtime language script =
  let file, channel = Filename.open_temp_file "rupam-" (suffix language) in
  Fun.protect
    ~finally:(fun () -> Sys.remove file)
    (fun () ->
       Fun.protect
         ~finally:(fun () -> close_out_noerr channel)
         (fun () -> output_string channel (runtime.prelude ^ script));
       let argv = Array.append runtime.command [| file |] in
       let pid = Unix.create_process argv.(0) argv Unix.stdin Unix.stdout Unix.stderr in
       match snd (Unix.waitpid [] pid) with
       | Unix.WEXITED 0 -> ()
       | _ -> Diagnostic.fail Host_failure (argv.(0) ^ " exited with an error"))
;;
