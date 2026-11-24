type dialect =
  | R_dplyr
  | Python_polars

let dialect_name = function
  | R_dplyr -> "r-dplyr"
  | Python_polars -> "python-polars"
;;

let dialects = List.map (fun d -> dialect_name d, d) [ R_dplyr; Python_polars ]

type t =
  { root : string option
  ; dialect : dialect option
  ; strict : bool
  }

let file = "rupam.toml"
let standalone = { root = None; dialect = None; strict = true }

let rec locate directory =
  if Sys.file_exists (Filename.concat directory file)
  then Some directory
  else (
    let parent = Filename.dirname directory in
    if parent = directory then None else locate parent)
;;

let read root =
  let path = Filename.concat root file in
  let settings =
    match Toml.Parser.from_filename path with
    | `Ok table -> table
    | `Error (message, { line; column; _ }) ->
      failwith (Printf.sprintf "%s:%d:%d: %s" path line column message)
  in
  let dialect =
    Option.map
      (fun name ->
         match List.assoc_opt name dialects with
         | Some d -> d
         | None ->
           failwith
             (Printf.sprintf
                "%s: unknown dialect %S. Expected one of: %s"
                path
                name
                (String.concat ", " (List.map fst dialects))))
      Toml.Lenses.(get settings (key "target" |-- table |-- key "dialect" |-- string))
  in
  { root = Some root
  ; dialect
  ; strict =
      Option.value
        Toml.Lenses.(get settings (key "check" |-- table |-- key "strict" |-- bool))
        ~default:true
  }
;;

let load directory =
  let directory =
    match directory with
    | "." | "" -> Sys.getcwd ()
    | d when Filename.is_relative d -> Filename.concat (Sys.getcwd ()) d
    | d -> d
  in
  Option.fold ~none:standalone ~some:read (locate directory)
;;

let require_dialect project =
  match project.dialect with
  | Some d -> d
  | None ->
    failwith
      "No target dialect. Pass --target r-dplyr (or python-polars), or set [target] \
       dialect in rupam.toml"
;;
