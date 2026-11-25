type dialect =
  | R_dplyr
  | Python_polars

let dialect_name = function
  | R_dplyr -> "r-dplyr"
  | Python_polars -> "python-polars"
;;

let dialects = List.map (fun d -> dialect_name d, d) [ R_dplyr; Python_polars ]

type origin = Default | Global of string | Project_file of string | Session of string
type 'a setting = { value : 'a; origin : origin }
type t =
  { root : string option
  ; dialect : dialect option setting
  ; preview_digits : int setting
  ; strict : bool setting
  }

let file = "rupam.toml"
let default value = { value; origin = Default }
let standalone =
  { root = None; dialect = default None; preview_digits = default 2; strict = default true }

let global_file () =
  let env name = match Sys.getenv_opt name with Some "" -> None | value -> value in
  let base =
    if Sys.win32 then env "APPDATA"
    else match env "XDG_CONFIG_HOME" with
      | Some path when not (Filename.is_relative path) -> Some path
      | _ -> Option.map (fun home -> Filename.concat home ".config") (env "HOME")
  in
  Option.map (fun dir -> Filename.concat (Filename.concat dir "rupam") file) base
;;

let options t =
  let row name display setting =
    let origin = match setting.origin with
      | Default -> "default"
      | Global path -> "global " ^ path
      | Project_file path -> "project " ^ path
      | Session source -> "session " ^ source
    in
    name, display setting.value, origin
  in
  [ row "target.dialect" (Option.fold ~none:"unset" ~some:dialect_name) t.dialect
  ; row "check.strict" string_of_bool t.strict
  ; row "preview.digits" string_of_int t.preview_digits ]
;;

let rec locate directory =
  if Sys.file_exists (Filename.concat directory file)
  then Some directory
  else (
    let parent = Filename.dirname directory in
    if parent = directory then None else locate parent)
;;

let read path origin previous =
  let settings =
    match Toml.Parser.from_filename path with
    | `Ok table -> table
    | `Error (message, { line; column; _ }) ->
      failwith (Printf.sprintf "%s:%d:%d: %s" path line column message)
  in
  let field section name parse inherited =
    let invalid expected = failwith (path ^ ": " ^ section ^ "." ^ name ^ " expects " ^ expected) in
    match Toml.Lenses.(get settings (key section)) with
    | None -> inherited
    | Some (Toml.Types.TTable fields) ->
      (match Toml.Lenses.(get fields (key name)) with
       | None -> inherited
       | Some raw -> { value = parse raw invalid; origin })
    | Some _ -> failwith (path ^ ": " ^ section ^ " must be a table")
  in
  { previous with
    dialect = field "target" "dialect" (fun raw invalid ->
      match raw with
      | Toml.Types.TString n when List.mem_assoc n dialects -> List.assoc_opt n dialects
      | _ -> invalid (String.concat " or " (List.map fst dialects))) previous.dialect
  ; strict = field "check" "strict" (fun raw invalid ->
      match raw with Toml.Types.TBool b -> b | _ -> invalid "true or false") previous.strict
  ; preview_digits = field "preview" "digits" (fun raw invalid ->
      match raw with Toml.Types.TInt n when n >= 0 && n <= 17 -> n
      | _ -> invalid "an integer from 0 to 17") previous.preview_digits
  }
;;

let load directory =
  let directory =
    match directory with
    | "." | "" -> Sys.getcwd ()
    | d when Filename.is_relative d -> Filename.concat (Sys.getcwd ()) d
    | d -> d
  in
  let base = match global_file () with
    | Some path when Sys.file_exists path -> read path (Global path) standalone
    | _ -> standalone
  in
  match locate directory with
  | None -> base
  | Some root ->
    let path = Filename.concat root file in
    read path (Project_file path) { base with root = Some root }
;;

let require_dialect project =
  match project.dialect.value with
  | Some d -> d
  | None ->
    failwith
      "No target dialect. Pass --target r-dplyr (or python-polars), or set [target] \
       dialect in rupam.toml"
;;
