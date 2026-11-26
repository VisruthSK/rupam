type origin =
  | Default
  | Global of string
  | Project_file of string
  | Session of string

type 'a setting =
  { value : 'a
  ; origin : origin
  }

type t =
  { root : string option
  ; target : Target.t option setting
  ; preview_digits : int setting
  ; strict : bool setting
  }

let file = "rupam.toml"
let default value = { value; origin = Default }

let standalone =
  { root = None
  ; target = default None
  ; preview_digits = default 2
  ; strict = default true
  }
;;

let global_file () =
  let env name =
    match Sys.getenv_opt name with
    | Some "" -> None
    | value -> value
  in
  let base =
    if Sys.win32
    then env "APPDATA"
    else (
      match env "XDG_CONFIG_HOME" with
      | Some path when not (Filename.is_relative path) -> Some path
      | _ -> Option.map (fun home -> Filename.concat home ".config") (env "HOME"))
  in
  Option.map (fun dir -> Filename.concat (Filename.concat dir "rupam") file) base
;;

let options t =
  let row name display setting =
    let origin =
      match setting.origin with
      | Default -> "default"
      | Global path -> "global " ^ path
      | Project_file path -> "project " ^ path
      | Session source -> "session " ^ source
    in
    name, display setting.value, origin
  in
  let part f = Option.fold ~none:"unset" ~some:f in
  [ row
      "target.language"
      (part (fun (t : Target.t) -> Target.language_name t.language))
      t.target
  ; row
      "target.dialect"
      (part (fun (t : Target.t) -> Target.dialect_name t.dialect))
      t.target
  ; row "check.strict" string_of_bool t.strict
  ; row "preview.digits" string_of_int t.preview_digits
  ]
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
      Diagnostic.fail
        Invalid_config
        (Printf.sprintf "%s:%d:%d: %s" path line column message)
  in
  let field section name parse inherited =
    let invalid expected =
      Diagnostic.fail
        Invalid_config
        (path ^ ": " ^ section ^ "." ^ name ^ " expects " ^ expected)
    in
    match Toml.Lenses.(get settings (key section)) with
    | None -> inherited
    | Some (Toml.Types.TTable fields) ->
      (match Toml.Lenses.(get fields (key name)) with
       | None -> inherited
       | Some raw -> { value = parse raw invalid; origin })
    | Some _ -> Diagnostic.fail Invalid_config (path ^ ": " ^ section ^ " must be a table")
  in
  let text section name names =
    field
      section
      name
      (fun raw invalid ->
         match raw with
         | Toml.Types.TString n when List.mem_assoc n names -> Some (List.assoc n names)
         | _ -> invalid (String.concat " or " (List.map fst names)))
      (default None)
  in
  let language = text "target" "language" Target.languages in
  let dialect = text "target" "dialect" Target.dialects in
  let target =
    let invalid message = Diagnostic.fail Invalid_config (path ^ ": " ^ message) in
    match language.value, dialect.value with
    | None, None -> previous.target
    | Some l, None ->
      (match Target.dialects_of l with
       | [ d ] -> { value = Target.make l d; origin }
       | _ -> invalid "target.language needs a target.dialect")
    | None, Some d -> { value = Target.make (Target.language_of d) d; origin }
    | Some l, Some d ->
      (match Target.make l d with
       | Some t -> { value = Some t; origin }
       | None ->
         invalid
           (Printf.sprintf
              "%s is not a dialect of %s"
              (Target.dialect_name d)
              (Target.language_name l)))
  in
  { previous with
    target
  ; strict =
      field
        "check"
        "strict"
        (fun raw invalid ->
           match raw with
           | Toml.Types.TBool b -> b
           | _ -> invalid "true or false")
        previous.strict
  ; preview_digits =
      field
        "preview"
        "digits"
        (fun raw invalid ->
           match raw with
           | Toml.Types.TInt n when n >= 0 && n <= 17 -> n
           | _ -> invalid "an integer from 0 to 17")
        previous.preview_digits
  }
;;

let load directory =
  let directory =
    match directory with
    | "." | "" -> Sys.getcwd ()
    | d when Filename.is_relative d -> Filename.concat (Sys.getcwd ()) d
    | d -> d
  in
  let base =
    match global_file () with
    | Some path when Sys.file_exists path -> read path (Global path) standalone
    | _ -> standalone
  in
  match locate directory with
  | None -> base
  | Some root ->
    let path = Filename.concat root file in
    read path (Project_file path) { base with root = Some root }
;;

let require_target project =
  match project.target.value with
  | Some d -> d
  | None ->
    Diagnostic.fail
      Missing_target
      "No target. Pass --target r-dplyr or python-polars, or set [target] language and \
       dialect in rupam.toml"
;;
