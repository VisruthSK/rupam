type language =
  | R
  | Python

type dialect =
  | Dplyr
  | Polars

type t =
  { language : language
  ; dialect : dialect
  }

let language_of = function
  | Dplyr -> R
  | Polars -> Python
;;

let make language dialect =
  if language_of dialect = language then Some { language; dialect } else None
;;

let r_dplyr = { language = R; dialect = Dplyr }
let python_polars = { language = Python; dialect = Polars }
let all = [ r_dplyr; python_polars ]

let language_name = function
  | R -> "r"
  | Python -> "python"
;;

let dialect_name = function
  | Dplyr -> "dplyr"
  | Polars -> "polars"
;;

let name t = language_name t.language ^ "-" ^ dialect_name t.dialect
let languages = List.map (fun l -> language_name l, l) [ R; Python ]
let dialects = List.map (fun d -> dialect_name d, d) [ Dplyr; Polars ]
let targets = List.map (fun t -> name t, t) all

let dialects_of language =
  List.filter_map (fun t -> if t.language = language then Some t.dialect else None) all
;;

let yojson_of_t t = `String (name t)

let t_of_yojson = function
  | `String n when List.mem_assoc n targets -> List.assoc n targets
  | json -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "expected a target name" json
;;
