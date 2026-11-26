open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request =
  | Capabilities
  | Options
  | Version
  | Bindings
  | Check of { source : string }
  | Complete of { source : string }
  | Submit of
      { revision : int
      ; source : string
      }
  | Preview of
      { source : string
      ; offset : int
      ; limit : int
      }
  | Code of
      { source : string
      ; target : Target.t option
      }
  | Retarget of { target : Target.t }
  | Save of { path : string option }
  | Undo of { revision : int }
  | Redo of { revision : int }
  | Lock

type category =
  | Protocol
  | Diagnostic
  | Execution
  | Cancelled
  | Superseded

exception Rejected of category * string

type source_params = { source : string } [@@deriving of_yojson]

type submit_params =
  { revision : int
  ; source : string
  }
[@@deriving of_yojson]

type preview_params =
  { source : string
  ; offset : int [@default 0]
  ; limit : int [@default 20]
  }
[@@deriving of_yojson]

type code_params =
  { source : string
  ; target : Target.t option [@yojson.option]
  }
[@@deriving of_yojson]

type target_params = { target : Target.t } [@@deriving of_yojson]
type save_params = { path : string option [@yojson.option] } [@@deriving of_yojson]
type revision_params = { revision : int } [@@deriving of_yojson]

let none = Fun.const

let methods : (string * (Yojson.Safe.t -> request)) list =
  [ "capabilities", none Capabilities
  ; "options", none Options
  ; "version", none Version
  ; "bindings", none Bindings
  ; ( "complete"
    , fun p ->
        let ({ source } : source_params) = source_params_of_yojson p in
        Complete { source } )
  ; ( "check"
    , fun p ->
        let ({ source } : source_params) = source_params_of_yojson p in
        Check { source } )
  ; ( "submit"
    , fun p ->
        let { revision; source } : submit_params = submit_params_of_yojson p in
        Submit { revision; source } )
  ; ( "preview"
    , fun p ->
        let ({ source; offset; limit } : preview_params) = preview_params_of_yojson p in
        Preview { source; offset; limit } )
  ; ( "code"
    , fun p ->
        let { source; target } : code_params = code_params_of_yojson p in
        Code { source; target } )
  ; ( "target"
    , fun p ->
        let { target } : target_params = target_params_of_yojson p in
        Retarget { target } )
  ; ( "save"
    , fun p ->
        let ({ path } : save_params) = save_params_of_yojson p in
        Save { path } )
  ; ( "undo"
    , fun p ->
        let { revision } = revision_params_of_yojson p in
        Undo { revision } )
  ; ( "redo"
    , fun p ->
        let { revision } = revision_params_of_yojson p in
        Redo { revision } )
  ; "lock", none Lock
  ]
;;

let decode name params =
  match List.assoc_opt name methods with
  | None -> raise (Rejected (Protocol, "Unknown method " ^ name))
  | Some parse ->
    (try parse params with
     | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
       raise (Rejected (Protocol, "Invalid params: " ^ Printexc.to_string exn)))
;;

let category_name = function
  | Protocol -> "protocol"
  | Diagnostic -> "diagnostic"
  | Execution -> "execution"
  | Cancelled -> "cancelled"
  | Superseded -> "superseded"
;;

let point (p : Lexing.position) =
  `Assoc [ "line", `Int p.pos_lnum; "column", `Int (1 + p.pos_cnum - p.pos_bol) ]
;;

let diagnostic (d : Diagnostic.t) =
  let category : category =
    match d.code with
    | Host_failure | Missing_under_error | Arithmetic_overflow | Contract_failure ->
      Execution
    | _ -> Diagnostic
  in
  ( category
  , `Assoc
      ([ "category", `String (category_name category)
       ; "code", `String (Diagnostic.name d.code)
       ; "message", `String d.message
       ]
       @
       match d.span with
       | Some (start, finish) ->
         [ "span", `Assoc [ "start", point start; "end", point finish ] ]
       | None -> []) )
;;

let typ t = `String (Types.display t)

let page (p : Page.t) =
  `Assoc
    [ "total", `Int p.total
    ; "offset", `Int p.offset
    ; "ordered", `Bool p.ordered
    ; ( "columns"
      , `List
          (List.map
             (fun (c : Page.column) ->
                `Assoc
                  [ "name", `String c.name
                  ; "type", typ c.typ
                  ; ( "cells"
                    , `List
                        (Array.to_list
                           (Array.map
                              (function
                                | None -> `Null
                                | Some s -> `String s)
                              c.cells)) )
                  ])
             p.columns) )
    ]
;;

let event : Session.event -> Yojson.Safe.t = function
  | Bound (name, description) ->
    `Assoc [ "kind", `String "bound"; "name", `String name; "type", `String description ]
  | Output text -> `Assoc [ "kind", `String "output"; "text", `String text ]
  | Value (text, t) ->
    `Assoc [ "kind", `String "value"; "value", `String text; "type", typ t ]
  | Table p -> `Assoc [ "kind", `String "table"; "page", page p ]
  | Viewed (subject, r) ->
    `Assoc
      [ "kind", `String "view"
      ; "source", `String (Format.expression subject)
      ; "rows", `Int (Session.total r)
      ]
  | Schema (rows, columns) ->
    `Assoc
      [ "kind", `String "schema"
      ; "rows", `Int rows
      ; ( "columns"
        , `List
            (List.map (fun (n, t) -> `Assoc [ "name", `String n; "type", typ t ]) columns)
        )
      ]
  | Typed t -> `Assoc [ "kind", `String "type"; "type", typ t ]
  | Inspection_failed d ->
    `Assoc [ "kind", `String "inspection-failed"; "diagnostic", snd (diagnostic d) ]
  | Bars (column, f) ->
    `Assoc
      [ "kind", `String "bars"
      ; "column", `String column
      ; "missing", `Int f.missing
      ; "distinct", `Int f.distinct
      ; "top", `List (List.map (fun (v, n) -> `List [ `String v; `Int n ]) f.top)
      ]
  | Histogram (column, { histogram = h; _ }) ->
    `Assoc
      [ "kind", `String "histogram"
      ; "column", `String column
      ; "low", `Float h.low
      ; "high", `Float h.high
      ; "missing", `Int h.missing
      ; "counts", `List (Array.to_list (Array.map (fun c -> `Int c) h.counts))
      ]
;;

let item : TransformIR.item -> Yojson.Safe.t option = function
  | Define (id, e) -> Some (`Assoc [ "name", `String id.name; "type", typ e.typ ])
  | Show e | Write (e, _) -> Some (`Assoc [ "type", typ e.typ ])
  | Variant _ -> None
;;
