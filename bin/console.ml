open Mosaic
module Style = Ansi.Style
module Color = Ansi.Color

type msg =
  | Edit of Editor.t
  | Submit of string
  | Recall of int
  | Complete
  | Clear
  | Quit
  | Resize of int * int
  | Scroll of Viewer.move
  | Act of Viewer.action
  | Return
  | Close

type model =
  { session : Repl.t
  ; input : Editor.t
  ; pending : string list
  ; history : string list
  ; recalled : int option
  ; menu : menu option
  ; pages : Rupam.Page.t list ref
  ; opened : (Rupam.Syntax.expr * Rupam.Session.result) option ref
  ; viewer : Viewer.t option
  ; size : int * int
  }

and menu =
  { before : string
  ; items : Repl.suggestion list
  ; selected : int option
  }

let accent = Color.of_rgb 120 180 255
let subtle = Color.grayscale ~level:12
let faint = Color.grayscale ~level:8
let success = Color.of_rgb 125 211 160
let failure = Color.of_rgb 240 113 120
let typed = Color.of_rgb 130 200 220
let style ?bold ?dim fg = Style.make ?bold ?dim ~fg ()
let prompt = "rupam \xe2\x9d\xaf "
let continuation = "    \xe2\x80\xa6 "
let full = { width = pct 100; height = auto }

let capture f =
  flush stdout;
  flush stderr;
  let file = Filename.temp_file "rupam" ".out" in
  let target = Unix.openfile file [ O_WRONLY; O_TRUNC ] 0o600 in
  let saved_out = Unix.dup Unix.stdout
  and saved_err = Unix.dup Unix.stderr in
  Unix.dup2 target Unix.stdout;
  Unix.dup2 target Unix.stderr;
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stdout;
        flush stderr;
        Unix.dup2 saved_out Unix.stdout;
        Unix.dup2 saved_err Unix.stderr;
        List.iter Unix.close [ saved_out; saved_err; target ])
      f
  in
  let text = In_channel.with_open_text file In_channel.input_all in
  Sys.remove file;
  result, text
;;

let output_line line =
  let prefix = "error: " in
  if String.starts_with ~prefix line
  then
    box
      ~flex_direction:Row
      [ text ~style:(style ~bold:true failure) "error "
      ; text ~wrap:`Word (String.sub line 7 (String.length line - 7))
      ]
  else (
    match String.index_opt line ':' with
    | Some i
      when i > 0
           && i + 2 <= String.length line
           && line.[i - 1] = ' '
           && line.[i + 1] = ' ' ->
      box
        ~flex_direction:Row
        [ text (String.sub line 0 (i - 1))
        ; text ~style:(style subtle) " : "
        ; text ~style:(style typed) (String.sub line (i + 2) (String.length line - i - 2))
        ]
    | _ -> text line)
;;

let page_view ?limit ~digits page =
  let open Rupam.Page in
  let styled (part, text) =
    match part with
    | Plain -> Mosaic.text text
    | Heading -> Mosaic.text ~style:(style ~bold:true typed) text
    | Types -> Mosaic.text ~style:(style subtle) text
    | Index | Missing -> Mosaic.text ~style:(style faint) text
    | Note -> Mosaic.text ~style:(style subtle) text
  in
  List.map
    (fun line -> box ~flex_direction:Row (List.map styled line))
    (lines ?limit ~digits page)
;;

let transcript ~digits source output pages =
  let lines = String.split_on_char '\n' source in
  let echoed =
    List.mapi
      (fun i line ->
         box
           ~flex_direction:Row
           [ text ~style:(style faint) (if i = 0 then prompt else continuation)
           ; text ~style:(style subtle) line
           ])
      lines
  in
  let results =
    String.split_on_char '\n' output
    |> List.filter (fun l -> String.trim l <> "")
    |> List.map output_line
  in
  box
    ~flex_direction:Column
    ~size:full
    ~margin:(margin_lrtb 0 0 0 1)
    (echoed @ results @ List.concat_map (page_view ~digits) pages)
;;

let status session =
  let dialect =
    match Rupam.Session.target session.Repl.session with
    | Some t -> Rupam.Target.name t
    | None -> "no target"
  in
  text
    ~style:(style faint)
    (Printf.sprintf "  %s \xc2\xb7 :help \xc2\xb7 Ctrl+D quits" dialect)
;;

let terminal () =
  match Tty.window Unix.stdout with
  | Some (width, height) -> width, max 8 (height - 1)
  | None -> 100, 30
;;

let grid model (v : Viewer.t) =
  let body =
    match Viewer.bars v with
    | [] ->
      page_view
        ~limit:(fst model.size)
        ~digits:(Repl.digits model.session)
        (Viewer.visible v)
    | bars -> List.map (fun line -> text ~style:(style typed) line) bars
  in
  box
    ~flex_direction:Column
    ~size:{ width = pct 100; height = px (snd model.size) }
    (text ~style:(style faint) (Viewer.status v) :: body)
;;

let view model =
  match model.viewer with
  | Some v -> grid model v
  | None ->
    let pending =
      List.mapi
        (fun i line ->
           box
             ~flex_direction:Row
             [ text ~style:(style subtle) (if i = 0 then prompt else continuation)
             ; text line
             ])
        (List.rev model.pending)
    in
    let editor =
      box
        ~flex_direction:Row
        ~align_items:Center
        ~size:{ width = pct 100; height = px 1 }
        [ text
            ~style:(style ~bold:true accent)
            (if model.pending = [] then prompt else continuation)
        ; input
            ~size:{ width = pct 100; height = px 1 }
            ~focused:true
            ~state:model.input
            ~on_change:(fun e -> Edit e)
            ~on_submit:(fun s -> Submit s)
            ()
        ]
    in
    let suggestions =
      match model.menu with
      | None -> []
      | Some menu ->
        let width =
          List.fold_left
            (fun w (s : Repl.suggestion) -> max w (String.length s.text))
            0
            menu.items
        in
        let first =
          match menu.selected with
          | Some i when i >= 8 -> i - 7
          | _ -> 0
        in
        let shown = List.filteri (fun i _ -> i >= first && i < first + 8) menu.items in
        List.mapi
          (fun i (s : Repl.suggestion) ->
             let chosen = menu.selected = Some (first + i) in
             box
               ~flex_direction:Row
               [ text
                   ~style:(if chosen then style ~bold:true accent else style typed)
                   (Printf.sprintf
                      "  %s %-*s"
                      (if chosen then "âº" else " ")
                      width
                      s.text)
               ; text ~style:(style faint) ("  " ^ s.detail)
               ])
          shown
        @
        let rest = List.length menu.items - first - List.length shown in
        if rest > 0
        then [ text ~style:(style faint) (Printf.sprintf "    â¦ %d more" rest) ]
        else []
    in
    box
      ~flex_direction:Column
      ~size:full
      (pending @ [ editor ] @ suggestions @ [ status model.session ])
;;

let banner session =
  Cmd.static_commit
    (box
       ~flex_direction:Column
       ~size:full
       ~margin:(margin_lrtb 0 0 0 1)
       [ box
           ~flex_direction:Row
           [ text ~style:(style ~bold:true accent) "rupam"
           ; text ~style:(style subtle) "  typed data analysis for R and Python"
           ]
       ; text
           ~style:(style faint)
           (match Rupam.Session.path session.Repl.session with
            | Some path -> "loaded " ^ path
            | None -> "type an expression, or :help")
       ])
;;

let evaluate model source =
  let entry = String.concat "\n" (List.rev (source :: model.pending)) in
  if String.trim entry = ""
  then { model with input = Editor.empty; pending = [] }, Cmd.none
  else if Repl.incomplete entry
  then { model with input = Editor.empty; pending = source :: model.pending }, Cmd.none
  else (
    let history = entry :: model.history in
    Line.save_history history;
    model.pages := [];
    model.opened := None;
    let next, output =
      capture (fun () ->
        match Repl.parse entry with
        | Error message ->
          Repl.report message;
          true
        | Ok command -> Repl.run model.session command)
    in
    let committed =
      Cmd.static_commit
        (transcript
           ~digits:(Repl.digits model.session)
           entry
           output
           (List.rev !(model.pages)))
    in
    if not next
    then (
      Rupam.Session.close model.session.session;
      model, Cmd.batch [ committed; Cmd.quit ])
    else
      ( { model with
          input = Editor.empty
        ; pending = []
        ; history
        ; recalled = None
        ; menu = None
        ; viewer =
            Option.map
              (fun (subject, result) ->
                 let width, height = model.size in
                 Viewer.open_ model.session.session subject result ~width ~height)
              !(model.opened)
        }
      , committed ))
;;

let recall model step =
  let count = List.length model.history in
  let index =
    match model.recalled, step with
    | None, -1 -> Some 0
    | Some i, -1 -> Some (min (count - 1) (i + 1))
    | Some 0, 1 -> None
    | Some i, 1 -> Some (i - 1)
    | current, _ -> current
  in
  match index with
  | Some i when i < count ->
    let text = List.nth model.history i in
    { model with recalled = Some i; input = Editor.of_string text }
  | _ -> { model with recalled = None; input = Editor.empty }
;;

let common_prefix = function
  | [] -> ""
  | (first : Repl.suggestion) :: rest ->
    List.fold_left
      (fun prefix (s : Repl.suggestion) ->
         let n = min (String.length prefix) (String.length s.text) in
         let rec go i = if i < n && prefix.[i] = s.text.[i] then go (i + 1) else i in
         String.sub prefix 0 (go 0))
      first.text
      rest
;;

let complete model =
  match model.menu with
  | Some ({ items; _ } as menu) when items <> [] ->
    let next =
      match menu.selected with
      | None -> 0
      | Some i -> (i + 1) mod List.length items
    in
    let item = List.nth items next in
    { model with
      input = Editor.of_string (menu.before ^ item.text)
    ; menu = Some { menu with selected = Some next }
    }
  | _ ->
    let line = Editor.text model.input in
    let start, candidates = Repl.complete model.session line in
    let before = String.sub line 0 start in
    (match candidates with
     | [] -> { model with menu = None }
     | [ only ] ->
       { model with input = Editor.of_string (before ^ only.text); menu = None }
     | many ->
       let prefix = common_prefix many in
       let typed = String.sub line start (String.length line - start) in
       let input =
         if String.length prefix > String.length typed
         then Editor.of_string (before ^ prefix)
         else model.input
       in
       { model with input; menu = Some { before; items = many; selected = None } })
;;

let update msg model =
  match msg with
  | Edit input -> { model with input; menu = None }, Cmd.none
  | Submit source -> evaluate model source
  | Recall step -> { (recall model step) with menu = None }, Cmd.none
  | Complete -> complete model, Cmd.none
  | Clear -> { model with input = Editor.empty; pending = [] }, Cmd.static_clear
  | Quit ->
    Rupam.Session.close model.session.session;
    model, Cmd.quit
  | Resize _ ->
    let width, height = terminal () in
    ( { model with
        size = width, height
      ; viewer = Option.map (Viewer.resize ~width ~height) model.viewer
      }
    , Cmd.none )
  | Scroll step ->
    ( { model with viewer = Option.map (fun v -> Viewer.move v step) model.viewer }
    , Cmd.none )
  | Act action ->
    ( { model with viewer = Option.map (fun v -> Viewer.act v action) model.viewer }
    , Cmd.none )
  | Return ->
    ( { model with
        viewer = None
      ; input =
          Option.fold
            ~none:model.input
            ~some:(fun v -> Editor.of_string (Viewer.source v))
            model.viewer
      }
    , Cmd.none )
  | Close -> { model with viewer = None }, Cmd.none
;;

let subscriptions model =
  let resize = Sub.on_resize (fun ~width ~height -> Resize (width, height)) in
  match model.viewer with
  | Some v ->
    let page = Viewer.rows v in
    Sub.batch
      [ resize
      ; Sub.on_keys
          ~phase:Capture
          [ Shortcut.up, Scroll (Rows (-1))
          ; Shortcut.down, Scroll (Rows 1)
          ; Shortcut.char 'k', Scroll (Rows (-1))
          ; Shortcut.char 'j', Scroll (Rows 1)
          ; Shortcut.left, Scroll (Columns (-1))
          ; Shortcut.right, Scroll (Columns 1)
          ; Shortcut.char 'h', Scroll (Columns (-1))
          ; Shortcut.char 'l', Scroll (Columns 1)
          ; Shortcut.key Page_up, Scroll (Rows (-page))
          ; Shortcut.key Page_down, Scroll (Rows page)
          ; Shortcut.space, Scroll (Rows page)
          ; Shortcut.char 'g', Scroll Top
          ; Shortcut.char 'G', Scroll Bottom
          ; Shortcut.char 's', Act (Sort true)
          ; Shortcut.char 'S', Act (Sort false)
          ; Shortcut.char 'f', Act Keep_value
          ; Shortcut.char 'u', Act Undo
          ; Shortcut.char 'd', Act Distribution
          ; Shortcut.enter, Return
          ; Shortcut.char 'q', Close
          ; Shortcut.escape, Close
          ]
      ]
  | None ->
    Sub.batch
      [ resize
      ; Sub.on_keys
          ~phase:Capture
          ([ Shortcut.up, Recall (-1)
           ; Shortcut.down, Recall 1
           ; Shortcut.tab, Complete
           ; Shortcut.ctrl 'l', Clear
           ]
           @ if Editor.text model.input = "" then [ Shortcut.ctrl 'd', Quit ] else [])
      ]
;;

let start ~project ?path () =
  match Repl.open_session ~project ?path () with
  | None -> ()
  | Some session ->
    let pages = ref [] in
    let opened = ref None in
    let session =
      { session with
        show = (fun ~digits:_ page -> pages := page :: !pages)
      ; view = (fun subject result -> opened := Some (subject, result))
      }
    in
    let config = Matrix.config ~mode:`Primary ~mouse:`Off () in
    run
      ~config
      { init =
          (fun () ->
            ( { session
              ; input = Editor.empty
              ; pending = []
              ; history = Line.load_history ()
              ; recalled = None
              ; menu = None
              ; pages
              ; opened
              ; viewer = None
              ; size = terminal ()
              }
            , banner session ))
      ; update
      ; view
      ; subscriptions
      }
;;
