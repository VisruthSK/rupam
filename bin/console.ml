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
  | Select_cell of int * int
  | Act of Viewer.action
  | Return
  | Close
  | Focus_prompt
  | Focus_grid
  | Newline
  | Insert_pipe
  | Ignore
  | Name_edit of Editor.t
  | Name_confirm
  | Name_cancel
  | Draft_tick
  | Draft_ready of int * (Rupam.Session.result, Rupam.Diagnostic.t) result
  | Column_ready of int * string * (Rupam.Arrow_result.profile, Rupam.Diagnostic.t) result

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
  ; editing : bool
  ; naming : Editor.t option
  ; placement : placement
  ; working : work option
  ; closing : bool
  ; save_requested : int option
  ; size : int * int
  }

and placement =
  | Below
  | Beside

and work =
  | Preview_work
  | Profile_work

and menu =
  { before : string
  ; items : Repl.suggestion list
  ; selected : int option
  }

let accent = Color.of_rgb 235 200 142
let subtle = Color.grayscale ~level:17
let faint = Color.grayscale ~level:8
let failure = Color.of_rgb 240 113 120
let missing = Color.of_rgb 218 137 145
let typed = Color.of_rgb 232 225 215
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
        [ (let value = String.sub line 0 (i - 1) in
           if value = "None" then text ~style:(style missing) value else text value)
        ; text ~style:(style subtle) " : "
        ; text ~style:(style typed) (String.sub line (i + 2) (String.length line - i - 2))
        ]
    | _ -> if line = "None" then text ~style:(style missing) line else text line)
;;

let page_view ~digits page =
  let open Rupam.Page in
  let styled (part, text) =
    match part with
    | Plain -> Mosaic.text text
    | Heading -> Mosaic.text ~style:(style ~bold:true typed) text
    | Types -> Mosaic.text ~style:(style subtle) text
    | Index -> Mosaic.text ~style:(style faint) text
    | Missing -> Mosaic.text ~style:(style missing) text
    | Note -> Mosaic.text ~style:(style subtle) text
  in
  List.map
    (fun line -> box ~flex_direction:Row (List.map styled line))
    (lines ~digits page)
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

type grid_row =
  | Column_types
  | Observation of int

let grid_table ~digits (v : Viewer.t) (page : Rupam.Page.t) =
  let index_width = Viewer.index_width v in
  let cell_width = Viewer.cell_width v in
  let frozen = Viewer.frozen v in
  let with_rule rule cells =
    List.concat
      (List.mapi
         (fun i cell -> if frozen = Some i then [ rule; cell ] else [ cell ])
         cells)
  in
  let columns =
    Table.column ~width:(`Fixed index_width) ~alignment:`Right "#"
    :: with_rule
         (Table.column ~width:(`Fixed 1) "│")
         (List.map
            (fun (c : Rupam.Page.column) ->
               Table.column
                 ~width:(`Fixed cell_width)
                 ~alignment:
                   (if Rupam.Types.numeric (Rupam.Types.present c.typ)
                    then `Right
                    else `Left)
                 c.name)
            page.columns)
  in
  let rule = Table.cell ~style:(style faint) "│" in
  let row id index cells = Table.row id (Array.of_list (index :: with_rule rule cells)) in
  let types =
    row
      Column_types
      (Table.cell "")
      (List.map
         (fun (c : Rupam.Page.column) ->
            Table.cell ~style:(style subtle) ("<" ^ Rupam.Types.display c.typ ^ ">"))
         page.columns)
  in
  let count = min (Viewer.rows v) (page.total - page.offset) in
  let focus = Viewer.current v in
  let rows =
    types
    :: List.init count (fun i ->
      let r = i + page.offset in
      row
        (Observation r)
        (Table.cell ~style:(style faint) (string_of_int (r + 1)))
        (List.map
           (fun (c : Rupam.Page.column) ->
              let value = Rupam.Page.format_cell ~digits c.typ c.cells.(i) in
              let style =
                if v.row = r && focus = Some c.name
                then
                  Some
                    (Style.make
                       ~bold:true
                       ~fg:(if value = None then missing else accent)
                       ~bg:(Color.of_rgb 79 62 48)
                       ())
                else if value = None
                then Some (style missing)
                else None
              in
              Table.cell ?style (Option.value value ~default:"None"))
           page.columns))
  in
  let on_mouse event =
    match Event.Mouse.kind event with
    | Event.Mouse.Scroll { direction; delta } ->
      Event.Mouse.prevent_default event;
      Some
        (Scroll
           (match direction with
            | Scroll_up -> Pan_rows (-3 * delta)
            | Scroll_down -> Pan_rows (3 * delta)
            | Scroll_left -> Pan_columns (-delta)
            | Scroll_right -> Pan_columns delta))
    | Up { button = Left; dragged = false } ->
      let x, y = Event.Mouse.local event in
      let x = x - index_width - 2 in
      let boundary = Option.map (fun count -> count * (cell_width + 1)) frozen in
      let x =
        match boundary with
        | Some boundary when x >= boundary -> if x < boundary + 2 then -1 else x - 2
        | _ -> x
      in
      let row = y - 4 in
      if x < 0 || row < 0 || row >= count
      then None
      else (
        Event.Mouse.prevent_default event;
        Some (Select_cell (page.offset + row, x / (cell_width + 1))))
    | _ -> None
  in
  table
    ~key:"viewer-grid"
    ~size:{ width = pct 100; height = px (count + 5) }
    ~border:true
    ~border_style:Border.rounded
    ~cell_padding:0
    ~header_color:typed
    ~show_column_separator:false
    ~show_row_separator:false
    ~show_scroll_indicator:false
    ~selection_visible:false
    ~wheel_navigation:false
    ~columns
    ~rows
    ~equal:( = )
    ~selected:None
    ~on_change:(fun _ -> Ignore)
    ~on_mouse
    ()
;;

let controls actions =
  box
    ~flex_direction:Row
    (List.map
       (fun (label, action) ->
          button
            ~border:false
            ~margin:(margin_lrtb 0 2 0 0)
            ~text_style:(style typed)
            ~on_activate:action
            label)
       actions)
;;

let prompt_editor ?key ?on_focus_request ~focused ~label ~state ~on_change () =
  box
    ~flex_direction:Row
    ~size:full
    [ text ~style:(style ~bold:true accent) label
    ; textarea
        ?key
        ?on_focus_request
        ~size:full
        ~focused
        ~show_cursor:focused
        ~text_color:typed
        ~focused_text_color:(Color.of_rgb 244 231 213)
        ~state
        ~on_change
        ()
    ]
;;

let input_pane model ~focused =
  let lines = List.length (String.split_on_char '\n' (Editor.text model.input)) in
  box
    ~flex_direction:Row
    ~size:{ width = pct 100; height = px (max 1 (min 8 lines)) }
    [ prompt_editor
        ~label:(if model.pending = [] then prompt else continuation)
        ~focused
        ~on_focus_request:Focus_prompt
        ~state:model.input
        ~on_change:(fun e -> Edit e)
        ()
    ]
;;

let suggestions model =
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
                  (if chosen then "\xe2\x80\xba" else " ")
                  width
                  s.text)
           ; text ~style:(style faint) ("  " ^ s.detail)
           ])
      shown
    @
    let rest = List.length menu.items - first - List.length shown in
    if rest > 0
    then [ text ~style:(style faint) (Printf.sprintf "    \xe2\x80\xa6 %d more" rest) ]
    else []
;;

let grid model (v : Viewer.t) =
  let page = Viewer.visible v in
  let digits = Repl.digits model.session in
  let ordering = Viewer.ordering model.session.session v in
  let width, height = model.size in
  let body =
    box
      ~flex_direction:Column
      ~size:{ width = pct 100; height = px (max 1 (height - Viewer.panel_height)) }
      [ text ~style:(style subtle) (Rupam.Page.summary ~ordering page)
      ; grid_table ~digits v page
      ]
  in
  let notice =
    if model.closing
    then text ~style:(style subtle) "Closing preview\xe2\x80\xa6"
    else (
      match v.policy, v.diagnostic with
      | Some _, _ ->
        controls
          [ "1 Skip", Act (Missing_policy Skip)
          ; "2 Keep", Act (Missing_policy Keep)
          ; "3 Error", Act (Missing_policy Fail)
          ]
      | None, Some diagnostic ->
        box
          ~flex_direction:Column
          ~size:{ width = pct 100; height = auto }
          (List.map
             (text ~wrap:`Word ~style:(style failure))
             (String.split_on_char '\n' (Rupam.Diagnostic.render diagnostic)))
      | None, None -> text "")
  in
  let hint =
    if model.editing
    then []
    else
      [ text
          ~style:(style faint)
          "s/S sort \xc2\xb7 = filter to value \xc2\xb7 d describe column \xc2\xb7 u \
           undo \xc2\xb7 i edit \xc2\xb7 q close"
      ]
  in
  let details = List.map (text ~style:(style typed)) (Viewer.bars ~digits v) in
  let naming =
    match model.naming with
    | None -> []
    | Some state ->
      [ box
          ~flex_direction:Row
          ~size:{ width = pct 100; height = px 1 }
          [ prompt_editor
              ~label:"name \xe2\x9d\xaf "
              ~focused:true
              ~state
              ~on_change:(fun e -> Name_edit e)
              ()
          ]
      ]
  in
  let editor =
    (input_pane model ~focused:(model.editing && model.naming = None) :: naming)
    @ (if model.naming = None then suggestions model else [])
    @ [ notice ]
  in
  let table =
    box
      ~flex_direction:Column
      ~size:{ width = pct 100; height = auto }
      ((body :: details) @ hint)
  in
  match model.placement with
  | Below ->
    box
      ~flex_direction:Column
      ~size:{ width = pct 100; height = px height }
      (table :: editor)
  | Beside ->
    box
      ~flex_direction:Row
      ~size:{ width = px width; height = px height }
      [ box ~flex_direction:Column ~size:{ width = pct 40; height = pct 100 } editor
      ; box ~flex_direction:Column ~size:{ width = pct 60; height = pct 100 } [ table ]
      ]
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
    let editor = input_pane model ~focused:true in
    box
      ~flex_direction:Column
      ~size:full
      (pending @ [ editor ] @ suggestions model @ [ status model.session ])
;;

let banner session =
  let loaded =
    match Rupam.Session.path session.Repl.session with
    | Some path -> [ text ~style:(style faint) ("loaded " ^ path) ]
    | None -> []
  in
  Cmd.static_commit
    (box
       ~flex_direction:Column
       ~size:full
       ~margin:(margin_lrtb 0 0 0 1)
       (text ~style:(style ~bold:true accent) ("rupam v" ^ Rupam.Version.version)
        :: loaded))
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
    else (
      let viewer =
        Option.map
          (fun (subject, result) ->
             let width, height = model.size in
             Viewer.open_ model.session.session subject result ~width ~height)
          !(model.opened)
      in
      ( { model with
          pending = []
        ; history
        ; recalled = None
        ; menu = None
        ; viewer
        ; input =
            Option.fold
              ~none:Editor.empty
              ~some:(fun v -> Editor.of_string (Viewer.source v))
              viewer
        ; editing = false
        }
      , committed )))
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

let completion_prefix input =
  Editor.selected_text (Editor.select ~anchor:0 ~cursor:(Editor.cursor input) input)
;;

let insert_completion input before text =
  let anchor = Editor.length (Editor.of_string before) in
  Editor.insert text (Editor.select ~anchor ~cursor:(Editor.cursor input) input)
;;

let complete_editor session input menu =
  match menu with
  | Some ({ items; _ } as menu) when items <> [] ->
    let next =
      match menu.selected with
      | None -> 0
      | Some i -> (i + 1) mod List.length items
    in
    let item = List.nth items next in
    insert_completion input menu.before item.text, Some { menu with selected = Some next }
  | _ ->
    let line = completion_prefix input in
    let start, candidates = Repl.complete session line in
    let before = String.sub line 0 start in
    (match candidates with
     | [] -> input, None
     | [ only ] ->
       ( insert_completion input before only.text
       , Some { before; items = [ only ]; selected = Some 0 } )
     | many ->
       let prefix = common_prefix many in
       let typed = String.sub line start (String.length line - start) in
       let input =
         if String.length prefix > String.length typed
         then insert_completion input before prefix
         else input
       in
       input, Some { before; items = many; selected = None })
;;

let filter_menu session input menu =
  let line = completion_prefix input in
  if line = ""
  then None
  else
    Option.map
      (fun _ ->
         let start, items = Repl.complete session line in
         { before = String.sub line 0 start; items; selected = None })
      menu
;;

let close_viewer model =
  Option.iter Viewer.close model.viewer;
  { model with
    viewer = None
  ; editing = false
  ; naming = None
  ; closing = false
  ; save_requested = None
  ; input = Editor.empty
  ; menu = None
  }
;;

let submit_viewer model v name =
  match Viewer.submission v name with
  | source -> evaluate (close_viewer { model with pending = [] }) source
  | exception Rupam.Diagnostic.Error diagnostic ->
    ( { model with
        viewer = Some { v with diagnostic = Some diagnostic }
      ; save_requested = None
      ; naming = None
      }
    , Cmd.none )
;;

let save_viewer model (v : Viewer.t) =
  if Viewer.named v
  then submit_viewer model v ""
  else
    ( { model with
        naming = Some (Editor.of_string v.binding)
      ; save_requested = None
      ; menu = None
      }
    , Cmd.none )
;;

let finish_viewer_work model =
  match model.viewer with
  | Some v when model.save_requested = Some v.generation && v.settled = v.generation ->
    if v.displayed = v.generation
    then save_viewer model v
    else { model with save_requested = None }, Cmd.none
  | _ -> model, Cmd.none
;;

let change_draft model f =
  match model.viewer with
  | None -> model
  | Some v ->
    let (next : Viewer.t) = f v in
    let input =
      if Viewer.source next = Editor.text model.input
      then model.input
      else Editor.of_string (Viewer.source next)
    in
    if next.generation = v.generation
    then { model with viewer = Some next; input }
    else (
      if model.working = Some Preview_work then Rupam.Session.cancel_draft v.context;
      { model with viewer = Some next; input; save_requested = None })
;;

let result_of f =
  match f () with
  | value -> Ok value
  | exception exn -> Error (Rupam.Diagnostic.of_exn exn)
;;

let update msg model =
  match msg with
  | Ignore -> model, Cmd.none
  | Edit input ->
    let model = { model with input; menu = filter_menu model.session input model.menu } in
    change_draft model (fun v -> Viewer.edit v (Editor.text input)), Cmd.none
  | Newline ->
    let input = Editor.insert "\n" model.input in
    ( change_draft { model with input; menu = None } (fun v ->
        Viewer.edit v (Editor.text input))
    , Cmd.none )
  | Insert_pipe ->
    let input = Editor.insert " |> " model.input in
    ( change_draft { model with input; menu = None } (fun v ->
        Viewer.edit v (Editor.text input))
    , Cmd.none )
  | Name_edit state -> { model with naming = Some state }, Cmd.none
  | Name_confirm ->
    (match model.viewer, model.naming with
     | Some v, Some state ->
       let name = String.trim (Editor.text state) in
       if Rupam.Identifier.valid name && name <> ""
       then submit_viewer { model with naming = None } v name
       else model, Cmd.none
     | _ -> model, Cmd.none)
  | Name_cancel -> { model with naming = None }, Cmd.none
  | Submit source -> evaluate model source
  | Recall step -> { (recall model step) with menu = None }, Cmd.none
  | Complete ->
    let input, menu = complete_editor model.session model.input model.menu in
    ( change_draft { model with input; menu } (fun v -> Viewer.edit v (Editor.text input))
    , Cmd.none )
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
  | Select_cell (row, column) ->
    ( { model with
        viewer = Option.map (fun v -> Viewer.move v (Cell (row, column))) model.viewer
      ; editing = false
      }
    , Cmd.none )
  | Act Distribution ->
    (match model.viewer with
     | Some v when model.working = None ->
       (match Viewer.current v with
        | Some name ->
          (match List.assoc_opt name v.profiles with
           | Some profile ->
             { model with viewer = Some (Viewer.show_profile v name profile) }, Cmd.none
           | None ->
             ( { model with working = Some Profile_work }
             , Cmd.perform (fun dispatch ->
                 let profile =
                   result_of (fun () -> Rupam.Session.column_summary v.result name)
                 in
                 dispatch (Column_ready (v.displayed, name, profile))) ))
        | None -> model, Cmd.none)
     | _ -> model, Cmd.none)
  | Act action -> change_draft model (fun v -> Viewer.act v action), Cmd.none
  | Return ->
    (match model.viewer with
     | Some v when model.working = None && v.generation = v.displayed ->
       save_viewer model v
     | Some v when model.working <> None || v.generation <> v.settled ->
       { model with save_requested = Some v.generation }, Cmd.none
     | _ -> model, Cmd.none)
  | Close ->
    if model.working <> None
    then (
      Option.iter
        (fun (v : Viewer.t) -> Rupam.Session.cancel_draft v.context)
        model.viewer;
      { model with closing = true }, Cmd.none)
    else close_viewer model, Cmd.none
  | Focus_prompt -> { model with editing = true }, Cmd.none
  | Focus_grid -> { model with editing = false; menu = None }, Cmd.none
  | Draft_tick ->
    (match model.viewer with
     | Some v
       when model.working = None
            && (not model.closing)
            && v.generation <> v.settled
            && Unix.gettimeofday () -. v.edited_at >= 0.15 ->
       ( { model with working = Some Preview_work }
       , Cmd.perform (fun dispatch ->
           let result =
             result_of (fun () -> Rupam.Session.preview_draft v.context (Viewer.source v))
           in
           dispatch (Draft_ready (v.generation, result))) )
     | _ -> model, Cmd.none)
  | Draft_ready (generation, result) ->
    let model = { model with working = None } in
    (match model.viewer with
     | Some v when (not model.closing) && v.generation = generation ->
       finish_viewer_work
         { model with viewer = Some (Viewer.publish v generation result) }
     | _ ->
       Result.iter Rupam.Session.release_draft result;
       if not model.closing
       then
         Option.iter
           (fun (v : Viewer.t) -> Rupam.Session.reset_cancelled_draft v.context)
           model.viewer;
       (if model.closing then close_viewer model else model), Cmd.none)
  | Column_ready (generation, name, result) ->
    let model = { model with working = None } in
    if model.closing
    then close_viewer model, Cmd.none
    else (
      let viewer =
        Option.map
          (fun (v : Viewer.t) ->
             if generation <> v.displayed || Viewer.current v <> Some name
             then v
             else (
               match result with
               | Ok profile -> Viewer.show_profile v name profile
               | Error diagnostic -> { v with diagnostic = Some diagnostic }))
          model.viewer
      in
      finish_viewer_work { model with viewer })
;;

let editing_keys =
  Sub.on_key ~phase:Capture (fun event ->
    let key = Event.Key.data event in
    let only_shift =
      not (key.mods.alt || key.mods.super || key.mods.hyper || key.mods.meta)
    in
    match key.key with
    | Enter when key.mods.shift && (not key.mods.ctrl) && only_shift -> Some Newline
    | Char c
      when (Uchar.to_int c = Char.code 'm' || Uchar.to_int c = Char.code 'M')
           && key.mods.ctrl
           && key.mods.shift
           && only_shift -> Some Insert_pipe
    | _ -> None)
;;

let subscriptions model =
  let resize = Sub.on_resize (fun ~width ~height -> Resize (width, height)) in
  match model.viewer with
  | Some v ->
    let page = Viewer.rows v in
    Sub.batch
      [ resize
      ; (if model.editing && model.naming = None then editing_keys else Sub.none)
      ; (if model.working <> None || v.generation = v.settled
         then Sub.none
         else Sub.every ~id:"viewer-preview" 0.05 (fun _ -> Draft_tick))
      ; (if model.closing
         then Sub.none
         else
           Sub.on_keys
             ~phase:Capture
             (if model.naming <> None
              then [ Shortcut.enter, Name_confirm; Shortcut.escape, Name_cancel ]
              else if model.editing
              then
                [ Shortcut.enter, Return
                ; Shortcut.tab, Complete
                ; Shortcut.escape, Focus_grid
                ]
              else
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
                ; Shortcut.char 's', Act (Sort Ascending)
                ; Shortcut.char 'S', Act (Sort Descending)
                ; Shortcut.char '=', Act Keep_value
                ; Shortcut.char 'u', Act Undo
                ; Shortcut.char 'd', Act Distribution
                ; Shortcut.char 'i', Focus_prompt
                ; Shortcut.tab, Focus_prompt
                ; Shortcut.enter, Return
                ; Shortcut.char 'q', Close
                ; Shortcut.escape, Close
                ]
                @
                match v.policy with
                | None -> []
                | Some _ ->
                  [ Shortcut.char '1', Act (Missing_policy Skip)
                  ; Shortcut.char '2', Act (Missing_policy Keep)
                  ; Shortcut.char '3', Act (Missing_policy Fail)
                  ]))
      ]
  | None ->
    Sub.batch
      [ resize
      ; editing_keys
      ; Sub.on_keys
          ~phase:Capture
          ([ Shortcut.up, Recall (-1)
           ; Shortcut.down, Recall 1
           ; Shortcut.tab, Complete
           ; Shortcut.enter, Submit (Editor.text model.input)
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
    let config =
      Matrix.config ~mode:`Primary ~mouse:`Sgr_normal ~kitty_keyboard:`Auto ()
    in
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
              ; editing = false
              ; naming = None
              ; placement = Below
              ; working = None
              ; closing = false
              ; save_requested = None
              ; size = terminal ()
              }
            , banner session ))
      ; update
      ; view
      ; subscriptions
      }
;;

type pick =
  | Move of int
  | Jump of int
  | Choose
  | Cancel

let pick ~title options =
  let chosen = ref None in
  let count = List.length options in
  let view selected =
    let rows =
      List.mapi
        (fun i option ->
           let option = Printf.sprintf "%d. %s" (i + 1) option in
           if i = selected
           then text ~style:(style ~bold:true accent) ("  \xe2\x9d\xaf " ^ option)
           else text ~style:(style subtle) ("    " ^ option))
        options
    in
    let help =
      text
        ~style:(style faint)
        "  \xe2\x86\x91/\xe2\x86\x93 or number \xc2\xb7 Enter choose \xc2\xb7 Esc cancel"
    in
    box
      ~flex_direction:Column
      ~size:full
      ((text ~style:(style ~bold:true accent) title :: rows) @ [ help ])
  in
  let update msg selected =
    match msg with
    | Move step -> (selected + step + count) mod count, Cmd.none
    | Jump i -> i, Cmd.none
    | Choose ->
      chosen := Some selected;
      selected, Cmd.quit
    | Cancel -> selected, Cmd.quit
  in
  let subscriptions _ =
    Sub.on_keys
      ([ Shortcut.up, Move (-1)
       ; Shortcut.down, Move 1
       ; Shortcut.char 'k', Move (-1)
       ; Shortcut.char 'j', Move 1
       ; Shortcut.enter, Choose
       ; Shortcut.escape, Cancel
       ; Shortcut.ctrl 'c', Cancel
       ]
       @ List.init (min count 9) (fun i ->
         Shortcut.char (Char.chr (Char.code '1' + i)), Jump i))
  in
  run
    ~config:(Matrix.config ~mode:`Primary ~mouse:`Off ())
    { init = (fun () -> 0, Cmd.none); update; view; subscriptions };
  !chosen
;;
