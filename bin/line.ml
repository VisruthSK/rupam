open Lwt.Syntax

type t =
  { history : LTerm_history.t
  ; file : string option
  }

let interactive = lazy (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)

let state_directory () =
  match Sys.getenv_opt "XDG_STATE_HOME", Sys.getenv_opt "LOCALAPPDATA" with
  | Some dir, _ -> dir
  | None, Some dir when Sys.win32 -> dir
  | _ -> Filename.concat (Filename.concat (Sys.getenv "HOME") ".local") "state"
;;

let rec ensure_directory path =
  if not (Sys.file_exists path)
  then (
    ensure_directory (Filename.dirname path);
    Sys.mkdir path 0o755)
;;

let ephemeral = { history = LTerm_history.create []; file = None }

let persistent () =
  if not (Lazy.force interactive)
  then ephemeral
  else (
    let file = Filename.concat (Filename.concat (state_directory ()) "rupam") "history" in
    let history = LTerm_history.create ~max_entries:1000 [] in
    ensure_directory (Filename.dirname file);
    Lwt_main.run (LTerm_history.load history file);
    { history; file = Some file })
;;

class reader term history prompt complete =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term

    method! completion =
      let before = Zed_string.to_utf8 (Zed_rope.to_string self#input_prev) in
      let start, words = complete before in
      self#set_completion
        (Zed_utf8.length (String.sub before 0 start))
        (List.map (fun w -> Zed_string.of_utf8 w, Zed_string.of_utf8 "") words)

    initializer
      self#set_prompt
        (React.S.const (LTerm_text.eval [ B_fg LTerm_style.cyan; S prompt; E_fg ]))
  end

let read t ?(complete = fun _ -> 0, []) prompt =
  if not (Lazy.force interactive)
  then In_channel.input_line stdin
  else
    Lwt_main.run
      (let* term = Lazy.force LTerm.stdout in
       Lwt.catch
         (fun () ->
            let+ line =
              (new reader term (LTerm_history.contents t.history) prompt complete)#run
            in
            Some (Zed_string.to_utf8 line))
         (function
           | Sys.Break -> Lwt.return (Some "")
           | LTerm_read_line.Interrupt -> Lwt.return None
           | exn -> Lwt.fail exn))
;;

let remember t line =
  LTerm_history.add t.history (Zed_string.of_utf8 line);
  Option.iter (fun file -> Lwt_main.run (LTerm_history.save t.history file)) t.file
;;

let clear () =
  if Lazy.force interactive
  then
    Lwt_main.run
      (let* term = Lazy.force LTerm.stdout in
       LTerm.clear_screen term)
;;
