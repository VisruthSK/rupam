let interactive = lazy (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout)

let state_directory () =
  match Sys.getenv_opt "XDG_STATE_HOME", Sys.getenv_opt "LOCALAPPDATA" with
  | Some dir, _ -> dir
  | None, Some dir when Sys.win32 -> dir
  | _ -> Filename.concat (Filename.concat (Sys.getenv "HOME") ".local") "state"
;;

let history_file () =
  Filename.concat (Filename.concat (state_directory ()) "rupam") "history"
;;

let rec ensure_directory path =
  if not (Sys.file_exists path)
  then (
    ensure_directory (Filename.dirname path);
    Sys.mkdir path 0o755)
;;

let load_history () =
  match In_channel.with_open_text (history_file ()) In_channel.input_lines with
  | lines ->
    List.rev
      (List.filter_map
         (fun line ->
            try Some (Scanf.unescaped line) with
            | Scanf.Scan_failure _ -> None)
         lines)
  | exception Sys_error _ -> []
;;

let save_history entries =
  let file = history_file () in
  ensure_directory (Filename.dirname file);
  Out_channel.with_open_text file (fun ch ->
    List.iter
      (fun entry -> output_string ch (String.escaped entry ^ "\n"))
      (List.rev (List.filteri (fun i _ -> i < 1000) entries)))
;;
