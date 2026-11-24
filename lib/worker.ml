type t =
  { pid : int
  ; requests : out_channel
  ; replies : in_channel
  ; mutable revision : int
  }

let python =
  {|
import contextlib
import io
import json
import sys

session = {}
for line in sys.stdin:
    request = json.loads(line)
    output = io.StringIO()
    try:
        with contextlib.redirect_stdout(output):
            exec(request["script"], session)
        reply = {"output": output.getvalue()}
    except Exception as exc:
        reply = {"error": str(exc)}
    print(json.dumps(dict(reply, revision=request["revision"])), flush=True)
|}
;;

let r =
  {|
session <- new.env(parent = globalenv())
input <- file("stdin", "r")
repeat {
  line <- readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  request <- jsonlite::fromJSON(line)
  reply <- tryCatch({
    output <- capture.output(eval(parse(text = request$script), envir = session))
    list(output = paste(c(output, ""), collapse = "\n"))
  }, error = function(e) list(error = conditionMessage(e)))
  reply$revision <- request$revision
  cat(jsonlite::toJSON(reply, auto_unbox = TRUE), "\n", sep = "")
  flush(stdout())
}
|}
;;

let start (runtime : Runtime.t) language =
  let argv =
    match language with
    | Runtime.Python -> Array.append runtime.command [| "-u"; "-c"; python |]
    | R -> Array.append runtime.command [| "-e"; runtime.prelude ^ r |]
  in
  let input, requests = Unix.pipe ~cloexec:true () in
  let replies, output = Unix.pipe ~cloexec:true () in
  let pid =
    try Unix.create_process argv.(0) argv input output Unix.stderr with
    | exn ->
      List.iter Unix.close [ input; requests; replies; output ];
      raise exn
  in
  Unix.close input;
  Unix.close output;
  { pid
  ; requests = Unix.out_channel_of_descr requests
  ; replies = Unix.in_channel_of_descr replies
  ; revision = 0
  }
;;

let stop w =
  close_out_noerr w.requests;
  close_in_noerr w.replies;
  (try Unix.kill w.pid Sys.sigterm with
   | Unix.Unix_error (Unix.ESRCH, _, _) -> ());
  ignore (Unix.waitpid [] w.pid)
;;

let active = Hashtbl.create 2
let () = at_exit (fun () -> Hashtbl.iter (fun _ w -> stop w) active)

let run runtime language script =
  let key = runtime, language in
  let w =
    match Hashtbl.find_opt active key with
    | Some w -> w
    | None ->
      let w = start runtime language in
      Hashtbl.add active key w;
      w
  in
  w.revision <- w.revision + 1;
  let reply =
    try
      Yojson.Basic.to_channel
        w.requests
        (`Assoc [ "revision", `Int w.revision; "script", `String script ]);
      output_char w.requests '\n';
      flush w.requests;
      Yojson.Basic.from_string (input_line w.replies)
    with
    | exn ->
      Hashtbl.remove active key;
      stop w;
      raise exn
  in
  let open Yojson.Basic.Util in
  if to_int (member "revision" reply) <> w.revision then failwith "Stale backend response";
  match member "error" reply |> to_string_option with
  | Some message -> failwith message
  | None ->
    print_string (member "output" reply |> to_string);
    flush stdout
;;
