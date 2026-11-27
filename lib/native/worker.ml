open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request =
  { revision : int
  ; script : string
  }
[@@deriving yojson_of]

type reply =
  { revision : int
  ; output : string option [@yojson.option]
  ; error : string option [@yojson.option]
  ; code : string option [@yojson.option]
  }
[@@deriving of_yojson]

type t =
  { language : Target.language
  ; pid : int
  ; requests : out_channel
  ; replies : in_channel
  ; mutable revision : int
  ; mutable pending : int list
  ; lifecycle : Mutex.t
  ; mutable stopped : bool
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
        if getattr(exc, "code", None):
            reply["code"] = exc.code
    try:
        print(json.dumps(dict(reply, revision=request["revision"])), flush=True)
    except BrokenPipeError:
        break
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
  }, error = function(e) c(list(error = conditionMessage(e)), if (is.character(e$code)) list(code = e$code)))
  reply$revision <- request$revision
  delivered <- tryCatch({
    cat(jsonlite::toJSON(reply, auto_unbox = TRUE), "\n", sep = "")
    flush(stdout())
    TRUE
  }, error = function(e) FALSE)
  if (!delivered) break
}
|}
;;

let start (runtime : Runtime.t) (language : Target.language) =
  let argv =
    match language with
    | Python -> Array.append runtime.command [| "-u"; "-c"; python |]
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
  { language
  ; pid
  ; requests = Unix.out_channel_of_descr requests
  ; replies = Unix.in_channel_of_descr replies
  ; revision = 0
  ; pending = []
  ; lifecycle = Mutex.create ()
  ; stopped = false
  }
;;

let stop w =
  Mutex.lock w.lifecycle;
  let stop = not w.stopped in
  w.stopped <- true;
  Mutex.unlock w.lifecycle;
  if stop
  then (
    close_out_noerr w.requests;
    close_in_noerr w.replies;
    ignore (Unix.waitpid [] w.pid))
;;

let cancel w =
  Mutex.lock w.lifecycle;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock w.lifecycle)
    (fun () ->
       if not w.stopped
       then (
         try Unix.kill w.pid (if Sys.win32 then Sys.sigterm else Sys.sigkill) with
         | Unix.Unix_error (Unix.ESRCH, _, _) -> ()))
;;

let host_error message =
  let contains needle = Astring.String.is_infix ~affix:needle message in
  let lines = String.split_on_char '\n' message |> List.map String.trim in
  if contains "to `i64` failed"
  then "arithmetic overflow: Int out of range"
  else (
    match List.rev (List.filter (String.starts_with ~prefix:"! ") lines) with
    | last :: _ -> String.sub last 2 (String.length last - 2)
    | [] ->
      (match List.filter (fun l -> l <> "") lines with
       | first :: _ -> first
       | [] -> message))
;;

let language w = w.language

let request w script =
  w.revision <- w.revision + 1;
  try
    Yojson.Safe.to_channel w.requests (yojson_of_request { revision = w.revision; script });
    output_char w.requests '\n';
    flush w.requests;
    w.revision
  with
  | exn ->
    stop w;
    raise exn
;;

let receive w revision =
  let reply =
    try reply_of_yojson (Yojson.Safe.from_string (input_line w.replies)) with
    | exn ->
      stop w;
      raise exn
  in
  if reply.revision <> revision then invalid_arg "Stale backend response";
  reply
;;

let send w script = w.pending <- w.pending @ [ request w script ]

let evaluate w script =
  let pending = w.pending in
  w.pending <- [];
  List.iter
    (fun revision ->
       match (receive w revision).error with
       | Some message -> Diagnostic.fail Host_failure (host_error message)
       | None -> ())
    pending;
  let reply = receive w (request w script) in
  match reply.error, reply.output with
  | Some message, _ ->
    Diagnostic.fail
      (Option.value (Option.bind reply.code Diagnostic.of_name) ~default:Diagnostic.Host_failure)
      (host_error message)
  | None, output -> Option.value output ~default:""
;;
