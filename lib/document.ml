type location =
  { line : int
  ; column : int
  ; source : Syntax.span
  }

type t =
  { buffer : Buffer.t
  ; mutable line : int
  ; mutable column : int
  ; mutable locations : location list
  }

let create () = { buffer = Buffer.create 4096; line = 1; column = 1; locations = [] }

let text d s =
  Buffer.add_string d.buffer s;
  String.iter
    (fun c ->
       if c = '\n'
       then (
         d.line <- d.line + 1;
         d.column <- 1)
       else d.column <- d.column + 1)
    s
;;

let newline d indent =
  text d "\n";
  text d (String.make indent ' ')
;;

let mark d source =
  d.locations <- { line = d.line; column = d.column; source } :: d.locations
;;

let width = 80

let fits d render =
  let scratch = create () in
  render scratch;
  let flat = Buffer.contents scratch.buffer in
  (not (String.contains flat '
')) && d.column - 1 + String.length flat <= width
;;

let separated d separator f xs =
  List.iteri
    (fun i x ->
       if i > 0 then text d separator;
       f x)
    xs
;;

let contents d = Buffer.contents d.buffer, List.rev d.locations
