type location = { generated_line:int; generated_column:int; source:Syntax.span }
type t = {mutable next:int; spans:(int,Syntax.span) Hashtbl.t}

let create () = {next=0;spans=Hashtbl.create 32}
let plain _ code = code

let mark t span code =
  let i=t.next in
  t.next<-i+1; Hashtbl.add t.spans i span;
  Printf.sprintf "\030%d\031%s" i code

let finish t marked =
  let output=Buffer.create (String.length marked) in
  let rec scan i line column entries =
    if i=String.length marked then Buffer.contents output,List.rev entries else
    match marked.[i] with
    | '\030' ->
        let stop=String.index_from marked (i+1) '\031' in
        let id=int_of_string (String.sub marked (i+1) (stop-i-1)) in
        let source=Hashtbl.find t.spans id in
        scan (stop+1) line column ({generated_line=line;generated_column=column;source}::entries)
    | ch ->
        Buffer.add_char output ch;
        if ch='\n' then scan (i+1) (line+1) 1 entries else scan (i+1) line (column+1) entries in
  scan 0 1 1 []
