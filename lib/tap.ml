type t =
  { kind : Syntax.tap
  ; subject : Syntax.expr
  ; arguments : (string option * Syntax.expr) list
  }

let rec strip (e : Syntax.expr) =
  match e.form with
  | Tap (kind, (None, subject) :: arguments) ->
    let subject, taps = strip subject in
    subject, taps @ [ { kind; subject; arguments } ]
  | Pipe (v, f) ->
    let v, taps = strip v in
    { e with form = Pipe (v, f) }, taps
  | Call (f, (None, a) :: rest) ->
    let a, taps = strip a in
    { e with form = Call (f, (None, a) :: rest) }, taps
  | _ -> e, []
;;

let split (i : Syntax.item) =
  match i.declaration with
  | Evaluate ({ form = Tap _; _ } as e) -> None, snd (strip e)
  | Evaluate e ->
    let e, taps = strip e in
    Some { i with declaration = Evaluate e }, taps
  | Let (name, None, e) ->
    let e, taps = strip e in
    Some { i with declaration = Let (name, None, e) }, taps
  | _ -> Some i, []
;;
