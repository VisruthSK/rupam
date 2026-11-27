let package = Build_id.package
let revision = if Build_id.revision = "" then None else Some Build_id.revision

let version =
  match revision with
  | None -> package
  | Some r -> package ^ "+" ^ String.map (fun c -> if c = '-' then '.' else c) r
;;

let specification = 1
let protocol = 1
let platform = Printf.sprintf "%s %d-bit" Sys.os_type Sys.word_size

let describe () =
  Printf.sprintf
    "rupam %s\nspecification %d, protocol %d\n%s, OCaml %s"
    version
    specification
    protocol
    platform
    Sys.ocaml_version
;;
