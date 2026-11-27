type model
type msg

val app
  :  project:Rupam_native.Project.t
  -> ?path:string
  -> unit
  -> (model, msg) Mosaic.app option

val config : Matrix.config
val start : project:Rupam_native.Project.t -> ?path:string -> unit -> unit
val pick : title:string -> string list -> int option
