open Shape

let q = Polars.quoted
let lower ~fields plan =
  let names = Array.make plan.Table.next "" in
  List.iteri (fun i n -> names.(i) <- n) fields;
  let next = ref (List.length fields) in
  let bind n = names.(!next) <- n; incr next in
  let rec emit e = match e.node with
    | Input i -> ".data[[" ^ q names.(i) ^ "]]"
    | Broadcast (_, e) -> emit e
    | Constant (`Int i) -> "bit64::as.integer64(" ^ q (Int64.to_string i) ^ ")"
    | Constant (`Float f) -> Printf.sprintf "%.17g" f
    | Constant (`Bool b) -> if b then "TRUE" else "FALSE"
    | Coalesce (a,b) -> "rp_coalesce(" ^ emit a ^ ", " ^ emit b ^ ")"
    | Abs a when a.typ.primitive = Float64 -> "abs(" ^ emit a ^ ")"
    | Abs _ -> raise (Error "R Int64 abs requires checked overflow")
    | Binary (op,a,b) ->
        if a.typ.primitive = Int64 && op <> Greater then raise (Error "R Int64 arithmetic requires checked overflow");
        let op = match op with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Greater -> ">" in
        if op = ">" && a.typ.primitive = Float64 then "rp_gt(" ^ emit a ^ ", " ^ emit b ^ ")"
        else "(" ^ emit a ^ " " ^ op ^ " " ^ emit b ^ ")"
    | Reduce (r,a) ->
        let f = match r with Mean -> "mean" | Sd -> "sd" | Sum -> "sum" in
        "rp_fold(" ^ emit a ^ ", " ^ f ^ ")" in
  let stage = function
    | Table.Take n -> Printf.sprintf "slice_head(n = %d)" n
    | Distinct _ -> "distinct()"
    | Filter x -> "filter(" ^ emit x ^ ")"
    | Select ns -> "select(all_of(c(" ^ String.concat ", " (List.map q ns) ^ ")))"
    | Bind (n,x) -> let v = emit x in bind n; "mutate(" ^ q n ^ " := " ^ v ^ ")"
    | Arrange xs -> "arrange(" ^ String.concat ", " (List.map (fun (x,d) ->
        if d then "desc(" ^ emit x ^ ")" else emit x) xs) ^ ")"
    | Summarize (keys,values) ->
        let key_names = List.map fst keys in
        let by = if keys=[] then [] else [".by = all_of(c(" ^ String.concat ", " (List.map q key_names) ^ "))"] in
        let args = List.map (fun (n,x) -> q n ^ " := " ^
          Option.fold ~none:"bit64::as.integer64(n())" ~some:emit x) values in
        List.iter bind (key_names @ List.map fst values);
        "summarise(" ^ String.concat ", " (args @ by) ^ ")" in
  List.rev plan.steps |> List.map stage |> String.concat " |>\n  "

let preamble = {|library(dplyr)

rp_missing <- function(x) is.na(x) & !is.nan(x)
rp_coalesce <- function(x, y) {
  n <- max(length(x), length(y))
  x <- rep_len(x, n)
  missing <- rp_missing(x)
  x[missing] <- rep_len(y, n)[missing]
  x
}
rp_gt <- function(x, y) {
  z <- x > y
  z[(is.nan(x) | is.nan(y)) & !(rp_missing(x) | rp_missing(y))] <- FALSE
  z
}
rp_fold <- function(x, f) {
  x <- x[!rp_missing(x)]
  if (!length(x)) NA_real_ else f(x)
}
rp_report <- function(args, path) {
  output <- suppressWarnings(system2("data-dict", c(args, shQuote(path)), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(output, "status"))) stop("data-dict validation failed")
  jsonlite::fromJSON(paste(output, collapse = "\n"), simplifyVector = FALSE)
}
rp_source <- function(path, expected, name) {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  snapshot <- file.path(root, basename(path))
  stopifnot(file.copy(path, snapshot))
  model <- rp_report("export-spec", snapshot)
  stopifnot(identical(model, jsonlite::fromJSON(expected, simplifyVector = FALSE)))
  sources <- list()
  for (table in model$tables) {
    rel <- table$source$parquet
    if (is.null(rel) || startsWith(rel, "/") || ".." %in% strsplit(rel, "/", fixed = TRUE)[[1]]) stop("Unsupported snapshot path")
    dest <- file.path(root, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(dest)) stopifnot(file.copy(file.path(dirname(path), rel), dest))
    sources[[table$name]] <- dest
  }
  for (level in c("validate-meta", "validate-data")) {
    report <- rp_report(c(level, "--json"), snapshot)
    stopifnot(identical(report$status, "ok"), all(vapply(report$steps, function(s) identical(s$outcome, "pass"), logical(1))))
  }
  nanoparquet::read_parquet(sources[[name]])
}

|}

let compile ~directory declarations =
  let emit (d,path) =
    let open Analysis_program in
    if List.exists (fun (c : Contract.column) -> c.restricted) d.table.columns then
      failwith "Export of restricted data requires authorization";
    let pipeline = lower ~fields:d.fields d.plan in
    Printf.sprintf "rp_source(%s, %s, %s) |>\n  %snanoparquet::write_parquet(%s)\n"
      (q d.dictionary.path) (q (Yojson.Basic.to_string d.dictionary.metadata)) (q d.table.name)
      (if pipeline="" then "" else pipeline ^ " |>\n  ") (q path) in
  preamble ^ String.concat "\n" (List.map emit (Analysis_program.compile ~directory declarations))
