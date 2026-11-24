open Shape

let q = Polars.quoted

let lower ?(annotate = Source_map.plain) ~fields plan =
  let names = Array.make plan.Table.next "" in
  List.iteri (fun i n -> names.(i) <- n) fields;
  let next = ref (List.length fields) in
  let bind n =
    names.(!next) <- n;
    incr next
  in
  let rec emit e =
    match e.node with
    | Input i -> ".data[[" ^ q names.(i) ^ "]]"
    | Broadcast (_, e) -> emit e
    | Constant (`Int i) -> "bit64::as.integer64(" ^ q (Int64.to_string i) ^ ")"
    | Constant (`Float f) -> Printf.sprintf "%.17g" f
    | Constant (`Bool b) -> if b then "TRUE" else "FALSE"
    | Choose (c, t, f) -> "dplyr::if_else(" ^ emit c ^ ", " ^ emit t ^ ", " ^ emit f ^ ")"
    | Coalesce (a, b) -> "rp_coalesce(" ^ emit a ^ ", " ^ emit b ^ ")"
    | Polars_sqrt _ ->
      raise (Error "R target cannot execute the Python capability polars.sqrt")
    | Abs a when a.typ.primitive = Float64 -> "abs(" ^ emit a ^ ")"
    | Abs _ -> raise (Error "R Int64 abs requires checked overflow")
    | Binary (op, a, b) ->
      if a.typ.primitive = Int64 && op <> Greater
      then raise (Error "R Int64 arithmetic requires checked overflow");
      let op =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Greater -> ">"
      in
      if op = ">" && a.typ.primitive = Float64
      then "rp_gt(" ^ emit a ^ ", " ^ emit b ^ ")"
      else "(" ^ emit a ^ " " ^ op ^ " " ^ emit b ^ ")"
    | Reduce (r, a) ->
      let f =
        match r with
        | Mean -> "mean"
        | Sd -> "sd"
        | Sum -> "sum"
      in
      "rp_fold(" ^ emit a ^ ", " ^ f ^ ")"
  in
  let stage = function
    | Table.Pivot p ->
      let ids = List.map fst p.ids in
      let ls xs = "c(" ^ String.concat ", " (List.map q xs) ^ ")" in
      List.iter bind (ids @ if p.wider then p.labels else [ p.key; p.value ]);
      if p.wider
      then
        Printf.sprintf
          "rp_wider(%s, %s, %s, %s)"
          (ls ids)
          (q p.key)
          (q p.value)
          (ls p.labels)
      else
        Printf.sprintf
          "tidyr::pivot_longer(all_of(%s), names_to = %s, values_to = %s)"
          (ls p.labels)
          (q p.key)
          (q p.value)
    | Table.Rename changes ->
      let pair (a, b, i) =
        names.(i) <- b;
        q b ^ " = " ^ q a
      in
      "rename(" ^ String.concat ", " (List.map pair changes) ^ ")"
    | Window (n, x, keys) ->
      let v = emit x in
      bind n;
      "mutate("
      ^ q n
      ^ " := "
      ^ v
      ^ ", .by = all_of(c("
      ^ String.concat ", " (List.map (fun (k, _) -> q k) keys)
      ^ ")))"
    | Table.Take n -> Printf.sprintf "slice_head(n = %d)" n
    | Distinct keys ->
      "distinct(" ^ String.concat ", " (List.map emit keys) ^ ", .keep_all = TRUE)"
    | Filter x -> "filter(" ^ emit x ^ ")"
    | Select ns -> "select(all_of(c(" ^ String.concat ", " (List.map q ns) ^ ")))"
    | Bind (n, x) ->
      let v = emit x in
      bind n;
      "mutate(" ^ q n ^ " := " ^ v ^ ")"
    | Arrange xs ->
      "arrange("
      ^ String.concat
          ", "
          (List.map (fun (x, d) -> if d then "desc(" ^ emit x ^ ")" else emit x) xs)
      ^ ")"
    | Summarize (keys, values) ->
      let key_names = List.map fst keys in
      let by =
        if keys = []
        then []
        else [ ".by = all_of(c(" ^ String.concat ", " (List.map q key_names) ^ "))" ]
      in
      let args =
        List.map
          (fun (n, x) ->
             q n ^ " := " ^ Option.fold ~none:"bit64::as.integer64(n())" ~some:emit x)
          values
      in
      List.iter bind (key_names @ List.map fst values);
      "summarise(" ^ String.concat ", " (args @ by) ^ ")"
  in
  List.rev plan.steps
  |> List.map (Table.render_step annotate plan stage)
  |> String.concat " |>\n  "
;;

let preamble =
  {|suppressPackageStartupMessages(library(dplyr))

rp_wider <- function(data, ids, key, value, labels) {
  if (!all(data[[key]] %in% labels)) stop("Unexpected pivot key")
  if (anyDuplicated(data[c(ids, key)])) stop("Duplicate pivot cell")
  data[[key]] <- factor(data[[key]], levels = labels)
  tidyr::pivot_wider(data, id_cols = all_of(ids), names_from = all_of(key),
                     values_from = all_of(value), names_expand = TRUE)
}
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
rp_source <- function(path, expected, name, expected_types) {
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
    stopifnot(identical(report$run$tool$version, "0.0.3"), identical(report$status, "ok"), all(vapply(report$steps, function(s) identical(s$outcome, "pass"), logical(1))))
  }
  fields <- Filter(function(t) identical(t$name, name), model$tables)[[1]]$columns
  file <- sources[[name]]
  footer <- nanoparquet::read_parquet_schema(file)
  footer <- footer[!is.na(footer$r_col), ]
  types <- c(Int64 = "INT64", Float64 = "DOUBLE", String = "BYTE_ARRAY", Boolean = "BOOLEAN")
  expected_types <- unlist(jsonlite::fromJSON(expected_types), use.names = TRUE)
  stopifnot(identical(footer$name, names(expected_types)),
            identical(footer$type, unname(types[expected_types])))
  data <- nanoparquet::read_parquet(file, options = nanoparquet::parquet_options(read_int64_type = "double"))
  for (column in names(expected_types)[expected_types == "Int64"]) {
    x <- data[[column]]
    if (any(abs(x) >= 2^53, na.rm = TRUE)) stop("Int64 source exceeds exact nanoparquet conversion range")
    data[[column]] <- bit64::as.integer64(x)
  }
  data[vapply(fields, function(c) c$name, character(1))]
}

|}
;;

let rec table ?(annotate = Source_map.plain) d =
  let open Analysis_program in
  let base =
    match d.join with
    | None ->
      Printf.sprintf
        "rp_source(%s, %s, %s, %s)"
        (q d.dictionary.path)
        (q (Yojson.Basic.to_string d.dictionary.metadata))
        (q d.table.name)
        (q (Yojson.Basic.to_string d.physical_types))
    | Some { left; right; key; kind } ->
      Printf.sprintf
        "%s_join(%s, %s, by = %s, na_matches = \"never\")"
        (Analysis_program.join_name kind)
        (table ~annotate left)
        (table ~annotate right)
        (q key)
  in
  match lower ~annotate ~fields:d.fields d.plan with
  | "" -> base
  | plan -> base ^ " |>\n  " ^ plan
;;

let compile ?(annotate = Source_map.plain) ?strict ~directory declarations =
  let emit (d, path) =
    let d = Analysis_program.public_view d in
    Printf.sprintf
      "%s |>\n  nanoparquet::write_parquet(%s)\n"
      (table ~annotate d)
      (q path)
  in
  preamble
  ^ String.concat
      "\n"
      (List.map emit (Analysis_program.compile ?strict ~directory declarations))
;;

let preview_page ~runtime d ~offset ~limit =
  let d = Analysis_program.public_view d in
  let script =
    Printf.sprintf
      "%s\nprint(as_tibble(%s) |> slice(seq.int(%d, length.out = %d)), n = %d)\n"
      preamble
      (table d)
      (offset + 1)
      limit
      limit
  in
  print_endline (if d.plan.ordered then "Ordered preview" else "Unordered preview");
  Worker.run runtime Runtime.R script
;;
