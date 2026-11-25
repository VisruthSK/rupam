suppressPackageStartupMessages(library(dplyr))

rp_wider <- function(data, ids, key, value, labels) {
  if (!all(data[[key]] %in% labels)) stop("Unexpected pivot key")
  if (anyDuplicated(data[c(ids, key)])) stop("Duplicate pivot cell")
  data[[key]] <- factor(data[[key]], levels = labels)
  tidyr::pivot_wider(data, id_cols = all_of(ids), names_from = all_of(key),
                     values_from = all_of(value), names_expand = TRUE)
}

rp_present <- function(x) {
  if (anyNA(x)) stop("filter condition is missing and missing = error", call. = FALSE)
  x
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
  data <- rp_read(file, "parquet", unlist(jsonlite::fromJSON(expected_types)))
  data[vapply(fields, function(c) c$name, character(1))]
}

options(arrow.int64_downcast = FALSE)

rp_arrow_type <- function(kind) {
  switch(kind,
    Int64 = arrow::int64(),
    Float64 = arrow::float64(),
    Boolean = arrow::boolean(),
    String = arrow::utf8())
}

rp_read <- function(path, format, schema) {
  data <- if (format == "parquet") {
    arrow::read_parquet(path, as_data_frame = FALSE)
  } else {
    fields <- Map(function(name, kind) arrow::field(name, rp_arrow_type(kind)), names(schema), schema)
    arrow::read_delim_arrow(path, delim = if (format == "tsv") "\t" else ",",
                            schema = arrow::schema(unname(fields)), skip = 1, na = "",
                            as_data_frame = FALSE)
  }
  kinds <- c(int64 = "Int64", double = "Float64", bool = "Boolean", string = "String", large_string = "String")
  found <- kinds[vapply(data$schema$fields, function(f) f$type$ToString(), character(1))]
  if (!identical(names(data), names(schema)) || !identical(unname(found), unname(schema)))
    stop(path, " no longer matches the schema it was compiled against")
  data <- dplyr::as_tibble(as.data.frame(data))
  for (column in names(schema)[schema == "Float64"]) {
    x <- data[[column]]
    x[is.nan(x)] <- NA
    data[[column]] <- x
  }
  data
}

rp_write <- function(data, path) {
  arrow::write_parquet(data, path)
  invisible(data)
}
