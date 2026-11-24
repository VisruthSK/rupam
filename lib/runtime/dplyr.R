suppressPackageStartupMessages(library(dplyr))

rp_wider <- function(data, ids, key, value, labels) {
  if (!all(data[[key]] %in% labels)) stop("Unexpected pivot key")
  if (anyDuplicated(data[c(ids, key)])) stop("Duplicate pivot cell")
  data[[key]] <- factor(data[[key]], levels = labels)
  tidyr::pivot_wider(data, id_cols = all_of(ids), names_from = all_of(key),
                     values_from = all_of(value), names_expand = TRUE)
}
rp_coalesce <- function(x, y) {
  n <- max(length(x), length(y))
  x <- rep_len(x, n)
  missing <- is.na(x)
  x[missing] <- rep_len(y, n)[missing]
  x
}
rp_fold <- function(x, f) {
  x <- x[!is.na(x)]
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
  for (column in names(expected_types)[expected_types == "Float64"]) {
    x <- data[[column]]
    x[is.nan(x)] <- NA
    data[[column]] <- x
  }
  data[vapply(fields, function(c) c$name, character(1))]
}

