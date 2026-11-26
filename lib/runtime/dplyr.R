suppressPackageStartupMessages(library(dplyr))

rp_wider <- function(data, ids, key, value, labels) {
  if (!all(data[[key]] %in% labels)) stop("Unexpected pivot key")
  if (anyDuplicated(data[c(ids, key)])) stop("Duplicate pivot cell")
  data[[key]] <- factor(data[[key]], levels = labels)
  tidyr::pivot_wider(data, id_cols = all_of(ids), names_from = all_of(key),
                     values_from = all_of(value), names_expand = TRUE)
}

rp_covered <- function(left, right, key, kind) {
  sides <- switch(kind, left = list(list(left, right)), right = list(list(right, left)),
                  list(list(left, right), list(right, left)))
  for (side in sides)
    if (nrow(dplyr::anti_join(side[[1]], side[[2]], by = key, na_matches = "never")) > 0)
      stop(kind, "_join with unmatched = error found rows with no match on ", key, call. = FALSE)
  left
}

rp_assert_key <- function(data, columns) {
  if (anyNA(data[columns])) stop(paste(columns, collapse = ", "), " is not a key: some rows are missing it", call. = FALSE)
  if (anyDuplicated(data[columns])) stop(paste(columns, collapse = ", "), " is not a key: some rows repeat it", call. = FALSE)
  data
}

rp_present <- function(x) {
  if (anyNA(x)) stop("filter condition is missing and missing = error", call. = FALSE)
  x
}

options(arrow.int64_downcast = FALSE)

rp_read <- function(path, digest, format, columns) {
  bytes <- readBin(path, "raw", file.size(path))
  if (!identical(unname(tools::sha256sum(bytes = bytes)), digest))
    stop(path, " changed since rupam lock: run rupam lock, then recompile", call. = FALSE)
  types <- list(Int64 = arrow::int64(), Float64 = arrow::float64(), Bool = arrow::boolean(), String = arrow::utf8(), Date = arrow::date32())
  data <- if (format == "parquet") {
    arrow::read_parquet(bytes, col_select = dplyr::all_of(names(columns)), as_data_frame = FALSE)
  } else {
    fields <- Map(function(name, kind) arrow::field(name, types[[kind]]), names(columns), columns)
    arrow::read_delim_arrow(bytes, delim = if (format == "tsv") "\t" else ",",
                            schema = arrow::schema(unname(fields)), skip = 1, na = "",
                            as_data_frame = FALSE)
  }
  data <- dplyr::as_tibble(as.data.frame(data))
  data[] <- lapply(data, function(x) {
    if (is.double(x) && !bit64::is.integer64(x)) x[is.nan(x)] <- NA
    x
  })
  data
}

rp_result <- function(data, path) {
  arrow::write_ipc_file(data, path, chunk_size = 65536L, compression = "uncompressed")
  nrow(data)
}

rp_write <- function(data, path) {
  arrow::write_parquet(data, path)
  invisible(data)
}
