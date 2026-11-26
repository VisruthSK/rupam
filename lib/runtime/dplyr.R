suppressPackageStartupMessages(library(dplyr))

rp_wider <- function(data, ids, key, value, labels) {
  if (!all(data[[key]] %in% labels)) stop("Unexpected pivot key")
  if (anyDuplicated(data[c(ids, key)])) stop("Duplicate pivot cell")
  data[[key]] <- factor(data[[key]], levels = labels)
  tidyr::pivot_wider(data, id_cols = all_of(ids), names_from = all_of(key),
                     values_from = all_of(value), names_expand = TRUE)
}




options(arrow.int64_downcast = FALSE)

rp_read <- function(path, format, columns) {
  types <- list(Int64 = arrow::int64(), Float64 = arrow::float64(), Bool = arrow::boolean(), String = arrow::utf8(), Date = arrow::date32())
  data <- if (format == "parquet") {
    arrow::read_parquet(path, col_select = dplyr::all_of(names(columns)), as_data_frame = FALSE)
  } else {
    fields <- Map(function(name, kind) arrow::field(name, types[[kind]]), names(columns), columns)
    arrow::read_delim_arrow(path, schema = arrow::schema(unname(fields)), skip = 1, na = "",
                            parse_options = arrow::CsvParseOptions$create(delimiter = if (format == "tsv") "\t" else ",", ignore_empty_lines = FALSE),
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
