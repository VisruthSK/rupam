rp_float <- function(x) {
  one <- function(v) {
    if (is.na(v)) return(NA_character_)
    for (precision in 0:16) {
      text <- sprintf("%.*e", precision, v)
      if (as.numeric(text) == v) break
    }
    negative <- startsWith(text, "-")
    digits <- gsub("[-.]", "", sub("e.*", "", text))
    exponent <- as.integer(sub(".*e", "", text))
    sign <- if (negative) "-" else ""
    if (exponent < -4 || exponent >= 16) {
      mantissa <- if (nchar(digits) > 1) paste0(substr(digits, 1, 1), ".", substring(digits, 2)) else digits
      return(paste0(sign, mantissa, "e", if (exponent < 0) "-" else "+", formatC(abs(exponent), width = 2, flag = "0")))
    }
    if (exponent < 0) return(paste0(sign, "0.", strrep("0", -exponent - 1), digits))
    padded <- paste0(digits, strrep("0", max(0, exponent + 1 - nchar(digits))))
    fraction <- substring(padded, exponent + 2)
    paste0(sign, substr(padded, 1, exponent + 1), ".", if (fraction == "") "0" else fraction)
  }
  vapply(as.numeric(x), one, character(1), USE.NAMES = FALSE)
}

rp_format_number <- function(x, spec) {
  parts <- regmatches(spec, regexec("^(?:(.)?([<>^=]))?([-+ ])?(0)?([0-9]+)?(,)?(?:\\.([0-9]+))?([dfeg%])?$", spec, perl = TRUE))[[1]]
  if (!length(parts)) stop("Unsupported format spec ", spec, call. = FALSE)
  fill <- if (nzchar(parts[2])) parts[2] else if (nzchar(parts[5])) "0" else " "
  align <- if (nzchar(parts[3])) parts[3] else if (nzchar(parts[5])) "=" else ">"
  sign <- parts[4]
  width <- if (nzchar(parts[6])) as.integer(parts[6]) else 0L
  grouping <- nzchar(parts[7])
  precision <- if (nzchar(parts[8])) as.integer(parts[8]) else 6L
  type <- parts[9]
  one <- function(v) {
    if (is.na(v)) return(NA_character_)
    negative <- v < 0
    v <- abs(v)
    body <- switch(if (nzchar(type)) type else "s",
      d = prettyNum(if (bit64::is.integer64(v)) as.character(v) else format(v, scientific = FALSE), big.mark = if (grouping) "," else ""),
      f = formatC(as.numeric(v), format = "f", digits = precision, big.mark = if (grouping) "," else ""),
      e = sprintf("%.*e", precision, as.numeric(v)),
      g = sprintf("%.*g", max(precision, 1L), as.numeric(v)),
      `%` = paste0(formatC(as.numeric(v) * 100, format = "f", digits = precision, big.mark = if (grouping) "," else ""), "%"),
      s = if (is.double(v) && !bit64::is.integer64(v)) rp_float(v) else as.character(v))
    prefix <- if (negative) "-" else if (sign == "+") "+" else if (sign == " ") " " else ""
    padding <- max(0L, width - nchar(prefix) - nchar(body))
    pad <- strrep(fill, padding)
    switch(align,
      `<` = paste0(prefix, body, pad),
      `^` = paste0(strrep(fill, padding %/% 2), prefix, body, strrep(fill, padding - padding %/% 2)),
      `=` = paste0(prefix, pad, body),
      paste0(pad, prefix, body))
  }
  vapply(as.list(x), one, character(1), USE.NAMES = FALSE)
}
