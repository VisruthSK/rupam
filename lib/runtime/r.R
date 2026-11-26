suppressPackageStartupMessages(library(bit64))

rp_variant <- function(tag, ...) structure(list(...), tag = tag)

rp_dynamic <- function(v) {
  tag <- attr(v, "tag")
  if (!is.null(tag))
    return(if (length(v) == 0) tag else paste0(tag, "(", paste(vapply(v, rp_dynamic, ""), collapse = ", "), ")"))
  if (is.null(v)) return("None")
  if (bit64::is.integer64(v)) return(as.character(v))
  if (is.logical(v)) return(if (v) "TRUE" else "FALSE")
  if (is.double(v)) return(rp_float(v))
  if (is.character(v)) return(encodeString(v, quote = '"'))
  if (is.function(v)) return("<function>")
  if (!is.null(names(v)))
    return(paste0("{", paste(names(v), "=", vapply(v, rp_dynamic, ""), collapse = ", "), "}"))
  if (is.list(v)) return(paste0("[", paste(vapply(v, rp_dynamic, ""), collapse = ", "), "]"))
  paste(format(v), collapse = " ")
}

rp_format <- function(v, shape) {
  if (is.character(shape)) {
    return(switch(shape,
      int = as.character(v),
      float = rp_float(v),
      bool = if (isTRUE(v)) "TRUE" else "FALSE",
      string = encodeString(v, quote = '"'),
      date = paste0('d"', format(v), '"'),
      duration = paste0("days(", v, ")"),
      period = paste0("months(", v, ")"),
      `function` = "<function>",
      rp_dynamic(v)))
  }
  switch(shape[[1]],
    list = paste0("[", paste(vapply(v, rp_format, "", shape[[2]]), collapse = ", "), "]"),
    option = if (is.null(v) || (length(v) == 1 && is.na(v))) "None" else rp_format(v, shape[[2]]),
    tuple = paste0("(", paste(mapply(rp_format, v, shape[-1]), collapse = ", "), ")"),
    record = paste0("{", paste(vapply(shape[[2]], function(f)
      paste0(f[[1]], " = ", rp_format(v[[f[[1]]]], f[[2]])), ""), collapse = ", "), "}"),
    rp_dynamic(v))
}

rp_show <- function(v, shape) rp_format(v, jsonlite::fromJSON(shape, simplifyVector = FALSE))

rp_seq <- function(first, last, step = 1L) {
  if (step == 0) stop("seq needs a nonzero step")
  if ((last - first) * step < 0) return(list())
  lapply(seq_len(as.integer((last - first) %/% step) + 1L), function(k) first + (k - 1L) * step)
}

rp_element <- function(x, i) {
  n <- length(x)
  if (is.null(i)) i <- n
  if (i < 1 || i > n) stop("index ", i, " is outside 1..", n, call. = FALSE)
  x[[as.integer(i)]]
}

rp_slice <- function(x, a, b) {
  n <- length(x)
  a <- if (is.null(a)) 1L else as.integer(a)
  b <- if (is.null(b)) n else as.integer(b)
  if (a < 1 || b > n) stop("slice ", a, "..=", b, " is outside 1..", n, call. = FALSE)
  if (a > b) list() else x[a:b]
}

rp_coalesce <- function(x, fallback) if (is.null(x)) fallback else x

rp_sort <- function(xs) {
  absent <- vapply(xs, is.null, logical(1))
  present <- xs[!absent]
  if (length(present) == 0) return(xs)
  c(present[order(do.call(c, present))], xs[absent])
}

rp_unique <- function(xs) {
  absent <- vapply(xs, is.null, logical(1))
  repeated <- logical(length(xs))
  repeated[!absent] <- duplicated(do.call(c, xs[!absent]))
  repeated[absent] <- duplicated(absent[absent])
  xs[!repeated]
}

rp_add_months <- function(x, n) {
  lt <- as.POSIXlt(x)
  total <- lt$year * 12L + lt$mon + as.integer(n)
  first <- as.Date(sprintf("%04d-%02d-01", total %/% 12L + 1900L, total %% 12L + 1L))
  last <- as.Date(sprintf("%04d-%02d-01", (total + 1L) %/% 12L + 1900L, (total + 1L) %% 12L + 1L)) - 1L
  pmin(first + lt$mday - 1L, last)
}

rp_parse_date <- function(s, f) {
  d <- as.Date(s, format = f)
  d[!is.na(d) & format(d, f) != s] <- NA
  d
}

rp_and <- function(a, b) {
  if (isFALSE(a) || isFALSE(b)) FALSE else if (is.null(a) || is.null(b)) NULL else TRUE
}

rp_or <- function(a, b) {
  if (isTRUE(a) || isTRUE(b)) TRUE else if (is.null(a) || is.null(b)) NULL else FALSE
}

rp_approx <- function(a, b) abs(a - b) <= sqrt(.Machine$double.eps) * pmax(abs(a), abs(b))

rp_vreduce <- function(kind, policy, xs) {
  absent <- vapply(xs, is.null, logical(1))
  if (policy == "keep" && !(kind %in% c("n_distinct", "first", "last")) && any(absent)) return(NULL)
  kept <- if (policy == "keep") xs else xs[!absent]
  if (kind %in% c("first", "last")) {
    if (!length(kept)) return(NULL)
    return(kept[[if (kind == "first") 1L else length(kept)]])
  }
  if (kind == "n_distinct") return(bit64::as.integer64(length(rp_unique(kept))))
  present <- xs[!absent]
  if (!length(present) || (kind %in% c("var", "sd") && length(present) < 2)) return(NULL)
  v <- do.call(c, present)
  switch(kind,
    sum = sum(v),
    min = min(v),
    max = max(v),
    mean = mean(as.double(v)),
    median = stats::median(as.double(v)),
    var = stats::var(as.double(v)),
    sd = stats::sd(as.double(v)))
}
