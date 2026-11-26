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
      `function` = "<function>",
      rp_dynamic(v)))
  }
  switch(shape[[1]],
    list = paste0("[", paste(vapply(v, rp_format, "", shape[[2]]), collapse = ", "), "]"),
    option = if (is.null(v)) "None" else rp_format(v, shape[[2]]),
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
