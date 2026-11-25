rp_mixed <- function(f, a, b) {
  if (is.double(a) || is.double(b)) f(as.numeric(a), as.numeric(b)) else f(a, b)
}
rp_variant <- function(tag, ...) structure(list(...), tag = tag)

rp_float <- function(x) {
  for (digits in 15:17) {
    text <- sprintf("%.*g", digits, x)
    if (as.numeric(text) == x) break
  }
  if (grepl("[.eni]", text)) text else paste0(text, ".0")
}

rp_dynamic <- function(v) {
  tag <- attr(v, "tag")
  if (!is.null(tag))
    return(if (length(v) == 0) tag else paste0(tag, "(", paste(vapply(v, rp_dynamic, ""), collapse = ", "), ")"))
  if (is.null(v)) return("None")
  if (bit64::is.integer64(v)) return(as.character(v))
  if (is.logical(v)) return(if (v) "true" else "false")
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
      bool = if (isTRUE(v)) "true" else "false",
      string = encodeString(v, quote = '"'),
      `function` = "<function>",
      rp_dynamic(v)))
  }
  switch(shape[[1]],
    list = paste0("[", paste(vapply(v, rp_format, "", shape[[2]]), collapse = ", "), "]"),
    option = if (is.null(v)) "None" else paste0("Some(", rp_format(v, shape[[2]]), ")"),
    tuple = paste0("(", paste(mapply(rp_format, v, shape[-1]), collapse = ", "), ")"),
    record = paste0("{", paste(vapply(shape[[2]], function(f)
      paste0(f[[1]], " = ", rp_format(v[[f[[1]]]], f[[2]])), ""), collapse = ", "), "}"),
    rp_dynamic(v))
}

rp_show <- function(v, shape) rp_format(v, jsonlite::fromJSON(shape, simplifyVector = FALSE))
