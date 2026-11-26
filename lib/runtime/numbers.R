rp_checked <- function(x) {
  withCallingHandlers(
    if (is.double(x) && any(is.nan(x) | is.infinite(x))) stop("arithmetic overflow: a result is not finite", call. = FALSE) else x,
    warning = function(w) if (grepl("integer64 overflow", conditionMessage(w))) stop("arithmetic overflow: Int out of range", call. = FALSE)
  )
}
