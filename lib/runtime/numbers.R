rp_fail <- function(code, message) {
  stop(structure(class = c("rupam_error", "error", "condition"),
                 list(message = message, call = NULL, code = code)))
}

rp_exact_float <- function(x) {
  d <- suppressWarnings(as.double(x))
  if (any(!is.na(x) & bit64::as.integer64(d) != x)) rp_fail("inexact-float", "an Int has no exact Float value")
  d
}

rp_checked <- function(x) {
  withCallingHandlers(
    if (is.double(x) && any(is.nan(x) | is.infinite(x))) rp_fail("arithmetic-overflow", "arithmetic overflow: a result is not finite") else x,
    warning = function(w) if (grepl("integer64 overflow", conditionMessage(w))) rp_fail("arithmetic-overflow", "arithmetic overflow: Int out of range")
  )
}
