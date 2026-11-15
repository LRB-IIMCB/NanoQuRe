################################################################################
# ASSERTION HELPERS
################################################################################
#' Assert condition is TRUE, stop with message if FALSE
#'
#' Stops with the given message, without the call, unless the condition
#' evaluates to a single TRUE.
#'
#' @param cond Logical condition to check
#' @param msg Error message if condition is FALSE
#'
#' @return Invisible TRUE if assertion passes
#' @keywords internal
#'
assert_condition <- function(cond, msg = "Assertion failed") {
  if (!isTRUE(cond)) {
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Test if x is a single non-empty character string
#'
#' @param x Object to test
#' @param min.chars Minimum number of characters [1]
#' @param null.ok Allow NULL? [FALSE]
#'
#' @return TRUE if x is a valid string, FALSE otherwise
#' @keywords internal
#'
is_string <- function(x, min.chars = 1, null.ok = FALSE) {
  if (is.null(x)) {
    return(null.ok) # guard: NULL is valid only on request
  }
  is.character(x) && length(x) == 1 && !is.na(x) && nchar(x) >= min.chars
}
