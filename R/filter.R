# ---- Filtering Functions ----

#' Filter captured expressions by function name
#'
#' Selects only those expressions where the top-level function matches
#' one of the specified names.
#'
#' @param capture A `code_capture` object.
#' @param fn_names A character vector of function names to keep.
#' @return A filtered `code_capture` object.
#' @export
filter_by_function <- function(capture, fn_names) {
  stopifnot(inherits(capture, "code_capture"))
  fn_names <- as.character(fn_names)

  keep <- vapply(capture$expressions, function(e) {
    if (!is.call(e)) return(FALSE)
    fname <- e[[1]]
    if (is.call(fname) && identical(fname[[1]], as.name("::"))) {
      as.character(fname[[3]]) %in% fn_names
    } else {
      as.character(fname) %in% fn_names
    }
  }, logical(1))

  format_capture(capture$expressions[keep], capture_type = capture$capture_type)
}

#' Filter expressions using a custom predicate
#'
#' Selects expressions for which the given predicate function returns TRUE.
#'
#' @param capture A `code_capture` object.
#' @param predicate A function taking a call and returning TRUE or FALSE.
#' @return A filtered `code_capture` object.
#' @export
filter_by_predicate <- function(capture, predicate) {
  stopifnot(inherits(capture, "code_capture"))
  keep <- vapply(capture$expressions, predicate, logical(1))
  format_capture(capture$expressions[keep], capture_type = capture$capture_type)
}

#' Check if an expression is an assignment
#'
#' Detects whether the top-level call is an assignment (`<-` or `=`).
#'
#' @param expr A call or list of calls.
#' @return Logical TRUE/FALSE (or logical vector if input is a list).
#' @export
is_assignment <- function(expr) {
  if (is.list(expr)) {
    return(vapply(expr, is_assignment, logical(1)))
  }
  if (!is.call(expr)) return(FALSE)

  fun <- as.character(expr[[1]])
  fun %in% c("<-", "=")
}
