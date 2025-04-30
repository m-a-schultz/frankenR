#' Check if an object is a call or a list of calls
#'
#' Determines whether the input is a single function call or a list entirely composed of calls.
#'
#' @param x An object to check.
#' @return Logical TRUE/FALSE.
is_call_or_list <- function(x) {
  is.call(x) || (is.list(x) && all(vapply(x, is.call, logical(1))))
}

# ---- Accessors ----

#' Get the function name from a call
#'
#' Extracts the function name (symbol) from a call expression.
#'
#' @param expr A call object.
#' @return The function name as a symbol, or NULL if not a call.
#' @export
get_function_name <- function(expr) {
  if (is.call(expr)) expr[[1]] else NULL
}

#' Get the arguments from a call
#'
#' Returns a list of arguments from a call expression.
#'
#' @param expr A call object.
#' @return A list of arguments, or NULL if not a call.
#' @export
get_arguments <- function(expr) {
  if (is.call(expr)) as.list(expr[-1]) else NULL
}

#' Get the deparsed text of expressions in a capture
#'
#' Converts all expressions in a `code_capture` object to character strings.
#'
#' @param capture A `code_capture` object.
#' @param collapse String used to collapse multi-line expressions.
#' @return A character vector with one element per expression.
#' @export
get_expr_text <- function(capture, collapse = "\n") {
  stopifnot(inherits(capture, "code_capture"))
  vapply(capture$expressions, function(e) {
    paste(deparse(e), collapse = collapse)
  }, character(1))
}

#' Get all arguments from all expressions in a capture
#'
#' Extracts arguments for each call in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @return A list of argument lists.
#' @export
get_all_arguments <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  lapply(capture$expressions, get_arguments)
}


