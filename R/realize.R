# ---- Realization Functions ----

#' Realize (evaluate) the arguments of a call
#'
#' Evaluates all arguments inside a function call, leaving the function name unchanged.
#' For assignment calls, only the RHS is evaluated; the LHS is preserved as a symbol.
#' If an evaluation fails, the original unevaluated expression is kept.
#'
#' @param expr A call or list of calls.
#' @param envir Environment for evaluation (default: `parent.frame()`).
#' @return A call with realized arguments, or a list of realized calls.
#' @export
realize_args <- function(expr, envir = parent.frame()) {
  if (is.list(expr)) {
    return(lapply(expr, realize_args, envir = envir))
  }
  stopifnot(is.call(expr))

  if (is_assignment(expr)) {
    # special case: do not realize LHS
    lhs <- expr[[2]]
    rhs <- tryCatch(eval(expr[[3]], envir = envir), error = function(e) expr[[3]])
    return(as.call(list(expr[[1]], lhs, rhs)))
  }

  args <- as.list(expr)
  fn <- args[[1]]

  realized_args <- lapply(args[-1], function(arg) {
    tryCatch(eval(arg, envir = envir), error = function(e) arg)
  })

  as.call(c(list(fn), realized_args))
}


#' Realize arguments across a code_capture object
#'
#' Evaluates the arguments of all expressions in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param envir Environment for evaluation (default: `parent.frame()`).
#' @return A new `code_capture` object with realized expressions.
#' @export
realize_capture <- function(capture, envir = parent.frame()) {
  stopifnot(inherits(capture, "code_capture"))

  realized_exprs <- list()
  for(expr in get_expressions(capture)){
    r_expr <- realize_args(expr, envir = envir)
    eval(r_expr, envir=envir)
    realized_exprs <- c(realized_exprs,
                        r_expr)
  }

  #format_capture(realized_exprs, capture_type = capture$capture_type)
  update_capture(capture, expr = realized_exprs)
}
