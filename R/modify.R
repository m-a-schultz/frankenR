# ---- Argument Helpers ----

#' Check if a call has a specific argument
#'
#' Tests whether a named argument exists in a function call.
#'
#' @param expr A expression or callobj
#' @param name The argument name to check for.
#' @return Logical TRUE/FALSE.
#' @export
has_arg <- function(expr, name) {
  e <- get_expr(expr)
  if (!is.call(e)) return(FALSE)
  name %in% names(as.list(e))
}

#' Get an argument from a call
#'
#' Retrieves the value of a named argument in a function call.
#'
#' @param expr A expression or callobj.
#' @param name The argument name to retrieve.
#' @return The argument value, or NULL if not found.
#' @export
get_arg <- function(expr, name) {
  e <- get_expr(expr)
  if (!is.call(e)) return(NULL)
  as.list(e)[[name]]
}

# ---- Argument Modification ----

#' Set or add an argument in a call
#'
#' Sets an argument to a new value in a call expression (or adds it if not present).
#'
#' @param expr A expression or callobj.
#' @param name The argument name.
#' @param value The new value for the argument.
#' @return The modified call (or list of calls).
#' @export
set_arg <- function(expr, name, value) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  args <- as.list(e)
  args[[name]] <- value
  new_expr <- as.call(c(args[[1]], args[-1]))
  match_input(expr, new_expr)
}

#' Change an existing argument in a call
#'
#' Changes the value of an existing argument in a call. Throws an error if the argument does not exist.
#'
#' @param expr A expression or callobj.
#' @param name The argument name to change.
#' @param new_value The new value to assign.
#' @return The modified call (or list of calls).
#' @export
change_arg <- function(expr, name, new_value) {
  if (!has_arg(expr, name)) stop("Argument not found: ", name)
  set_arg(expr, name, new_value)
}

#' Remove an argument from a call
#'
#' Deletes a named argument from a call expression.
#'
#' @param expr A expression or callobj.
#' @param name The argument name to remove.
#' @return The modified call (or list of calls).
#' @export
remove_arg <- function(expr, name) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  args <- as.list(e)
  if(name %in% names(args)) args[name] <- NULL
  new_expr <- as.call(c(args[[1]], args[-1]))
  match_input(expr, new_expr)
}

#' Add a new argument to a call
#'
#' Appends or inserts a new argument into a call expression.
#'
#' @param expr A expression or callobj
#' @param value The value to add as an argument.
#' @param name Optional name for the new argument.
#' @param position Optional position to insert the argument (1 = first, etc.).
#' @return The modified call (or list of calls).
#' @export
add_arg <- function(expr, value, name = NULL, position = NULL) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  args <- as.list(e)
  fn <- args[[1]]
  rest <- args[-1]

  new_arg <- if (is.null(name)) list(value) else stats::setNames(list(value), name)
  if (!is.null(position)) {
    rest <- append(rest, new_arg, after = position - 1)
  } else {
    rest <- c(rest, new_arg)
  }

  new_expr <- as.call(c(fn, rest))
  match_input(expr, new_expr)
}

#' Change the function in a call
#'
#' Replaces the function name of a call while keeping its arguments.
#'
#' @param expr A expression or callobj
#' @param new_func The new function name (symbol or character).
#' @return The modified call (or list of calls).
#' @export
change_func <- function(expr, new_func) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  if (is.character(new_func)) new_func <- as.name(new_func)
  args <- as.list(e)[-1]
  new_expr <- as.call(c(list(new_func), args))
  match_input(expr, new_expr)
}

#' Wrap an expression inside a function
#'
#' Creates a new call by wrapping the given expression inside another function call.
#'
#' @param expr A expression or callobj
#' @param wrapper_fn A function name (symbol or character) to wrap with.
#' @return A wrapped call (or list of calls).
#' @export
wrap_expr <- function(expr, wrapper_fn) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  new_expr <- call(wrapper_fn, e)
  match_input(expr, new_expr)
}

#' Unwrap the outer function call of an expression
#'
#' Removes the top-level function call and returns the first argument inside.
#'
#' @param expr A expression or callobj
#' @return The unwrapped expression(s).
#' @export
unwrap_expr <- function(expr) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  args <- as.list(e)[-1]
  new_expr <- if (length(args) == 1) args[[1]] else as.call(c(as.name("{"), args))
  match_input(expr, new_expr)
}


#' Duplicate an expression in a code_capture object
#'
#' Inserts a duplicate of the expression at index `i` directly after the original.
#'
#' @param capture A `code_capture` object.
#' @param i Integer index of the expression to duplicate.
#' @return A modified `code_capture` object with duplicated expression.
#' @export
duplicate_line <- function(capture, i) {
  stopifnot(inherits(capture, "code_capture"))
  stopifnot(i >= 1, i <= length(capture$data))

  idx <- seq_along(capture$data)
  idx <- sort(c(idx,i))



  # exprs <- capture$expressions
  # metas <- capture$meta %||% vector("list", length(exprs))
  #
  # capture$expressions <- append(capture$expressions, list(capture$expressions[[i]]), after = i)
  # capture$meta <- append(capture$meta %||% vector("list", length(capture$expressions)),
  #                        list(capture$meta[[i]]), after = i)
  # capture
  return(capture[idx])
}

