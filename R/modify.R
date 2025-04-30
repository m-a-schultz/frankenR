# ---- Argument Helpers ----

#' Check if a call has a specific argument
#'
#' Tests whether a named argument exists in a function call.
#'
#' @param expr A call object.
#' @param name The argument name to check for.
#' @return Logical TRUE/FALSE.
#' @export
has_arg <- function(expr, name) {
  if (!is.call(expr)) return(FALSE)
  name %in% names(as.list(expr))
}

#' Get an argument from a call
#'
#' Retrieves the value of a named argument in a function call.
#'
#' @param expr A call object.
#' @param name The argument name to retrieve.
#' @return The argument value, or NULL if not found.
#' @export
get_arg <- function(expr, name) {
  if (!is.call(expr)) return(NULL)
  args <- as.list(expr)
  args[[name]]
}

# ---- Argument Modification ----

#' Set or add an argument in a call
#'
#' Sets an argument to a new value in a call expression (or adds it if not present).
#'
#' @param expr A call or list of calls.
#' @param name The argument name.
#' @param value The new value for the argument.
#' @return The modified call (or list of calls).
#' @export
set_arg <- function(expr, name, value) {
  #browser()
  if (is.list(expr)) {
    return(lapply(expr, set_arg, name = name, value = value))
  }
  stopifnot(is.call(expr))
  args <- as.list(expr)
  args[[name]] <- value
  as.call(c(args[[1]], args[-1]))
}

#' Change an existing argument in a call
#'
#' Changes the value of an existing argument in a call. Throws an error if the argument does not exist.
#'
#' @param expr A call or list of calls.
#' @param name The argument name to change.
#' @param new_value The new value to assign.
#' @return The modified call (or list of calls).
#' @export
change_arg <- function(expr, name, new_value) {
  if (is.list(expr)) {
    return(lapply(expr, change_arg, name = name, new_value = new_value))
  }
  stopifnot(is.call(expr))
  if (!has_arg(expr, name)) stop("Argument not found: ", name)
  set_arg(expr, name, new_value)
}

#' Remove an argument from a call
#'
#' Deletes a named argument from a call expression.
#'
#' @param expr A call or list of calls.
#' @param name The argument name to remove.
#' @return The modified call (or list of calls).
#' @export
remove_arg <- function(expr, name) {
  if (is.list(expr)) {
    return(lapply(expr, remove_arg, name = name))
  }
  stopifnot(is.call(expr))
  args <- as.list(expr)
  if(name %in% names(args))
    args[name] <- NULL
  as.call(c(args[[1]], args[-1]))
}

#' Add a new argument to a call
#'
#' Appends or inserts a new argument into a call expression.
#'
#' @param expr A call or list of calls.
#' @param value The value to add as an argument.
#' @param name Optional name for the new argument.
#' @param position Optional position to insert the argument (1 = first, etc.).
#' @return The modified call (or list of calls).
#' @export
add_arg <- function(expr, value, name = NULL, position = NULL) {
  if (is.list(expr)) {
    return(lapply(expr, add_arg, value = value, name = name, position = position))
  }
  stopifnot(is.call(expr))
  args <- as.list(expr)
  fn <- args[[1]]
  rest <- args[-1]

  new_arg <- if (is.null(name)) list(value) else stats::setNames(list(value), name)

  if (!is.null(position)) {
    rest <- append(rest, new_arg, after = position - 1)
  } else {
    rest <- c(rest, new_arg)
  }

  as.call(c(fn, rest))
}

#' Change the function in a call
#'
#' Replaces the function name of a call while keeping its arguments.
#'
#' @param expr A call or list of calls.
#' @param new_func The new function name (symbol or character).
#' @return The modified call (or list of calls).
#' @export
change_func <- function(expr, new_func) {
  if (is.list(expr)) {
    return(lapply(expr, change_func, new_func = new_func))
  }
  stopifnot(is.call(expr))
  if (is.character(new_func)) new_func <- as.name(new_func)
  args <- as.list(expr)[-1]
  as.call(c(list(new_func), args))
}

#' Wrap an expression inside a function
#'
#' Creates a new call by wrapping the given expression inside another function call.
#'
#' @param expr A call or list of calls.
#' @param wrapper_fn A function name (symbol or character) to wrap with.
#' @return A wrapped call (or list of calls).
#' @export
wrap_expr <- function(expr, wrapper_fn) {
  if (is.list(expr)) {
    return(lapply(expr, wrap_expr, wrapper_fn = wrapper_fn))
  }
  stopifnot(is.call(expr))

  #if (is.character(wrapper_fn)) wrapper_fn <- as.name(wrapper_fn)
  call(wrapper_fn, expr)
}

#' Unwrap the outer function call of an expression
#'
#' Removes the top-level function call and returns the first argument inside.
#'
#' @param expr A call or list of calls.
#' @return The unwrapped expression(s).
#' @export
unwrap_expr <- function(expr) {
  if (is.list(expr)) {
    return(lapply(expr, unwrap_expr))
  }
  stopifnot(is.call(expr))

  args <- as.list(expr)[-1]
  if (length(args) == 1) {
    args[[1]]
  } else {
    as.call(c(as.name("{"), args))  # Wrap multiple arguments inside `{}` block
  }
}
