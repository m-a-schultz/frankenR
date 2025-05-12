# ---- Operators ----

#' Add or modify arguments in a call
#'
#' @param x A call object.
#' @param y A named list (for setting arguments) or a single value (for appending).
#' @export
`+.callobj` <- function(x, y) {
  e1 <- x$expr
  e2 <- y
  stopifnot(is.call(e1))

  if (is.list(e2) && !is.null(names(e2)) && all(names(e2) != "")) {
    # Named list: set or modify arguments
    for (nm in names(e2)) {
      e1 <- set_arg(e1, nm, e2[[nm]])
    }
    x$expr <- e1
    return(x)
  }

  # Single value: append as unnamed argument
  new_callobj(add_arg(e1, value = y), x$meta)
}

#' Remove arguments from a call
#'
#' @param x A call object.
#' @param y A character vector (names) or numeric vector (positions).
#' @export
`-.callobj` <- function(x, y) {
  e1 <- x$expr
  e2 <- y
  stopifnot(is.call(e1))

  args <- as.list(e1)
  fn <- args[[1]]
  args_rest <- args[-1]

  if (is.character(e2)) {
    # Remove by name
    args_rest <- args_rest[!names(args_rest) %in% e2]

  } else if (is.numeric(e2)) {
    # Remove by position (unnamed only)
    arg_names <- names(args_rest)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(args_rest))
    }
    unnamed_idx <- which(arg_names == "")
    if (any(e2 > length(unnamed_idx))) {
      stop("Index out of bounds in '-.callobj'.")
    }
    to_remove <- unnamed_idx[e2]
    args_rest <- args_rest[-to_remove]

  } else {
    stop("Unsupported type for '-.callobj'. Must be character or numeric.")
  }

  x$expr <- as.call(c(fn, args_rest))
  x
}


#' Extract an argument by name or position
#'
#' @param x A call object.
#' @param i A character (name) or numeric (position).
#' @param ... Ignored. Included for method consistency.
#' @export
`[.callobj` <- function(x, i, ...) {

  #stopifnot(is.call(x))
  x <- x$expr
  args <- as.list(x)[-1]

  if (is.character(i)) {
    args[[i]]
  } else if (is.numeric(i)) {
    arg_names <- names(args)
    if (is.null(arg_names)) {
      unnamed_args <- args
    } else {
      unnamed_args <- args[arg_names == "" | is.na(arg_names)]
    }

    if (i > length(unnamed_args)) stop("Subscript out of bounds for unnamed argument.")
    unnamed_args[[i]]
  } else {
    stop("Unsupported index type for '[[.callobj'. Must be character or numeric.")
  }
}

#' Length of a code_capture object
#'
#' @param x `code_capture` object.
#' @return The number of expressions in the capture
#' @export
`length.code_capture` <- function(x) {
  return( length(x$data))
}

#' Concatenate two code_capture objects
#'
#' Adds two `code_capture` objects together using `+`, combining expressions and metadata.
#'
#' @param e1 First `code_capture` object.
#' @param e2 Second `code_capture` object.
#' @return A new combined `code_capture` object.
#' @export
`+.code_capture` <- function(e1, e2) {
  stopifnot(inherits(e1, "code_capture"), inherits(e2, "code_capture"))

  new_capture_type <- paste(e1$capture_type, "+", e2$capture_type)
  new_capture(rbind(e1$data,
                    e2$data),
              capture_type = new_capture_type)
}

#' Extract an expression from a code_capture object
#'
#' Allows using `[[` to access a specific captured expression.
#'
#' @param x A `code_capture` object.
#' @param i A single index.
#' @param ... Ignored.
#' @return A single expression (call object).
#' @export
`[[.code_capture` <- function(x, i, ...) {
  stopifnot(inherits(x, "code_capture"))
  x$data[[i]]
}

#' Replace an expression inside a code_capture object
#'
#' Allows using `[[<-` to assign a new expression at a specific index.
#'
#' @param x A `code_capture` object.
#' @param i Index to replace.
#' @param value A call object (expression) to assign.
#' @return The modified `code_capture` object.
#' @export
`[[<-.code_capture` <- function(x, i, value) {
  stopifnot(inherits(x, "code_capture"))
  stopifnot(inherits(value,"callobj"))

  x$data[[i]] <- value
  x
}

#' Convert a call object to a list
#'
#' This method defines `as.list()` for objects of class `"callobj"`, allowing
#' function call expressions to be treated as lists of their components.
#' It temporarily removes the `"call"` class before conversion to avoid
#' dispatch issues and ensure correct coercion.
#'
#' @param x A call object.
#' @param ... Unsupported
#' @return A list where the first element is the function being called in the expression,
#'   followed by its arguments.
#' @export
as.list.callobj <- function(x, ...) {
  x <- unclass(x$expr)
  base::as.list(x)
}
