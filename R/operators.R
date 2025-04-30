# ---- Operators ----

#' Add or modify arguments in a call
#'
#' @param e1 A call object.
#' @param e2 A named list (for setting arguments) or a single value (for appending).
#' @export
`+.callobj` <- function(e1, e2) {
  stopifnot(is.call(e1))

  if (is.list(e2) && !is.null(names(e2)) && all(names(e2) != "")) {
    # Named list: set or modify arguments
    for (nm in names(e2)) {
      e1 <- set_arg(e1, nm, e2[[nm]])
    }
    return(e1)
  }

  # Single value: append as unnamed argument
  add_arg(e1, value = e2)
}

#' Remove arguments from a call
#'
#' @param e1 A call object.
#' @param e2 A character vector (names) or numeric vector (positions).
#' @export
`-.callobj` <- function(e1, e2) {
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

  as.call(c(fn, args_rest))
}


#' Extract an argument by name or position
#'
#' @param x A call object.
#' @param i A character (name) or numeric (position).
#' @param ... Ignored. Included for method consistency.
#' @export
`[.callobj` <- function(x, i, ...) {

  #stopifnot(is.call(x))
  x <- unclass(x)
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
    stop("Unsupported index type for '[[.callpbj'. Must be character or numeric.")
  }
}



#' Extract an argument by name (shorthand)
#'
#' @param x A call object.
#' @param name Name of the argument.
#' @export
`$.callobj` <- function(x, name) {
  x[name]
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

  structure(
    list(
      capture_type = new_capture_type,
      expressions = c(e1$expressions, e2$expressions),
      meta = c(e1$meta, e2$meta)
    ),
    class = "code_capture"
  )
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
  structure(
    x$expressions[[i]],
    class='callobj'
  )
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
  stopifnot(is.call(value))

  x$expressions[[i]] <- value
  x
}

#' Convert a call object to a list
#'
#' This method defines `as.list()` for objects of class `"call"`, allowing
#' function call expressions to be treated as lists of their components.
#' It temporarily removes the `"call"` class before conversion to avoid
#' dispatch issues and ensure correct coercion.
#'
#' @param x A call object.
#' @return A list where the first element is the function being called,
#'   followed by its arguments.
#' @export
as.list.callobj <- function(x) {
  x <- unclass(x)
  base::as.list(x)
}
