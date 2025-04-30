# ---- Capture Object Class and Methods ----

#' Create a capture_object
#'
#' Defines a lightweight container for storing code expressions
#' with a custom class.
#'
#' @param exprs A list of expressions (default: empty list).
#' @return An object of class `capture_object`.
#' @export
capture_object <- function(exprs = list()) {
  structure(exprs, class = "capture_object")
}

#' Subset a capture_object
#'
#' Provides subsetting (`[`) for `capture_object` while preserving the class.
#'
#' @param x A `capture_object`.
#' @param i Subset index.
#' @param ... Additional arguments (ignored).
#' @return A subsetted `capture_object`.
#' @export
`[.capture_object` <- function(x, i, ...) {
  subset <- NextMethod("[")
  class(subset) <- "capture_object"
  subset
}

#' Replace expressions inside a code_capture
#'
#' Provides assignment (`[<-`) for replacing elements of a `code_capture` object.
#'
#' @param x A `code_capture` object.
#' @param i Index to replace.
#' @param value New value(s) (must be a call or list of calls).
#' @return The modified `code_capture` object.
#' @export
`[<-.code_capture` <- function(x, i, value) {
  stopifnot(is.call(value) || is.list(value))
  if (is.call(value)) value <- list(value)
  x$expressions[i] <- value
  x
}

# ---- Print Method ----

#' Print a code_capture object
#'
#' Prints a summary of the `code_capture` object, including its type and contained expressions.
#'
#' @param x A `code_capture` object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns the object.
#' @export
print.code_capture <- function(x, ...) {
  cat("== Code Capture ==\n")
  cat("Type: ", x$capture_type, "\n")
  cat("Expressions:\n")
  exprs <- get_expr_text(x, collapse = "\n")
  for (i in seq_along(exprs)) {
    cat(sprintf("[%d] %s\n", i, exprs[[i]]))
  }
  invisible(x)
}

#' Subset a code_capture object
#'
#' Provides subsetting (`[`) for `code_capture` while preserving the class and metadata.
#'
#' @param x A `code_capture` object.
#' @param i Subset indices.
#' @param ... Ignored.
#' @return A subsetted `code_capture` object.
#' @export
`[.code_capture` <- function(x, i, ...) {
  stopifnot(inherits(x, "code_capture"))

  subset_expressions <- x$expressions[i]

  # subset meta too, but safely (meta can be NULL or shorter than expressions)
  subset_meta <- if (!is.null(x$meta) && length(x$meta) >= max(i, na.rm = TRUE)) {
    x$meta[i]
  } else {
    vector("list", length(subset_expressions))
  }

  structure(
    list(
      capture_type = x$capture_type,
      expressions = subset_expressions,
      meta = subset_meta
    ),
    class = "code_capture"
  )
}
