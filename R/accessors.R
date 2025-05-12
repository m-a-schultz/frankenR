
#' Access expressions from a capture object
#' @param capture A `code_capture` object
#' @return A list of expressions
#' @export
get_expressions <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  lapply(capture$data, `[[`, "expr")
}

#' Access metadata from a capture object
#' @param capture A `code_capture` object
#' @return A list of metadata
#' @export
get_metadata <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  lapply(capture$data, `[[`, "meta")
}

#' Replace metadata in a code_capture object
#'
#' Updates the metadata for a specific expression in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param i Integer index of the metadata entry to replace.
#' @param meta A named list containing metadata.
#' @return A modified `code_capture` object.
#' @export
set_metadata <- function(capture, i, meta) {
  stopifnot(inherits(capture, "code_capture"))
  capture$data[[i]]$meta <- meta
  capture
}

#' Replace an expression in a code_capture object
#'
#' Updates the expression at a specific index within a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param i Integer index of the expression to replace.
#' @param expr A call object representing the new expression.
#' @return A modified `code_capture` object.
#' @export
set_expression <- function(capture, i, expr) {
  stopifnot(inherits(capture, "code_capture"), is.call(expr))
  capture$data[[i]]$expr <- expr
  capture
}


#' Replace all expressions in a code_capture object
#'
#'
#' @param capture A `code_capture` object.
#' @param exprs A list of call objects
#' @return A modified `code_capture` object.
#' @export
set_expressions <- function(capture, exprs) {
  stopifnot(inherits(capture, "code_capture"))
  for(i in seq_along(exprs)){
    capture$data[[i]]$expr <- exprs[[i]]
  }
  capture
}

#' Replace all metadata in a code_capture object
#'
#' Updates the metadata for a specific expression in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param meta A list of named lists containing metadata.
#' @return A modified `code_capture` object.
#' @export
set_all_metadata <- function(capture,  meta) {
  stopifnot(inherits(capture, "code_capture"))
  for(i in seq_along(meta)){
    capture$data[[i]]$meta <- meta[[i]]
  }
  capture
}

#' Get expression and metadata pair at a given index
#'
#' Retrieves the combined expression and metadata list from a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param i Integer index to retrieve.
#' @return A list containing `expr` and `meta`.
#' @export
get_expr_meta <- function(capture, i) {
  stopifnot(inherits(capture, "code_capture"))
  capture$data[[i]]
}



# ---- Print Method ----


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

  subset_data <- x$data[i]

  new_capture(subset_data, x$capture_type)
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
  stopifnot(inherits(value,'callobj') || is.list(value))
  if (inherits(value,'code_capture')) value <- value$data
  if (inherits(value,'callobj')) value <- list(value)
  x$data[i] <- value
  x
}
