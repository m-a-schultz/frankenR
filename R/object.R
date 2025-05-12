# ---- Capture Object Classes and Methods ----

#' Create call object structure from an expression
#'
#' @param expr A call
#' @param meta A named list of metadata
#' @return A list with expr and meta fields
#' @keywords internal
new_callobj <- function(expr, meta = list()) {
  if (!is.call(expr)) {
    warning("Non-call object encountered during capture. Skipping.")
    return(NULL)
  }
  structure(
    list(expr = expr, meta = meta),
    class = 'callobj'
  )
}


new_capture <- function(expr_meta_list, capture_type = "unknown") {
  stopifnot(is.list(expr_meta_list))
  for (item in expr_meta_list) {
    stopifnot(is.list(item), "expr" %in% names(item), "meta" %in% names(item))
  }

  structure(
    list(
      capture_type = capture_type,
      data = expr_meta_list
    ),
    class = "code_capture"
  )
}

# ---- Format Captures ----

#' Format a set of expressions into a code_capture object
#'
#' Internal function to wrap expressions and optional metadata into a capture object.
#'
#' @param expr A call or list of calls.
#' @param meta A list of metadata (default empty list).
#' @param capture_type A string describing the capture type ("block", "script", etc.).
#' @return A `code_capture` object.
#' @keywords internal
format_capture <- function(expr, meta = list(), capture_type = "unknown") {
  if (is.list(expr) && all(vapply(expr, is.list, logical(1))) &&
      all(vapply(expr, function(x) all(c("expr", "meta") %in% names(x)), logical(1)))) {
    expr_meta_list <- expr
  } else if (is_call_or_list(expr)) {
    # Build unified format from expr + meta
    exprs <- if (is.call(expr)) list(expr) else expr
    if(is.null(meta) || length(meta)==0)
      meta <- vector("list", length(exprs))
    expr_meta_list <- Map(new_callobj,
                          exprs,
                          meta)
  } else {
    stop("Unsupported input to format_capture")
  }

  new_capture(expr_meta_list, capture_type)
}

#' Format a set of expressions into a code_capture object
#'
#' Modifies parts of a capture object
#'
#' @param capture A `code_capture` object to be modified
#' @param expr (optional) A call or list of calls.
#' @param meta (optional) A list of metadata (default empty list).
#' @param capture_type (optional) A string describing the capture type ("block", "script", etc.).
#' @return A `code_capture` object.
#' @keywords internal
update_capture <- function(capture, expr=NA, meta = NA, capture_type = NA) {
  if(!is.na(expr)[1])
    capture <- set_expressions(capture, expr)
  if(!is.na(meta)[1])
    capture <- set_all_metadata(capture, meta)
  if(!is.na(capture_type))
    capture$capture_type = capture_type
  capture
}

# ---- Print Method ----

#' Print method for code_capture objects
#' @param x A `code_capture` object
#' @param with_meta Boolean; Should metadata be printed
#' @param ... Ignored
#' @export
print.code_capture <- function(x, with_meta=TRUE, ...) {
  cat("== Code Capture ==\n")
  #cat("Type:", x$capture_type, "\n")
  #cat("Expressions:\n")
  for (i in seq_along(x$data)) {
    expr <- x$data[[i]]$expr
    code <- deparse(expr)
    if(with_meta){
      if(length(x$data[[i]]$meta)>0){
        comment <- paste0('     # ',
                          paste0(names(x$data[[i]]$meta),' = ', vapply(x$data[[i]]$meta, deparse, character(1)), collapse=', '))
        code <- paste0(code, comment)
      }
    }
    cat(sprintf("[%d] %s\n", i, code))
  }
  invisible(x)
}


