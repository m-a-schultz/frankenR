#' @importFrom utils modifyList getParseData
#' @importFrom stats setNames
NULL

# ---- Internal Store ----
.capture_store <- new.env(parent = emptyenv())
.capture_store$active <- FALSE
.capture_store$buffer <- list()

# ---- Capture API ----


#' General capture interface
#' @param x A code block `{}` or missing
#' @param script Optional path to script
#' @param envir Environment to associate capture with
#' @return A `code_capture` object or TRUE if session started
#' @export
capture <- function(x, script = NULL, envir=parent.frame()) {
  if (!is.null(script)) return(capture_script(script))
  if (missing(x)) {
    start_capture(envir=envir,nframe=sys.nframe())
    return(invisible(TRUE))
  }
  expr <- substitute(x)
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    return(capture_block(expr))
  }
  stop("Unsupported input: must be a code block `{}` or a script file path (use `script =` argument).")
}

#' Start capturing top-level expressions
#' @param clear Whether to clear previous buffer
#' @param envir Environment to associate capture with
#' @param nframe Internal safeguard
#' @return Invisibly TRUE
#' @export
start_capture <- function(clear = TRUE, envir = parent.frame(), nframe = sys.nframe()) {
  if (nframe != 1) {
    warning("start_capture() must be called from the top-level (global frame). Capture not started.")
    return(invisible(FALSE))
  }
  if (clear) .capture_store$buffer <- list()
  .capture_store$active <- TRUE
  .capture_store$envir <- envir

  if (!"code_logger" %in% getTaskCallbackNames()) {
    addTaskCallback(function(expr, value, ok, visible) {
      if (isTRUE(.capture_store$active) && ok) {
        .capture_store$buffer[[length(.capture_store$buffer) + 1]] <<- expr
      }
      TRUE
    }, name = "code_logger")
  }
  invisible(TRUE)
}

#' Finalize session capture
#'
#' @param realize Realize arguments (default FALSE)
#' @param envir Evaluation environment
#' @return A list of `callobj` objects
#' @export
end_capture <- function(realize = FALSE,envir = parent.frame()) {
  if (!.capture_store$active) stop("No active capture session")
  raw_calls <- .capture_store$buffer
  .capture_store$buffer <- list()
  .capture_store$active <- FALSE
  raw_calls <- Filter(function(expr) {
    !(is.call(expr) && as.character(expr[[1]]) %in% c("start_capture", "capture"))
  }, raw_calls)
  calls <- process_expressions(raw_calls)
  calls <- apply_inline_meta(calls)
  capture <- format_capture(calls, capture_type = 'session')
  process_capture(capture, realize = realize, evaluate = FALSE, envir = envir)
}

#' Capture a code block
#' @param expr A `{}` block
#' @param realize Realize arguments (default FALSE)
#' @param evaluate Evaluate immediately (default FALSE)
#' @param envir Evaluation environment
#' @return A `code_capture` object
#' @export
capture_block <- function(expr, realize = FALSE, evaluate = FALSE, envir = parent.frame()) {
  expr_list <- if (is.call(expr) && identical(expr[[1]], as.name("{"))) as.list(expr[-1]) else list(expr)
  calls <- process_expressions(expr_list)
  calls <- apply_inline_meta(calls)

  capture <- format_capture(calls, capture_type = 'block')
  process_capture(capture, realize = realize, evaluate = evaluate, envir = envir)
}

#' Capture a script file
#' @param path Path to script
#' @param encoding File encoding (default UTF-8)
#' @param realize Realize arguments (default FALSE)
#' @param evaluate Evaluate immediately (default FALSE)
#' @param envir Evaluation environment
#' @return A `code_capture` object
#' @export
capture_script <- function(path, encoding = "UTF-8", realize = FALSE, evaluate = FALSE, envir = parent.frame()) {
  if (!file.exists(path)) stop("File not found: ", path)
  raw_lines <- readLines(path, encoding = encoding)
  meta_by_line <- extract_meta_comments(raw_lines)

  exprs <- parse(file = path, encoding = encoding,keep.source=TRUE)
  parsed_data <- utils::getParseData(exprs)
  expr_rows <- parsed_data[parsed_data$parent == 0, ]
  expr_list <- as.list(exprs)

  final_calls <- list()
  for (i in seq_along(expr_list)) {
    expr <- expr_list[[i]]
    line_range <- expr_rows$line1[i]:expr_rows$line2[i]

    comment_meta <- list()
    for (ln in line_range) {
      tag <- as.character(ln)
      if (tag %in% names(meta_by_line)) {
        comment_meta <- utils::modifyList(comment_meta, meta_by_line[[tag]])
      }
    }

    captured <- new_callobj(expr, comment_meta)
    final_calls[[length(final_calls) + 1]] <- captured
  }

  calls <- apply_inline_meta(final_calls)

  capture <- format_capture(calls, capture_type = 'script')
  process_capture(capture, realize = realize, evaluate = evaluate, envir = envir)
}

#' Process expressions into `callobj` list
#' @param expr_list A list of unevaluated calls
#' @return A list of capture_call structures
#' @keywords internal
process_expressions <- function(expr_list) {
  final_calls <- list()
  for (expr in expr_list) {
    if (is.call(expr) && identical(expr[[1]], as.name("meta"))) {
      meta <- extract_named_meta(expr)
      final_calls <- append_meta_to_last(final_calls, meta)
    } else {
      captured <- new_callobj(expr)
      if (!is.null(captured))
        final_calls[[length(final_calls) + 1]] <- captured
    }
  }
  final_calls
}



#' Realize and/or evaluate a capture
#' @param capture A `code_capture` object
#' @param realize Logical; whether to realize arguments
#' @param evaluate Logical; whether to evaluate expressions
#' @param envir Environment to use
#' @return A processed `code_capture` object
#' @keywords internal
process_capture <- function(capture, realize = FALSE, evaluate = FALSE, envir = parent.frame()) {
  stopifnot(inherits(capture, "code_capture"))
  if (!realize && !evaluate) return(capture)

  if (!realize && evaluate) {
    for (expr in get_expressions(capture)) eval(expr, envir = envir)
    return(capture)
  }

  if (realize && !evaluate) {
    sandbox <- new.env(parent = envir)
    realized_exprs <- lapply(get_expressions(capture), function(expr) {
      eval(expr, sandbox)
      realize_args(expr, sandbox)
    })
    return(update_capture(capture, expr = realized_exprs))
  }

  if (realize && evaluate) {
    realized_exprs <- list()
    for (expr in get_expressions(capture)) {
      realized <- realize_args(expr, envir)
      eval(realized, envir)
      realized_exprs[[length(realized_exprs) + 1]] <- realized
    }
    return(update_capture(capture, expr = realized_exprs))
  }

  stop("Unhandled processing combination.")
}
