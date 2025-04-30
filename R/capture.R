#' @importFrom utils modifyList getParseData
#' @importFrom stats setNames
NULL

# ---- Session Capture ----

# ---- Internal Store ----
.capture_store <- new.env(parent = emptyenv())
.capture_store$active <- FALSE
.capture_store$buffer <- list()


#' Start capturing top-level expressions
#'
#' Begins recording expressions evaluated at the top level during the session.
#'
#' @param clear Logical; if TRUE, clears previous captures before starting.
#' @param envir Environment where capture will evaluate (usually left as default).
#' @param nframe Internal frame-checking safeguard (do not modify).
#' @return Invisibly returns TRUE.
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
      if (isTRUE(.capture_store$active)) {
        if (ok) {
          .capture_store$buffer[[length(.capture_store$buffer) + 1]] <<- expr
        }
      }
      TRUE
    }, name = "code_logger")
  }

  invisible(TRUE)
}



#' Start a capture session or capture code
#'
#' Depending on the input:
#' - If no input, starts a session capture (top-level expressions).
#' - If given a `{}` block, captures it immediately.
#' - If given a script file path, captures and parses it.
#'
#' @param x Optional: A code block (`{}`)
#' @param script Optional: script file path.
#' @return A `code_capture` object, or starts a capture session.
#' @export
capture <- function(x, script = NULL) {
  if (!is.null(script)) {
    return(capture_script(script))
  }

  if (missing(x)) {
    start_capture()
    return(invisible(TRUE))
  }

  expr <- substitute(x)

  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    return(capture_block(expr))
  }

  stop("Unsupported input: must be a code block `{}` or a script file path (use `script =` argument).")
}




#' End an active capture session
#'
#' Processes the collected expressions and attaches metadata if present.
#'
#' @return A `code_capture` object.
#' @export
capture_end <- function() {
  calls <- end_capture()
  expressions <- lapply(calls, function(x) x$expr)
  meta_info <- lapply(calls, function(x) x$meta)

  structure(list(
    capture_type = "session",
    expressions = expressions,
    meta = meta_info
  ), class = "code_capture")
}

#' End capturing and process expressions
#'
#' Stops the capture session and processes the collected expressions,
#' handling any attached metadata.
#'
#' @return A list of captured and processed expressions.
#' @export
end_capture <- function() {
  if (!.capture_store$active) {
    stop("No active capture session")
  }

  raw_calls <- .capture_store$buffer
  .capture_store$buffer <- list()
  .capture_store$active <- FALSE

  # --- Filter out start_capture() and capture() calls (very rare, defensive only) ---
  raw_calls <- Filter(function(expr) {
    !(is.call(expr) && as.character(expr[[1]]) %in% c("start_capture", "capture"))
  }, raw_calls)

  final_calls <- list()

  for (i in seq_along(raw_calls)) {
    expr <- raw_calls[[i]]

    if (is.call(expr) && identical(expr[[1]], as.name("meta"))) {
      if (length(final_calls) == 0) {
        warning("Orphan meta() with no previous call - ignored.")
        next
      }

      meta_args <- as.list(expr)[-1]
      clean_meta <- list()
      for (j in seq_along(meta_args)) {
        nm <- names(meta_args)[j]
        if (startsWith(nm, "._")) {
          clean_name <- substr(nm, 3, nchar(nm))
          clean_meta[[clean_name]] <- meta_args[[j]]
        }
      }

      final_calls[[length(final_calls)]]$meta <- utils::modifyList(
        final_calls[[length(final_calls)]]$meta,
        clean_meta
      )

    } else {
      captured <- capture_call(expr)
      final_calls[[length(final_calls) + 1]] <- captured
    }
  }

  final_calls
}


# Updated capture_block and capture_script to process meta() inside the block or script
capture_block <- function(expr) {
#  expr <- substitute(expr)

  expr_list <- if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    as.list(expr[-1])
  } else {
    list(expr)
  }

  calls <- process_expressions(expr_list)
  expressions <- lapply(calls, function(x) x$expr)
  meta_info <- lapply(calls, function(x) x$meta)

  structure(list(
    capture_type = "block",
    expressions = expressions,
    meta = meta_info
  ), class = "code_capture")
}

#' Capture code from a script
#'
#' @param path script file path.
#' @param encoding file encoding of script
#' @return A `code_capture` object
#' @export
capture_script <- function(path, encoding = "UTF-8") {
  if (!file.exists(path)) stop("File not found: ", path)

  # Read source lines and parse expressions
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

    # Create base capture structure
    captured <- capture_call(expr)

    # Attach meta() metadata from source comments
    comment_meta <- list()
    for (ln in line_range) {
      tag <- as.character(ln)
      if (tag %in% names(meta_by_line)) {
        comment_meta <- utils::modifyList(comment_meta, meta_by_line[[tag]])
      }
    }

    captured$meta <- comment_meta
    final_calls[[length(final_calls) + 1]] <- captured
  }

  # Then apply inline meta() call logic to enrich metadata
  enriched_calls <- list()
  for (i in seq_along(final_calls)) {
    expr <- final_calls[[i]]$expr

    if (is.call(expr) && identical(expr[[1]], as.name("meta"))) {
      if (length(enriched_calls) == 0) {
        warning("Orphan meta() call with no preceding expression in script.")
        next
      }

      meta_args <- as.list(expr)[-1]
      for (j in seq_along(meta_args)) {
        nm <- names(meta_args)[j]
        if (startsWith(nm, "._")) {
#          clean_name <- substr(nm, 3, nchar(nm))
          enriched_calls[[length(enriched_calls)]]$meta[[nm]] <- meta_args[[j]]
        }
      }
    } else {
      enriched_calls[[length(enriched_calls) + 1]] <- final_calls[[i]]
    }
  }

  expressions <- lapply(enriched_calls, `[[`, "expr")
  meta_info <- lapply(enriched_calls, `[[`, "meta")

  structure(list(
    capture_type = "script",
    expressions = expressions,
    meta = meta_info
  ), class = "code_capture")
}
# capture_script <- function(path, encoding = "UTF-8") {
#   if (!file.exists(path)) {
#     stop("File not found: ", path)
#   }
#
#   exprs <- parse(file = path, encoding = encoding)
#   calls <- process_expressions(as.list(exprs))
#   expressions <- lapply(calls, function(x) x$expr)
#   meta_info <- lapply(calls, function(x) x$meta)
#
#   structure(list(
#     capture_type = "script",
#     expressions = expressions,
#     meta = meta_info
#   ), class = "code_capture")
# }

# General worker function to process expressions and attach meta()
process_expressions <- function(expr_list) {
  final_calls <- list()

  for (i in seq_along(expr_list)) {
    expr <- expr_list[[i]]

    if (is.call(expr) && identical(expr[[1]], as.name("meta"))) {
      if (length(final_calls) == 0) {
        warning("Orphan meta() with no previous call - ignored.")
        next
      }

      meta_args <- as.list(expr)[-1]
      clean_meta <- list()
      for (j in seq_along(meta_args)) {
        nm <- names(meta_args)[j]
        if (startsWith(nm, "._")) {
          clean_name <- substr(nm, 3, nchar(nm))
          clean_meta[[clean_name]] <- meta_args[[j]]
        }
      }

      final_calls[[length(final_calls)]]$meta <- utils::modifyList(
        final_calls[[length(final_calls)]]$meta,
        clean_meta
      )

    } else {
      captured <- capture_call(expr)
      final_calls[[length(final_calls) + 1]] <- captured
    }
  }
  final_calls <- Filter(Negate(is.null), final_calls)

  final_calls
}

# Adjusted capture_call to initialize meta
capture_call <- function(expr) {
  if (!is.call(expr)) {
    warning("Non-call object encountered during capture. Skipping.")
    return(NULL)
  }

  list(
    expr = expr,
    meta = list()
  )
}


# ---- Format Captures ----

#' Format a set of expressions into a code_capture object
#'
#' Internal function to wrap expressions and optional metadata into a capture object.
#'
#' @param expr A call or list of calls.
#' @param capture_type A string describing the capture type ("block", "script", etc.).
#' @param meta A list of metadata (default empty list).
#' @return A `code_capture` object.
#' @keywords internal
format_capture <- function(expr, capture_type = "unknown", meta = list()) {
  exprs <- if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    as.list(expr[-1])
  } else if (is.call(expr)) {
    list(expr)
  } else if (is.list(expr) && all(vapply(expr, is.call, logical(1)))) {
    expr
  } else {
    stop("Unsupported input: must be a call or list of calls.")
  }

  structure(list(
    capture_type = capture_type,
    expressions = exprs,
    meta = meta
  ), class = "code_capture")
}
