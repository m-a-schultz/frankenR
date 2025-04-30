# ---- Rerunning Captures ----

#' Rerun captured code
#'
#' Evaluates expressions from a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @param envir The environment to evaluate in (default: parent.frame()).
#' @param verbose Logical; print each expression before evaluation (default: FALSE).
#' @param stop_on_error Logical; stop immediately if an error occurs (default: TRUE).
#' @param collect_results Logical; collect and return results in a list (default: TRUE).
#' @param new_env Logical; if TRUE, create a new clean environment (default: FALSE).
#' @return A list of results (if collect_results = TRUE), otherwise invisibly TRUE.
#' @export
rerun_capture <- function(capture,
                          envir = parent.frame(),
                          verbose = FALSE,
                          stop_on_error = TRUE,
                          collect_results = TRUE,
                          new_env = FALSE) {
  stopifnot(inherits(capture, "code_capture"))

  exprs <- capture$expressions

  if (new_env) {
    envir <- new.env(parent = emptyenv())
  }

  results <- list()

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]

    if (verbose) {
      cat(sprintf("[[%d]] %s\n", i, paste(deparse(expr), collapse = "\n")))
    }

    if (stop_on_error) {
      result <- eval(expr, envir = envir)
    } else {
      result <- tryCatch(eval(expr, envir = envir), error = function(e) e)
    }

    if (collect_results) {
      results[[i]] <- result
    }
  }

  if (collect_results) {
    invisible(results)
  } else {
    invisible(TRUE)
  }
}

# ---- Exporting Captures ----

#' Export captured code to a script file
#'
#' Writes the expressions from a `code_capture` object into a script file.
#'
#' @param capture A `code_capture` object.
#' @param path The file path to write to.
#' @param overwrite Logical; overwrite existing file? (default: FALSE).
#' @param meta Character; one of "none", "code", or "comments" (default: "none").
#' @details
#' The `meta` argument controls how metadata is handled during export:
#' \itemize{
#'   \item \code{"none"}: Metadata is ignored. Only the captured expressions are written.
#'   \item \code{"comments"}: Metadata is exported as comments (\code{# key: value}) immediately before each associated expression.
#'   \item \code{"code"}: Metadata is exported as actual \code{meta(...)} function calls immediately before each associated expression.
#' }
#'
#' Expressions are exported exactly as captured, without wrapping, indenting, or additional blank lines.
#'
#' @return Invisibly TRUE on success.
#' @export
export_capture <- function(capture,
                           path,
                           overwrite = FALSE,
                           meta = c("none", "code", "comments")) {
  # Validate capture object and match meta mode
  stopifnot(inherits(capture, "code_capture"))
  meta <- match.arg(meta)

  # Prevent accidental overwrite unless explicitly allowed
  if (file.exists(path) && !overwrite) {
    stop("File already exists. Set overwrite = TRUE to overwrite.")
  }

  # Get expressions and their corresponding metadata
  exprs <- capture$expressions
  meta_info <- capture$meta

  output_lines <- character()

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    expr_text <- paste(deparse(expr), collapse = "\n")

    # Handle metadata (if any exists for this expression)
    if (meta != "none" &&
        length(meta_info) >= i &&
        length(meta_info[[i]]) > 0) {
      meta_entry <- meta_info[[i]]

      if (meta == "comments") {
        # Convert metadata to `# key: value` lines
        comment_lines <- paste0("# ", names(meta_entry), ": ", vapply(meta_entry, deparse, character(1)))
        output_lines <- c(output_lines, comment_lines)

      } else if (meta == "code") {
        # Convert metadata to a real `meta(...)` call
        meta_call <- as.call(c(as.name("meta"), meta_entry))
        meta_text <- paste(deparse(meta_call), collapse = "\n")
        output_lines <- c(output_lines, meta_text)
      }
    }

    # Append the main expression after meta (if any)
    output_lines <- c(output_lines, expr_text)
  }

  # Write all lines to the file
  writeLines(output_lines, con = path)

  invisible(TRUE)
}
