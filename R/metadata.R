#' Metadata function for attaching metadata
#'
#' `meta()` is a placeholder function used inside capture sessions
#' to attach metadata to previous expressions. It does nothing at runtime.
#'
#' @param ... Named arguments representing metadata.
#' @return Nothing. Intended for side effects only in capture processing.
#' @export
meta <- function(...) { }

extract_meta_comments <- function(lines) {
  meta_map <- list()
  for (i in seq_along(lines)) {
    if (grepl("#@", lines[[i]], fixed = TRUE)) {
      comment <- sub(".*#@\\s*", "", lines[[i]])
      parts <- strsplit(comment, ",\\s*\\._")[[1]]

      kv <- list()
      for (j in seq_along(parts)) {
        piece <- parts[[j]]
        if (j > 1) piece <- paste0("._", piece)  # restore stripped key
        keyval <- strsplit(piece, "=", fixed = TRUE)[[1]]
        if (length(keyval) == 2) {
          key <- trimws(keyval[1])
          val <- trimws(keyval[2])
          #key <- sub("^\\.*_?", "", key)  # strip leading ._
          kv[[key]] <- tryCatch(eval(parse(text = val)), error = function(e) val)
        }
      }

      meta_map[[as.character(i)]] <- kv  # attach to previous line
    }
  }
  meta_map
}


# ---- Metadata Helpers ----

#' Extract named metadata from a `meta()` call
#' @param expr A `meta(...)` call expression
#' @return A named list of metadata
#' @keywords internal
extract_named_meta <- function(expr) {
  args <- as.list(expr)[-1]
  out <- list()
  for (j in seq_along(args)) {
    nm <- names(args)[j]
    if (startsWith(nm, "._")) {
      key <- nm#substr(nm, 3, nchar(nm))
      out[[key]] <- args[[j]]
    }
  }
  out
}

#' Append metadata to the most recent captured call
#' @param call_list List of call objects of class `callobj`
#' @param meta A named list of metadata
#' @return Updated list with metadata applied to last item
#' @keywords internal
append_meta_to_last <- function(call_list, meta) {
  n <- length(call_list)
  if (n == 0) return(call_list)
  call_list[[n]]$meta <- utils::modifyList(call_list[[n]]$meta, meta)
  call_list
}

#' Apply inline `meta()` calls to call list
#' @param calls A list of captured calls with metadata
#' @return A new list with inline metadata applied
#' @keywords internal
apply_inline_meta <- function(calls) {
  result <- list()
  for (call in calls) {
    expr <- call$expr
    if (is.call(expr) && identical(expr[[1]], as.name("meta"))) {
      meta <- extract_named_meta(expr)
      result <- append_meta_to_last(result, meta)
    } else {
      result[[length(result) + 1]] <- call
    }
  }
  result
}
