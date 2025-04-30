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
