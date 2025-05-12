# ---- Capture Verification with Detailed Mismatch Categories ----

#' Verify output of a capture against an environment
#'
#' Compares the values of assigned variables from a `code_capture` object against those
#' in a given environment, checking for consistency and reproducibility.
#'
#' The function re-executes the capture in a copy of the given environment and compares
#' the results of all variables assigned during the capture. If `details = TRUE`, it returns
#' a list categorizing matched, mismatched, and missing variables.
#'
#' @param capture A `code_capture` object to verify.
#' @param envir The environment considered to contain the correct reference values. Defaults to the calling environment.
#' @param details Logical; if TRUE, returns a list of mismatches and matches by category. If FALSE, returns a simple TRUE/FALSE.
#'
#' @return Either a logical (TRUE/FALSE) or a list with fields:
#' \describe{
#'   \item{matches}{Variables that matched exactly.}
#'   \item{value_mismatches}{Variables that exist in both but differ in value.}
#'   \item{missing_in_capture_result}{Variables expected but not produced by the capture.}
#'   \item{missing_in_reference}{Variables produced by the capture but missing in the reference environment.}
#' }
#' @export
verify_capture <- function(capture, envir = parent.frame(), details = FALSE) {
  stopifnot(inherits(capture, "code_capture"))
  stopifnot(is.environment(envir))

  assigned_vars <- unique(unlist(lapply(get_expressions(capture), function(expr) {
    if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=")) {
      as.character(expr[[2]])
    } else NULL
  })))

  temp_env <- new.env(parent = parent.env(envir))
  for (name in ls(envir, all.names = TRUE)) {
    assign(name, get(name, envir = envir), envir = temp_env)
  }

  results <- tryCatch({
    rerun_capture(capture, envir = temp_env, stop_on_error = TRUE, collect_results = FALSE)
    TRUE
  }, error = function(e) FALSE)

  if (!results) {
    if (details) {
      return(list(
        matches = character(0),
        value_mismatches = character(0),
        missing_in_capture_result = assigned_vars,
        missing_in_reference = character(0)
      ))
    }
    return(FALSE)
  }

  matches <- c()
  value_mismatches <- c()
  missing_in_capture_result <- c()
  missing_in_reference <- c()

  for (name in assigned_vars) {
    in_temp <- exists(name, envir = temp_env, inherits = FALSE)
    in_orig <- exists(name, envir = envir, inherits = FALSE)

    if (!in_temp) {
      missing_in_capture_result <- c(missing_in_capture_result, name)
    } else if (!in_orig) {
      missing_in_reference <- c(missing_in_reference, name)
    } else if (!identical(get(name, envir = temp_env), get(name, envir = envir))) {
      value_mismatches <- c(value_mismatches, name)
    } else {
      matches <- c(matches, name)
    }
  }

  if (details) {
    list(
      matches = matches,
      value_mismatches = value_mismatches,
      missing_in_capture_result = missing_in_capture_result,
      missing_in_reference = missing_in_reference
    )
  } else {
    length(value_mismatches) == 0 &&
      length(missing_in_capture_result) == 0 &&
      length(missing_in_reference) == 0
  }
}
