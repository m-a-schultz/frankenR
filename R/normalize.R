# ---- Normalization Functions ----

#' Normalize all calls inside an expression
#'
#' Recursively normalizes a call or list of calls by naming unnamed arguments
#' where safe and possible.
#'
#' @param expr A call, expression, or list of calls.
#' @param env Environment to find functions (default: `parent.frame()`).
#' @param partial_match Logical; whether to allow partial matching of argument names (default: TRUE).
#' @return A normalized version of the expression or block.
#' @export
normalize_calls <- function(expr, env = parent.frame(), partial_match = TRUE) {
  if (is.call(expr)) {
    expr_norm <- tryCatch(
      normalize_call(expr, env = env, partial_match = partial_match),
      error = function(e) expr  # If error, leave as-is
    )

    expr_args <- as.list(expr_norm)
    for (i in seq_along(expr_args)) {
      if (i == 1) next  # Skip function name
      expr_args[[i]] <- normalize_calls(expr_args[[i]], env = env, partial_match = partial_match)
    }

    as.call(expr_args)

  } else if (is.expression(expr) || is.list(expr)) {
    # Recurse through each element
    as.call(lapply(expr, normalize_calls, env = env, partial_match = partial_match))

  } else {
    expr
  }
}

#' Normalize a single call by naming unnamed arguments
#'
#' Attempts to add names to unnamed arguments in a call based on the function's formals.
#'
#' @param expr A call object.
#' @param env Environment to find the function (default: `parent.frame()`).
#' @param partial_match Logical; whether to allow partial matching of argument names (default: TRUE).
#' @return A call object with named arguments where safe.
#' @export
normalize_call <- function(expr, env = parent.frame(), partial_match = TRUE) {
  if (!is.call(expr)) return(expr)  # Leave non-calls unchanged

  fn_name <- as.character(expr[[1]])
  fn <- tryCatch(get(fn_name, envir = env), error = function(e) NULL)
  if (is.null(fn)) return(expr)  # Cannot find function; return unchanged

  formals_list <- tryCatch(formals(fn), error = function(e) NULL)
  if (is.null(formals_list)) return(expr)  # Cannot read formals

  formal_args <- names(formals_list)
  if (is.null(formal_args)) return(expr)

  call_args <- as.list(expr)[-1]
  arg_names <- names(call_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(call_args))
  }

  normalized_args <- list()
  formal_pos <- 1

  for (i in seq_along(call_args)) {
    name <- arg_names[i]

    if (is.null(name) || name == "") {
      # Unnamed: match positionally
      while (formal_pos <= length(formal_args) && formal_args[formal_pos] == "...") {
        formal_pos <- formal_pos + 1
      }
      if (formal_pos <= length(formal_args)) {
        normalized_args[[formal_args[formal_pos]]] <- call_args[[i]]
        formal_pos <- formal_pos + 1
      } else {
        normalized_args[[i]] <- call_args[[i]]
      }
    } else if (partial_match) {
      # Named: check for exact or safe partial match
      exact_match <- name %in% formal_args
      partial_matches <- which(startsWith(formal_args, name))

      if (exact_match) {
        normalized_args[[name]] <- call_args[[i]]
      } else if (length(partial_matches) == 1) {
        normalized_args[[formal_args[partial_matches]]] <- call_args[[i]]
      } else {
        normalized_args[[name]] <- call_args[[i]]
      }
    } else {
      normalized_args[[name]] <- call_args[[i]]
    }
  }

  as.call(c(expr[[1]], normalized_args))
}

#' Normalize a code_capture object
#'
#' Normalizes expressions inside a code_capture object by naming unnamed arguments
#' based on the corresponding function's formals.
#'
#' @param capture A code_capture object.
#' @param env Environment for function lookup (default: parent.frame()).
#' @param partial_match Logical; allow partial matching of argument names (default: TRUE).
#' @return A new normalized code_capture object.
#' @export
normalize_capture <- function(capture, env = parent.frame(), partial_match = TRUE) {
  stopifnot(inherits(capture, "code_capture"))

  normalize_single_expr <- function(expr) {
    if (!is.call(expr)) return(expr)

    fn_name <- as.character(expr[[1]])
    fn <- tryCatch(get(fn_name, envir = env), error = function(e) NULL)
    if (is.null(fn)) return(expr)

    formals_list <- tryCatch(formals(fn), error = function(e) NULL)
    if (is.null(formals_list)) return(expr)

    formal_args <- names(formals_list)
    if (is.null(formal_args)) return(expr)

    call_args <- as.list(expr)[-1]
    arg_names <- names(call_args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_args))
    }

    normalized_args <- list()
    formal_pos <- 1

    for (i in seq_along(call_args)) {
      name <- arg_names[i]

      if (is.null(name) || name == "") {
        # Unnamed: match positionally
        while (formal_pos <= length(formal_args) && formal_args[formal_pos] == "...") {
          formal_pos <- formal_pos + 1
        }
        if (formal_pos <= length(formal_args)) {
          normalized_args[[formal_args[formal_pos]]] <- call_args[[i]]
          formal_pos <- formal_pos + 1
        } else {
          normalized_args[[i]] <- call_args[[i]]
        }
      } else if (partial_match) {
        # Named: check for exact or safe partial match
        exact_match <- name %in% formal_args
        partial_matches <- which(startsWith(formal_args, name))

        if (exact_match) {
          normalized_args[[name]] <- call_args[[i]]
        } else if (length(partial_matches) == 1) {
          normalized_args[[formal_args[partial_matches]]] <- call_args[[i]]
        } else {
          normalized_args[[name]] <- call_args[[i]]
        }
      } else {
        normalized_args[[name]] <- call_args[[i]]
      }
    }

    as.call(c(expr[[1]], normalized_args))
  }

  normalized_exprs <- lapply(capture$expressions, normalize_single_expr)

  format_capture(normalized_exprs, capture_type = capture$capture_type, meta = capture$meta)
}


#' Check if a function or call accepts a specific argument
#'
#' Tests whether a function (or the function of a call) accepts the given argument name.
#'
#' @param func_or_call A function, function name (character or symbol), or a call expression.
#' @param arg_name The argument name to check for.
#' @param env Environment to search for the function if a name is given (default: parent.frame()).
#' @return Logical TRUE/FALSE.
#' @export
accepts_arg <- function(func_or_call, arg_name, env = parent.frame()) {
  # If it's a call, extract the function
  if (is.call(func_or_call)) {
    func_or_call <- func_or_call[[1]]
  }

  # Now treat it as function name or object
  if (is.character(func_or_call) || is.symbol(func_or_call)) {
    func_or_call <- tryCatch(get(as.character(func_or_call), envir = env), error = function(e) NULL)
  }

  if (is.null(func_or_call) || !is.function(func_or_call)) {
    return(FALSE)
  }

  formals_list <- tryCatch(formals(args(func_or_call)), error = function(e) NULL)
  if (is.null(formals_list)) {
    return(FALSE)
  }

  arg_name %in% names(formals_list)
}
