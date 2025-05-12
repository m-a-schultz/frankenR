# ---- Normalization Functions ----

#' Normalize a single call by naming unnamed arguments
#'
#' Attempts to add names to unnamed arguments in a call based on the function's formals.
#'
#' @param expr A call object or callobj.
#' @param env Environment to find the function (default: `parent.frame()`).
#' @param partial_match Logical; whether to allow partial matching of argument names (default: TRUE).
#' @param strict Logical; whether to forcibly assign argument names even for positional matches (default: TRUE).
#' @return The modified call or callobj (same type as input).
#' @export
normalize_call <- function(expr, env = parent.frame(), partial_match = TRUE, strict = TRUE) {
  e <- get_expr(expr)
  if (!is.call(e)) return(expr)

  # Special case: assignment → only normalize RHS
  if (is_assignment(expr)) {
    rhs <- get_rhs(expr)
    new_rhs <- normalize_call(rhs, env = env, partial_match = partial_match, strict = strict)
    return(set_rhs(expr, new_rhs))
  }

  # Normal case: function call
  fn_name <- e[[1]]
  fn <- tryCatch(
    if (is.symbol(fn_name)) get(as.character(fn_name), envir = env) else NULL,
    error = function(e) NULL
  )
  if (is.null(fn) || !is.function(fn)) return(expr)

  formals_list <- tryCatch(formals(fn), error = function(e) NULL)
  if (is.null(formals_list)) return(expr)

  formal_args <- names(formals_list)
  if (is.null(formal_args)) return(expr)

  call_args <- as.list(e)[-1]
  arg_names <- names(call_args)
  if (is.null(arg_names)) arg_names <- rep("", length(call_args))

  normalized_args <- list()
  formal_pos <- 1

  for (i in seq_along(call_args)) {
    name <- arg_names[i]

    if (is.null(name) || name == "") {
      # Unnamed → match by position
      while (formal_pos <= length(formal_args) && formal_args[formal_pos] == "...") {
        formal_pos <- formal_pos + 1
      }
      if (formal_pos <= length(formal_args)) {
        matched_name <- formal_args[formal_pos]
        if (strict) {
          normalized_args[[matched_name]] <- call_args[[i]]
        } else {
          normalized_args[[i]] <- call_args[[i]]
        }
        formal_pos <- formal_pos + 1
      } else {
        normalized_args[[i]] <- call_args[[i]]
      }
    } else if (partial_match) {
      # Named → check partial match
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

  new_call <- as.call(c(fn_name, normalized_args))
  match_input(expr, new_call)
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

  normalized_exprs <- lapply(get_expressions(capture), normalize_call)
  update_capture(capture, expr=normalized_exprs)
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
