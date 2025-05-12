# ---- Full Atomization ----

#' Fully atomize an expression
#'
#' Breaks a call into a sequence of sub-expressions, assigning intermediate results
#' to temporary variables, up to a given depth.
#'
#' @param expr A call object.
#' @param prefix Prefix for naming temporary variables.
#' @param depth Maximum recursion depth (default: Inf).
#' @return A list of calls: assignments followed by the final call.
#' @export
atomize_expr <- function(expr, prefix = "tmp", depth = Inf) {
  result <- atomize_expr_with_counter(expr, prefix = prefix, counter = 1, depth = depth)
  result$expressions
}


#' Fully atomize an expression with an external counter and depth control
#'
#' Like `atomize_expr()` but allows a depth limit and initial counter to be passed in.
#'
#' @param expr A call object.
#' @param prefix Prefix for naming temporary variables.
#' @param counter Initial counter value.
#' @param depth Maximum recursion depth (default: Inf).
#' @return A list containing `expressions` and updated `counter`.
#' @export
atomize_expr_with_counter <- function(expr, prefix = "tmp", counter = 1, depth = Inf) {
  stopifnot(is.call(expr))
  local_counter <- counter
  generated <- list()

  atomize_recursive <- function(e, level) {
    if (!is.call(e)) return(e)

    args <- as.list(e)
    fn <- args[[1]]
    args_rest <- args[-1]

    if (identical(fn, as.name("<-"))) {
      lhs <- args_rest[[1]]
      rhs <- atomize_recursive(args_rest[[2]], level)
      return(call("<-", lhs, rhs))
    }

    new_args <- lapply(args_rest, function(arg) {
      if (is.call(arg) && level < depth) {
        name <- paste0(prefix, local_counter)
        local_counter <<- local_counter + 1
        sub_expr <- atomize_recursive(arg, level + 1)
        generated[[length(generated) + 1]] <<- call("<-", as.name(name), sub_expr)
        as.name(name)
      } else {
        arg
      }
    })

    as.call(c(fn, new_args))
  }

  top_expr <- atomize_recursive(expr, level = 1)
  list(expressions = c(generated, list(top_expr)), counter = local_counter)
}



#' Fully atomize a code_capture object with depth control
#'
#' Breaks all complex expressions in a `code_capture` object into sequences of simpler expressions,
#' with optional control over atomization depth.
#'
#' @param capture A `code_capture` object.
#' @param prefix Prefix for naming temporary variables.
#' @param depth Maximum recursion depth for atomization (default: Inf).
#' @return A new `code_capture` object with atomized expressions.
#' @export
atomize_capture <- function(capture, prefix = "tmp", depth = Inf) {
  stopifnot(inherits(capture, "code_capture"))

  all_exprs <- list()
  all_meta <- list()
  counter <- 1

  for (i in seq_along(capture$data)){
    expr <- capture$data[[i]]$expr
    meta <- capture$data[[i]]$meta
    if (!is.call(expr)) {
      all_exprs[[length(all_exprs) + 1]] <- expr
    } else {
      atomized <- atomize_expr_with_counter(expr, prefix, counter, depth = depth)
      new_meta <- rep(list(meta), length(atomized$expressions))
      counter <- atomized$counter
      all_exprs <- c(all_exprs, atomized$expressions)
      all_meta <- c(all_meta, new_meta)
    }
  }

  format_capture(all_exprs, all_meta, capture_type = capture$capture_type)
}

#' Selectively atomize parts of a code_capture object
#'
#' Decomposes specified function calls into separate expressions across a `code_capture`,
#' respecting a recursion depth.
#'
#' @param capture A `code_capture` object.
#' @param fn_names Character vector of functions to atomize.
#' @param prefix Prefix for temporary variable names.
#' @param depth Maximum recursion depth (default: Inf).
#' @return A new `code_capture` object.
#' @export
atomize_selective_capture <- function(capture, fn_names, prefix = "tmp", depth = Inf) {
  stopifnot(inherits(capture, "code_capture"))

  all_exprs <- list()
  all_meta <- list()
  counter <- 1

  for (i in seq_along(capture$data)){
    expr <- capture$data[[i]]$expr
    meta <- capture$data[[i]]$meta
    if (!is.call(expr)) {
      all_exprs[[length(all_exprs) + 1]] <- expr
    } else {
      atomized <- atomize_selective_expr_with_counter(expr, fn_names, prefix, counter, depth = depth)
      new_meta <- rep(list(meta), length(atomized$expressions))
      counter <- atomized$counter
      all_exprs <- c(all_exprs, atomized$expressions)
      all_meta <- c(all_meta, new_meta)
    }
  }

  format_capture(all_exprs, all_meta, capture_type = capture$capture_type)
}

#' Selectively atomize parts of an expression
#'
#' Decomposes only calls to specified function names into separate expressions,
#' up to a given recursion depth.
#'
#' @param expr A call object.
#' @param fn_names Character vector of function names to atomize.
#' @param prefix Prefix for temporary variable names.
#' @param depth Maximum recursion depth (default: Inf).
#' @return A list of expressions, including assignments and the final call.
#' @export
atomize_selective_expr <- function(expr, fn_names, prefix = "tmp", depth = Inf) {
  atomize_selective_expr_with_counter(expr, fn_names, prefix, counter = 1, depth = depth)$expressions
}

#' Selectively atomize an expression with an external counter and depth control
#'
#' Like `atomize_selective_expr()`, but accepts and updates an external counter and limits depth.
#'
#' @param expr A call object.
#' @param fn_names Character vector of functions to atomize.
#' @param prefix Prefix for temporary variable names.
#' @param counter Initial counter value (will be updated).
#' @param depth Maximum recursion depth for selective atomization (default: Inf).
#' @return A list with `expressions` (list of calls) and updated `counter`.
#' @export
atomize_selective_expr_with_counter <- function(expr, fn_names, prefix = "tmp", counter = 1, depth = Inf) {
  stopifnot(is.call(expr))

  local_counter <- counter
  generated <- list()

  selective_recursive <- function(e, level) {
    if (!is.call(e)) return(e)

    args <- as.list(e)
    fn <- args[[1]]
    fn_char <- if (is.symbol(fn)) {
      as.character(fn)
    } else if (is.call(fn) && identical(fn[[1]], as.name("::"))) {
      as.character(fn[[3]])
    } else {
      NULL
    }

    if (!is.null(fn_char) && fn_char %in% fn_names && level <= depth) {
      name <- paste0(prefix, local_counter)
      local_counter <<- local_counter + 1
      generated[[length(generated) + 1]] <<- call("<-", as.name(name), e)
      return(as.name(name))
    }

    args_rest <- args[-1]
    new_args <- lapply(args_rest, selective_recursive, level = level + 1)
    as.call(c(fn, new_args))
  }

  top_expr <- selective_recursive(expr, level = 1)
  list(expressions = c(generated, list(top_expr)), counter = local_counter)
}

