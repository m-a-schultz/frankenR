# ---- Diagnostic Utilities ----

#' Recursively scan an expression tree for matching nodes
#'
#' Traverses an expression tree and collects nodes where the predicate returns TRUE.
#'
#' @param expr A call or expression object.
#' @param predicate A function taking a node and returning TRUE/FALSE.
#' @return A list of matching sub-expressions.
#' @keywords internal
scan_expr_tree <- function(expr, predicate) {
  matches <- list()
  recurse <- function(e) {
    if (predicate(e)) matches[[length(matches) + 1]] <<- e
    if (is.call(e) || is.pairlist(e)) lapply(as.list(e), recurse)
  }
  recurse(expr)
  matches
}

# ---- Shared Analysis Structure ----

#' Analyze the structure of a capture object
#'
#' Returns a detailed structure for each expression in the capture,
#' used internally by diagnostic functions.
#'
#' @param capture A `code_capture` object.
#' @return A list of analysis results (one per expression).
#' @keywords internal
analyze_capture_structure <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  lapply(get_expressions(capture), function(expr) {
    list(
      expr = expr,
      is_call = is.call(expr),
      is_meta = is.call(expr) && identical(expr[[1]], as.name("meta")),
      top_fun = if (is.call(expr)) as.character(expr[[1]]) else NA_character_,
      assigns = if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=")) as.character(expr[[2]]) else NULL,
      calls = scan_expr_tree(expr, is.call),
      symbols = scan_expr_tree(expr, function(e) is.symbol(e) && !is.call(e))
    )
  })
}

# ---- Optimized Diagnostic Functions ----

#' @keywords internal
detect_stochastic <- function(structure) {
  stochastic_fns <- c('runif', 'rnorm', 'sample', 'rbinom', 'rpois')
  any(vapply(structure, function(x) {
    any(vapply(x$calls, function(call) {
      as.character(call[[1]]) %in% stochastic_fns
    }, logical(1)))
  }, logical(1)))
}

#' @keywords internal
detect_seed_used <- function(structure) {
  any(vapply(structure, function(x) x$top_fun == "set.seed", logical(1)))
}

#' @keywords internal
detect_external_vars <- function(structure) {
  all_syms <- unique(unlist(lapply(structure, function(x) x$symbols)))
  assigned <- unique(unlist(lapply(structure, function(x) x$assigns)))

  funs <- unique(unlist(lapply(structure, function(x) {
    c(
      if (!is.na(x$top_fun)) x$top_fun else character(0),
      vapply(x$calls, function(call) as.character(call[[1]]), character(1))
    )
  })))

  # Helper function: detects operators
  is_operator <- function(name) {
    grepl("^%.*%$", name) || grepl("^[^A-Za-z0-9._]+$", name)
  }

  used_not_assigned <- setdiff(all_syms, c(assigned, funs))
  used_not_assigned <- used_not_assigned[!vapply(used_not_assigned, is_operator, logical(1))]
  used_not_assigned <- setdiff(used_not_assigned, c("TRUE", "FALSE", "NULL", "NA"))

  used_not_assigned
}



#' @keywords internal
detect_global_state <- function(structure) {
  global_calls <- c("globalenv", "assign", "get", "exists", "parent.env", "baseenv")
  any(vapply(structure, function(x) {
    any(vapply(x$calls, function(call) as.character(call[[1]]) %in% global_calls, logical(1)))
  }, logical(1)))
}

#' @keywords internal
detect_nse <- function(structure) {
  nse_calls <- c("eval", "quote", "bquote", "substitute", "match.call", "with", "subset")
  any(vapply(structure, function(x) {
    any(vapply(x$calls, function(call) as.character(call[[1]]) %in% nse_calls, logical(1)))
  }, logical(1)))
}

#' @keywords internal
detect_direct_assign <- function(structure) {
  any(vapply(structure, function(x) x$top_fun %in% c("<-", "=", "<<-"), logical(1)))
}

#' @keywords internal
detect_indirect_mutate <- function(structure) {
  mutating_calls <- c("assign", "Sys.setenv", "options")
  any(vapply(structure, function(x) {
    any(vapply(x$calls, function(call) as.character(call[[1]]) %in% mutating_calls, logical(1)))
  }, logical(1)))
}

#' @keywords internal
detect_orphan_meta <- function(structure) {
  any(vapply(seq_along(structure), function(i) {
    structure[[i]]$is_meta && i == 1
  }, logical(1)))
}

#' @keywords internal
detect_invalid_expr <- function(structure) {
  any(vapply(structure, function(x) !x$is_call, logical(1)))
}

#' @keywords internal
detect_duplicate_assigns <- function(structure) {
  assigns <- unlist(lapply(structure, function(x) x$assigns))
  any(duplicated(assigns))
}

#' @keywords internal
detect_side_effects <- function(structure) {
  side_effects <- c("print", "cat", "message", "sink", "writeLines", "write.csv")
  any(vapply(structure, function(x) {
    any(vapply(x$calls, function(call) as.character(call[[1]]) %in% side_effects, logical(1)))
  }, logical(1)))
}

#' Run diagnostics and return logical summary
#'
#' Fast, non-evaluative diagnostics on a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @return A named list of diagnostics (TRUE/FALSE or vector).
#' @keywords internal
diagnose_capture_summary_fast <- function(capture) {
  structure <- analyze_capture_structure(capture)
  list(
    stochastic = detect_stochastic(structure),
    seed_used = detect_seed_used(structure),
    external_vars = detect_external_vars(structure),
    global_state = detect_global_state(structure),
    nse = detect_nse(structure),
    direct_assign = detect_direct_assign(structure),
    indirect_mutate = detect_indirect_mutate(structure),
    orphan_meta = detect_orphan_meta(structure),
    invalid_expr = detect_invalid_expr(structure),
    duplicate_vars = detect_duplicate_assigns(structure),
    orphan_constants = detect_orphan_constants(structure),   # <<-- new line
    side_effects = detect_side_effects(structure)
  )

}

# ---- Diagnose and Result Class ----

#' Run diagnostics on a capture object
#'
#' Returns a structured list of diagnostic results indicating
#' stochasticity, side effects, non-standard evaluation, and more.
#'
#' @param capture A `code_capture` object.
#' @return An object of class `"code_diagnosis"`.
#' @export
diagnose_capture <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  results <- diagnose_capture_summary_fast(capture)
  structure(results, class = "code_diagnosis")
}

#' Print method for code diagnosis
#'
#' Displays the results of a `code_diagnosis` object in a readable format.
#'
#' @param x An object of class `"code_diagnosis"`.
#' @param ... Ignored.
#' @return Invisibly returns the diagnosis object.
#' @export
print.code_diagnosis <- function(x, ...) {
  cat("== Code Diagnosis ==\n")
  issues <- names(x)
  for (name in issues) {
    val <- x[[name]]
    if (is.logical(val)) {
      cat(sprintf(" - %-18s: %s\n", name, if (isTRUE(val)) "YES" else "no"))
    } else if (length(val) == 0) {
      cat(sprintf(" - %-18s: none\n", name))
    } else {
      cat(sprintf(" - %-18s: %s\n", name, paste(val, collapse = ", ")))
    }
  }
  invisible(x)
}


#' @keywords internal
#' @keywords internal
detect_orphan_constants <- function(structure) {
  orphan_vars <- character()

  assigned <- character()
  used_syms <- character()

  for (x in structure) {
    expr <- x$expr
    if (is_assignment(expr)) {
      rhs <- expr[[3]]
      if (is.atomic(rhs) && length(rhs) == 1) {
        assigned <- c(assigned, as.character(expr[[2]]))
      }
    }

    syms <- tryCatch(get_symbols(expr), error = function(e) list(lhs = list(symbols = character()),
                                                                 rhs = list(symbols = character())))
    used_syms <- c(used_syms, syms$rhs$symbols)
  }

  unused <- setdiff(assigned, used_syms)
  unused
}

