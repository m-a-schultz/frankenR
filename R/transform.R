#' Determine category for expression sorting
#'
#' Categorizes an expression as a constant, function definition, or other.
#'
#' @param expr A call expression.
#' @return A character string: "constant", "function", or "other".
#' @keywords internal
get_sort_category <- function(expr) {
  if (!is.call(expr) || !is_assignment(expr)) return("other")
  rhs <- expr[[3]]
  if (is.atomic(rhs) && length(rhs) == 1) return("constant")
  if (is.call(rhs) && identical(rhs[[1]], as.name("function"))) return("function")
  return("other")
}

#' Sort expressions in a code_capture object
#'
#' Sorts constant assignments and function definitions to the top,
#' preserving relative order and associated metadata.
#'
#' @param capture A `code_capture` object.
#' @return A new `code_capture` object with sorted expressions.
#' @export
sort_capture <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  categories <- vapply(get_expressions(capture), get_sort_category, character(1))
  indices <- seq_along(categories)

  ordered_indices <- c(
    indices[categories == "constant"],
    indices[categories == "function"],
    indices[categories == "other"]
  )

  capture[ordered_indices]
}

# ---- Immutable Rewriting ----

#' Make variables immutable by renaming reassignments
#'
#' Rewrites a `code_capture` so variables are never reassigned.
#' Each assignment to the same variable creates a new name (e.g., `x._1`, `x._2`, ...),
#' and all later uses are updated to use the latest version.
#'
#' @param capture A `code_capture` object.
#' @return A new `code_capture` object with immutable assignments.
#' @export
immutabilize_capture <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  assignments <- list()
  latest <- list()
  rewritten <- list()
# browser()
  for (i in seq_along(capture$data)) {
    expr <- capture$data[[i]]$expr
    original_lhs <- as.character(expr[[2]])

    # Rewrite RHS to latest versions
    expr <- rewrite_symbols(expr, latest)

    if (is_assignment(expr)) {
      lhs <- latest[[original_lhs]] %||% original_lhs

      count <- assignments[[original_lhs]]
      if (is.null(count)) count <- 0

      new_name <- if (count == 0) lhs else paste0(original_lhs, "._", count)
      expr[[2]] <- as.name(new_name)

      assignments[[original_lhs]] <- count + 1
      latest[[original_lhs]] <- new_name
    }

    rewritten[[length(rewritten) + 1]] <- expr
  }

  update_capture(capture, expr = rewritten)
}


#' Recursively replace symbols using rename map
#'
#' Walks an expression tree and replaces any symbols found in `latest` map.
#'
#' @param expr A call or symbol object.
#' @param latest A named list or environment mapping symbol names to replacements.
#' @return The transformed expression.
#' @keywords internal
rewrite_symbols <- function(expr, latest) {
  if (is.call(expr)) {
    as.call(lapply(expr, rewrite_symbols, latest))
  } else if (is.symbol(expr)) {
    name <- as.character(expr)
    if (!is.null(latest[[name]])) as.name(latest[[name]]) else expr
  } else {
    expr
  }
}

#' Compress redundant immutable variable versions
#'
#' If a reassigned variable version is only used once to create another,
#' rewrites later version to use the earlier name and removes redundancy.
#'
#' @param capture A `code_capture` object.
#' @return A new `code_capture` object with compressed variable names.
#' @export
compress_redundant_versions <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  assignments <- list()
  usage <- list()
  expr_index <- seq_along(capture$data)

  # Step 1: Track assignments and symbol usage
  for (i in expr_index) {
    expr <- capture$data[[i]]$expr

    if (is_assignment(expr)) {
      lhs <- get_lhs(expr)
      rhs <- get_rhs(expr)
      assignments[[lhs]] <- list(index = i, rhs = rhs)
    }

    for (sym in all_symbols(expr)) {
      usage[[sym]] <- c(usage[[sym]] %||% integer(), i)
    }
  }

  # Step 2: Attempt compression by base variable
  rename_map <- list()
  base_groups <- split(names(assignments), sub("\\._[0-9]+$", "", names(assignments)))
  base_groups <- base_groups[sapply(base_groups,length)>1]

  for (group in base_groups) {
    idx <- suppressWarnings(as.numeric(sub(".*\\._", "", group, perl = TRUE)))
    idx[is.na(idx)] <- 0
    versions <- group[order(idx)]
    for (j in seq_len(length(versions) - 1)) {
      prev <- versions[j]
      next_var <- versions[j + 1]

      rhs_expr <- assignments[[next_var]]$rhs
      rhs_symbols <- all_symbols(rhs_expr)

      if (prev %in% rhs_symbols) {
        use_indices <- usage[[prev]]
        assign_index <- assignments[[next_var]]$index

        if (all(use_indices <= assign_index)) {
          while(prev %in%names( rename_map) && prev != rename_map[[prev]])
            prev <- rename_map[[prev]]
          rename_map[[next_var]] <- prev
        }
      }
    }
  }

  new_exprs <- lapply(get_expressions(capture), substitute_symbols, rename_map = rename_map)
  update_capture(capture, expr=new_exprs) #format_capture(new_exprs, capture_type = capture$capture_type, meta = capture$meta)
}

# Utility Functions

get_lhs <- function(expr) as.character(expr[[2]])
get_rhs <- function(expr) expr[[3]]

all_symbols <- function(expr) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) lapply(e, walk)
    else if (is.symbol(e)) out <<- c(out, as.character(e))
  }
  walk(expr)
  unique(out)
}

#' Isolate code by realizing inputs
#'
#' Evaluates only the inputs used before they are defined, and prepends assignments
#' so the capture becomes self-contained.
#'
#' @param capture A `code_capture` object.
#' @param envir An environment in which to evaluate inputs (default: `parent.frame()`).
#' @return A new `code_capture` object with input assignments prepended. Issues warnings for any inputs not found in the environment
#' @export
isolate_capture <- function(capture, envir = parent.frame()) {
  stopifnot(inherits(capture, "code_capture"))

  input_info <- get_inputs(capture)
  input_names <- names(Filter(function(x) x$used_before_definition, input_info))

  input_assigns <- list()
  input_metas <- list()

  for (name in input_names) {
    value <- tryCatch(get(name, envir = envir), error = function(e) NULL)
    if (is.null(value)) {
      warning(sprintf("Could not resolve input: '%s'. Assignment skipped.", name))
      next
    }

    # ---- NEW CHECK: skip functions and primitives ----
    if (is.function(value) || is.primitive(value)) {
      next
    }

    input_assigns[[length(input_assigns) + 1]] <- call("<-", as.name(name), value)
    input_metas[[length(input_metas) + 1]] <- list()
  }

  format_capture(
    expr = c(input_assigns, get_expressions(capture)),
    meta = c(input_metas, get_metadata(capture)),
    capture_type = paste("realized_inputs+", capture$capture_type)
  )
}

#' Simplify a capture by removing unused constant assignments
#'
#' Removes constant assignments to variables that are never used later.
#'
#' @param capture A `code_capture` object.
#' @return A simplified `code_capture` object.
#' @export
simplify_capture <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  exprs <- get_expressions(capture)
  assigned_vars <- character()
  assigned_indices <- integer()

  # Step 1: Identify constant assignments
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    if (is_assignment(expr)) {
      rhs <- expr[[3]]
      if (is.atomic(rhs) && length(rhs) == 1) {
        assigned_vars <- c(assigned_vars, as.character(expr[[2]]))
        assigned_indices <- c(assigned_indices, i)
      }
    }
  }

  # Step 2: Check if those vars are used later
  to_remove <- logical(length(assigned_indices))
  for (j in seq_along(assigned_vars)) {
    var <- assigned_vars[j]
    later_exprs <- exprs[(assigned_indices[j] + 1):length(exprs)]
    used <- any(vapply(later_exprs, function(e) var %in% all_symbols(e), logical(1)))
    to_remove[j] <- !used
  }

  # Step 3: Remove orphan assignments
  remove_indices <- assigned_indices[to_remove]
  keep_indices <- setdiff(seq_along(capture$data), remove_indices)

  capture[keep_indices]
}


#' Standardize assignment operators to `<-`
#'
#' Rewrites any `=` assignments to use `<-` for consistency.
#'
#' @param capture A `code_capture` object.
#' @return A modified `code_capture` object.
#' @export
standardize_assignments <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  exprs <- lapply(get_expressions(capture), function(expr) {
    if (is_assignment(expr) && as.character(expr[[1]]) == "=") {
      expr[[1]] <- as.name("<-")
    }
    expr
  })

  update_capture(capture, expr = exprs)
}
#' Remove redundant self-assignments
#'
#' Removes expressions where a variable is assigned to itself (e.g., `x <- x`).
#'
#' @param capture A `code_capture` object.
#' @return A modified `code_capture` object with redundant assignments removed.
#' @export
remove_redundant_assignments <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  exprs <- get_expressions(capture)
  keep <- vapply(exprs, function(expr) {
    if (is_assignment(expr)) {
      lhs <- as.character(expr[[2]])
      rhs <- expr[[3]]
      return(!(is.symbol(rhs) && as.character(rhs) == lhs))
    }
    TRUE
  }, logical(1))

  capture[keep]
}
