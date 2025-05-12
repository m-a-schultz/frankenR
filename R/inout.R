
#' Extract symbols used in a call, separated by LHS and RHS
#'
#' Returns a list of symbols and their line numbers for both the left-hand side
#' (LHS) and right-hand side (RHS) of an expression. Works for assignment calls.
#'
#' @param expr An R call object (e.g., as captured from parse()).
#' @param parse_data Optional result from getParseData() on the full expression source.
#' @return A list with lhs and rhs components, each containing symbols and line numbers.
#' @export
get_symbols <- function(expr, parse_data = NULL) {
  stopifnot(is.call(expr))

  extract_symbols <- function(e) {
    if (is.symbol(e)) return(as.character(e))
    if (is.call(e) || is.expression(e)) {
      return(unlist(lapply(as.list(e), extract_symbols)))
    }
    character(0)
  }

  if (is.null(parse_data)) {
    exprs <- parse(text = deparse(expr),keep.source = TRUE)
    parse_data <- utils::getParseData(exprs)
  }

  symbol_lines <- function(symbols) {
    vapply(symbols, function(sym) {
      rows <- parse_data[parse_data$token == "SYMBOL" & parse_data$text == sym, , drop = FALSE]
      if (nrow(rows)) min(rows$line1) else NA_integer_
    }, integer(1))
  }

  if (as.character(expr[[1]]) %in% c("<-", "=")) {
    lhs <- extract_symbols(expr[[2]])
    rhs <- extract_symbols(expr[[3]])
    list(
      lhs = list(symbols = lhs, first_lines = symbol_lines(lhs)),
      rhs = list(symbols = rhs, first_lines = symbol_lines(rhs))
    )
  } else {
    syms <- extract_symbols(expr)
    list(
      lhs = list(symbols = character(0), first_lines = integer(0)),
      rhs = list(symbols = syms, first_lines = symbol_lines(syms))
    )
  }
}



#' Identify inputs used before they are assigned
#'
#' Examines a code_capture object and returns inputs that are used before being defined.
#'
#' @param capture A code_capture object.
#' @return A named list mapping each input symbol to a list with its first usage index
#'         and whether it was used before definition.
#' @export
get_inputs <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))

  seen_lhs <- character()
  first_def <- list()
  result <- list()

  for (i in seq_along(capture)) {
    expr <- capture[[i]]$expr
    #expr <- capture$expressions[[i]]
    symbols <- get_symbols(expr)

    for (j in seq_along(symbols$lhs$symbols)) {
      sym <- symbols$lhs$symbols[[j]]
      if (!sym %in% seen_lhs) {
        seen_lhs <- c(seen_lhs, sym)
        first_def[[sym]] <- i
      }
    }

    for (j in seq_along(symbols$rhs$symbols)) {
      sym <- symbols$rhs$symbols[[j]]
      if (!sym %in% names(result)) {
        result[[sym]] <- list(first_used = i, used_before_definition = !(sym %in% seen_lhs))
      }
    }
  }

  result
}

