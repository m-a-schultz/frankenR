#' Check if an object is a call or a list of calls
#'
#' Determines whether the input is a single function call or a list entirely composed of calls.
#'
#' @param x An object to check.
#' @return Logical TRUE/FALSE.
is_call_or_list <- function(x) {
  is.call(x) || (is.list(x) && all(vapply(x, is.call, logical(1))))
}

get_expr <- function(x) {
  if (inherits(x, "callobj")) x$expr else x
}

# Creates an output object that matches the type of the input object
match_input <- function(input, output_expr) {
  if (inherits(input, "callobj")) {
    new_callobj(output_expr, input$meta)
  } else {
    output_expr
  }
}
# ---- Accessors ----

#' Get the function name from a call
#'
#' Extracts the function name (symbol) from a call expression.
#'
#' @param x An expression or  call object.
#' @return The function name as a symbol, or NULL if not a call.
#' @export
get_function_name <- function(x) {
  if(inherits(x, 'callobj')){
    expr <- x$expr
  }else{
    expr <- x
  }
  if (is.call(expr)) expr[[1]] else NULL
}

#' Get top-level function names from expressions
#'
#' Extracts the function name from each expression in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @return A character vector of function names (or NA for non-calls).
#' @export
get_top_function_names <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  vapply(get_expressions(capture), function(e) {
    if (is.call(e)) as.character(e[[1]]) else NA_character_
  }, character(1))
}

#' Get the arguments from a call
#'
#' Returns a list of arguments from a call expression.
#'
#' @param expr A call object.
#' @return A list of arguments, or NULL if not a call.
#' @export
get_arguments <- function(expr) {
  if (is.call(expr)) as.list(expr[-1]) else NULL
}

#' Get the deparsed text of expressions in a capture
#'
#' Converts all expressions in a `code_capture` object to character strings.
#'
#' @param capture A `code_capture` object.
#' @param collapse String used to collapse multi-line expressions.
#' @return A character vector with one element per expression.
#' @export
get_expr_text <- function(capture, collapse = "\n") {
  stopifnot(inherits(capture, "code_capture"))
  vapply(get_expressions(capture), function(e) {
    #print(e)
    paste(deparse(e), collapse = collapse)
  }, character(1))
}

#' Get all arguments from all expressions in a capture
#'
#' Extracts arguments for each call in a `code_capture` object.
#'
#' @param capture A `code_capture` object.
#' @return A list of argument lists.
#' @export
get_all_arguments <- function(capture) {
  stopifnot(inherits(capture, "code_capture"))
  lapply(get_expressions(capture), get_arguments)
}


`%||%` <- function(a, b) if (!is.null(a)) a else b


# #' Check if an expression is an assignment
# #'
# #' Detects whether the top-level call is an assignment (`<-` or `=`).
# #'
# #' @param expr A call or list of calls.
# #' @return Logical TRUE/FALSE (or logical vector if input is a list).
# #' @export
# is_assignment <- function(expr) {
#   if (is.list(expr)) {
#     return(vapply(expr, is_assignment, logical(1)))
#   }
#   if (!is.call(expr)) return(FALSE)
#
#   fun <- as.character(expr[[1]])
#   fun %in% c("<-", "=")
# }

# ---- General Pattern Matching ----

#' Identify which expressions match a function + argument pattern
#'
#' Checks a list of expressions and returns a logical vector indicating which
#' expressions match any function name in `fn_map`, optionally restricted to specific argument names.
#'
#' @param exprs A list of expressions (calls or callobj objects).
#' @param fn_map A named list where names are function names and values are character vectors
#'        of argument names (empty vector = match any call to the function).
#'
#' @return A logical vector, TRUE for matches, FALSE otherwise.
#' @export
which_expr_matches <- function(exprs, fn_map) {
  vapply(exprs, function(e) {
    e <- get_expr(e)
    if (!is.call(e)) return(FALSE)

    fn <- as.character(e[[1]])
    if (!fn %in% names(fn_map)) return(FALSE)

    relevant_args <- fn_map[[fn]]
    if (length(relevant_args) == 0) {
      return(TRUE)
    }

    arg_names <- names(as.list(e)[-1])
    any(arg_names %in% relevant_args)
  }, logical(1))
}

