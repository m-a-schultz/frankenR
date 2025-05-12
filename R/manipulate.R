# ---- Expression Manipulation Utilities ----

#' Get the left-hand side of an assignment
#'
#' Extracts the variable being assigned to in an assignment call.
#'
#' @param expr A call object.
#' @return A character vector of the LHS variable name.
#' @export
get_lhs <- function(expr) {
  stopifnot(is.call(expr), is_assignment(expr))
  as.character(expr[[2]])
}

#' Get the right-hand side of an assignment
#'
#' Extracts the value or expression assigned in an assignment call.
#'
#' @param expr A call object.
#' @return The RHS expression.
#' @export
get_rhs <- function(expr) {
  stopifnot(is.call(expr), is_assignment(expr))
  expr[[3]]
}

#' Set the left-hand side of an assignment
#'
#' Replaces the variable name on the LHS of an assignment call.
#'
#' @param expr A call or callobj.
#' @param lhs A symbol or character name for the new LHS variable.
#' @return The modified call or callobj (same type as input).
#' @export
set_lhs <- function(expr, lhs) {
  e <- get_expr(expr)
  stopifnot(is.call(e), is_assignment(e))
  e[[2]] <- if (is.symbol(lhs)) lhs else as.name(lhs)
  match_input(expr, e)
}

#' Set the right-hand side of an assignment
#'
#' Replaces the RHS value or expression in an assignment call.
#'
#' @param expr A call or callobj.
#' @param rhs An expression for the new RHS.
#' @return The modified call or callobj (same type as input).
#' @export
set_rhs <- function(expr, rhs) {
  e <- get_expr(expr)
  stopifnot(is.call(e), is_assignment(e))
  e[[3]] <- rhs
  match_input(expr, e)
}

#' Get the function called in an expression
#'
#' Extracts the function from a call expression.
#'
#' @param expr A call object.
#' @return The function name or call (symbol or call).
#' @export
get_function <- function(expr) {
  stopifnot(is.call(expr))
  expr[[1]]
}

#' Set the function of a call expression
#'
#' Replaces the function being called.
#'
#' @param expr A call or callobj.
#' @param fn A symbol, call, or character to set as the new function.
#' @return The modified call or callobj (same type as input).
#' @export
set_function <- function(expr, fn) {
  e <- get_expr(expr)
  stopifnot(is.call(e))
  if (is.character(fn)) fn <- as.name(fn)
  e[[1]] <- fn
  match_input(expr, e)
}


#' Get the arguments of a call
#'
#' Returns all arguments of a call, excluding the function.
#'
#' @param expr A call object.
#' @return A list of argument expressions.
#' @export
get_arguments <- function(expr) {
  stopifnot(is.call(expr))
  as.list(expr)[-1]
}

#' Get the names of arguments in a call
#'
#' Returns the argument names (or empty strings for unnamed arguments).
#'
#' @param expr A call object.
#' @return A character vector of argument names.
#' @export
get_argument_names <- function(expr) {
  stopifnot(is.call(expr))
  arg_list <- as.list(expr)[-1]
  nms <- names(arg_list)
  if (is.null(nms)) nms <- rep("", length(arg_list))
  nms
}

# ---- Predicates ----

#' Check if an expression is a function call
#'
#' @param expr An expression.
#' @return TRUE if expr is a call, FALSE otherwise.
#' @export
is_function <- function(expr) {
  is.call(expr)
}

#' Check if an expression is a compound (nested) call
#'
#' Returns TRUE if any arguments of the call are themselves calls.
#'
#' @param expr A call or callobj.
#' @return Logical TRUE/FALSE.
#' @export
is_compound <- function(expr) {
  e <- get_expr(expr)
  if (!is.call(e)) return(FALSE)
  any(vapply(as.list(e)[-1], is.call, logical(1)))
}


#' Check if an expression is an assignment
#'
#' Determines whether an expression is an assignment call (`<-`, `=`, or `<<-`).
#'
#' @param expr A call or callobj.
#' @return TRUE if expression is assignment, FALSE otherwise.
#' @export
is_assignment <- function(expr) {
  e <- get_expr(expr)
  is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-")
}



#' Check if a call has an operator as function
#'
#' @param expr A call or callobj.
#' @return TRUE if the function name is a base operator.
#' @export
has_operator <- function(expr) {
  e <- get_expr(expr)
  if (!is.call(e)) return(FALSE)
  fname <- as.character(e[[1]])
  fname %in% c("+", "-", "*", "/", "%", "^", "<", ">", "=", "!", "&", "|", "$",
               "[", "[[", "[<-", "[[<-")
}

#' Get the operator of a call
#'
#' Returns the operator as character, or NULL if not an operator.
#'
#' @param expr A call or callobj.
#' @return Character name of operator, or NULL.
#' @export
get_operator <- function(expr) {
  e <- get_expr(expr)
  if (!is.call(e)) return(NULL)
  fname <- as.character(e[[1]])
  if (fname %in% c("+", "-", "*", "/", "%", "^", "<", ">", "=", "!", "&", "|", "$",
                   "[", "[[", "[<-", "[[<-")) {
    return(fname)
  }
  NULL
}


# ---- Replacement Utilities ----

#' Replace variable name(s) in an expression
#'
#' Recursively substitutes symbol names based on a mapping.
#'
#' @param expr A call, callobj, or expression.
#' @param mapping A named list or named character vector (old names = names, new names = values).
#' @return The modified expression (same type as input).
#' @export
replace_variable <- function(expr, mapping) {
  e <- get_expr(expr)
  if (!is.call(e) && !is.symbol(e)) return(match_input(expr, e))

  if (is.symbol(e)) {
    name <- as.character(e)
    if (name %in% names(mapping)) return(match_input(expr, as.name(mapping[[name]])))
    return(expr)
  }

  # Recurse into calls
  args <- as.list(e)
  args <- lapply(args, replace_variable, mapping = mapping)
  e_new <- as.call(args)
  match_input(expr, e_new)
}


#' Replace function in all calls inside an expression
#'
#' Recursively replaces any function matching old_func with new_func.
#'
#' @param expr A call, callobj, or expression.
#' @param old_func Character name of the function to replace.
#' @param new_func Character or symbol of the replacement function.
#' @return The modified expression (same type as input).
#' @export
replace_function <- function(expr, old_func, new_func) {
  e <- get_expr(expr)
  if (!is.call(e)) return(match_input(expr, e))

  # Replace function if it matches
  fname <- e[[1]]
  if (is.symbol(fname) && as.character(fname) == old_func) {
    e[[1]] <- if (is.symbol(new_func)) new_func else as.name(new_func)
  }

  # Recursively check all arguments
  args <- as.list(e)
  args[-1] <- lapply(args[-1], replace_function, old_func = old_func, new_func = new_func)
  e <- as.call(args)

  match_input(expr, e)
}

#' Replace operator in a call expression
#'
#' Recursively replaces any operator matching old_op with new_op.
#'
#' @param expr A call, callobj, or expression.
#' @param old_op Character name of the operator to replace.
#' @param new_op Character or symbol of the new operator.
#' @return The modified expression (same type as input).
#' @export
replace_operator <- function(expr, old_op, new_op) {
  e <- get_expr(expr)
  if (!is.call(e)) return(match_input(expr, e))

  # Replace operator at current level
  fname <- e[[1]]
  if (is.symbol(fname) && as.character(fname) == old_op) {
    e[[1]] <- if (is.symbol(new_op)) new_op else as.name(new_op)
  }

  # Recurse into arguments
  args <- as.list(e)
  args[-1] <- lapply(args[-1], replace_operator, old_op = old_op, new_op = new_op)
  e_new <- as.call(args)
  match_input(expr, e_new)
}



#' Substitute symbols in an expression
#'
#' Recursively walks an expression and replaces any symbols found in `rename_map`.
#'
#' @param expr A call or symbol object (or callobj).
#' @param rename_map A named list or character vector mapping old names to new names.
#' @return The transformed expression (same type as input).
#' @export
substitute_symbols <- function(expr, rename_map) {
  if (is.list(rename_map))
    rename_map <- unlist(rename_map)
  e <- get_expr(expr)
  new_expr <- if (is.call(e)) {
    as.call(lapply(e, substitute_symbols, rename_map = rename_map))
  } else if (is.symbol(e)) {
    name <- as.character(e)
    if (name %in% names(rename_map)) as.name(rename_map[name]) else e
  } else {
    e
  }
  match_input(expr, new_expr)
}

#' Recursively scan an expression tree
#'
#' Traverses an expression tree and collects nodes where the predicate returns TRUE.
#'
#' @param expr A call, symbol, or expression object.
#' @param predicate A function taking a node and returning TRUE/FALSE.
#' @return A list of matching sub-expressions.
#' @export
scan_expr_tree <- function(expr, predicate) {
  matches <- list()
  recurse <- function(e) {
    if (predicate(e)) matches[[length(matches) + 1]] <<- e
    if (is.call(e) || is.pairlist(e)) lapply(as.list(e), recurse)
  }
  recurse(expr)
  matches
}
