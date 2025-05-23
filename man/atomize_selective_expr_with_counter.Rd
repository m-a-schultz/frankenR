% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atomize.R
\name{atomize_selective_expr_with_counter}
\alias{atomize_selective_expr_with_counter}
\title{Selectively atomize an expression with an external counter and depth control}
\usage{
atomize_selective_expr_with_counter(
  expr,
  fn_names,
  prefix = "tmp",
  counter = 1,
  depth = Inf
)
}
\arguments{
\item{expr}{A call object.}

\item{fn_names}{Character vector of functions to atomize.}

\item{prefix}{Prefix for temporary variable names.}

\item{counter}{Initial counter value (will be updated).}

\item{depth}{Maximum recursion depth for selective atomization (default: Inf).}
}
\value{
A list with `expressions` (list of calls) and updated `counter`.
}
\description{
Like `atomize_selective_expr()`, but accepts and updates an external counter and limits depth.
}
