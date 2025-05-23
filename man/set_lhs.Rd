% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{set_lhs}
\alias{set_lhs}
\title{Set the left-hand side of an assignment}
\usage{
set_lhs(expr, lhs)
}
\arguments{
\item{expr}{A call or callobj.}

\item{lhs}{A symbol or character name for the new LHS variable.}
}
\value{
The modified call or callobj (same type as input).
}
\description{
Replaces the variable name on the LHS of an assignment call.
}
