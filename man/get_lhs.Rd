% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{get_lhs}
\alias{get_lhs}
\title{Get the left-hand side of an assignment}
\usage{
get_lhs(expr)
}
\arguments{
\item{expr}{A call object.}
}
\value{
A character vector of the LHS variable name.
}
\description{
Extracts the variable being assigned to in an assignment call.
}
