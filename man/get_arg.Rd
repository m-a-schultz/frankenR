% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{get_arg}
\alias{get_arg}
\title{Get an argument from a call}
\usage{
get_arg(expr, name)
}
\arguments{
\item{expr}{A expression or callobj.}

\item{name}{The argument name to retrieve.}
}
\value{
The argument value, or NULL if not found.
}
\description{
Retrieves the value of a named argument in a function call.
}
