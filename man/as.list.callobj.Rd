% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{as.list.callobj}
\alias{as.list.callobj}
\title{Convert a call object to a list}
\usage{
\method{as.list}{callobj}(x, ...)
}
\arguments{
\item{x}{A call object.}

\item{...}{Unsupported}
}
\value{
A list where the first element is the function being called in the expression,
  followed by its arguments.
}
\description{
This method defines `as.list()` for objects of class `"callobj"`, allowing
function call expressions to be treated as lists of their components.
It temporarily removes the `"call"` class before conversion to avoid
dispatch issues and ensure correct coercion.
}
