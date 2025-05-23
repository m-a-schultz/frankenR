% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{is_assignment}
\alias{is_assignment}
\title{Check if an expression is an assignment}
\usage{
is_assignment(expr)
}
\arguments{
\item{expr}{A call or callobj.}
}
\value{
TRUE if expression is assignment, FALSE otherwise.
}
\description{
Determines whether an expression is an assignment call (`<-`, `=`, or `<<-`).
}
