% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{get_expr_meta}
\alias{get_expr_meta}
\title{Get expression and metadata pair at a given index}
\usage{
get_expr_meta(capture, i)
}
\arguments{
\item{capture}{A `code_capture` object.}

\item{i}{Integer index to retrieve.}
}
\value{
A list containing `expr` and `meta`.
}
\description{
Retrieves the combined expression and metadata list from a `code_capture` object.
}
