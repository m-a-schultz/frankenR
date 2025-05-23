% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object.R
\name{update_capture}
\alias{update_capture}
\title{Format a set of expressions into a code_capture object}
\usage{
update_capture(capture, expr = NA, meta = NA, capture_type = NA)
}
\arguments{
\item{capture}{A `code_capture` object to be modified}

\item{expr}{(optional) A call or list of calls.}

\item{meta}{(optional) A list of metadata (default empty list).}

\item{capture_type}{(optional) A string describing the capture type ("block", "script", etc.).}
}
\value{
A `code_capture` object.
}
\description{
Modifies parts of a capture object
}
\keyword{internal}
