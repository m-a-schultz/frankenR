% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object.R
\name{format_capture}
\alias{format_capture}
\title{Format a set of expressions into a code_capture object}
\usage{
format_capture(expr, meta = list(), capture_type = "unknown")
}
\arguments{
\item{expr}{A call or list of calls.}

\item{meta}{A list of metadata (default empty list).}

\item{capture_type}{A string describing the capture type ("block", "script", etc.).}
}
\value{
A `code_capture` object.
}
\description{
Internal function to wrap expressions and optional metadata into a capture object.
}
\keyword{internal}
