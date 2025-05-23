% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{[<-.code_capture}
\alias{[<-.code_capture}
\title{Replace expressions inside a code_capture}
\usage{
\method{[}{code_capture}(x, i) <- value
}
\arguments{
\item{x}{A `code_capture` object.}

\item{i}{Index to replace.}

\item{value}{New value(s) (must be a call or list of calls).}
}
\value{
The modified `code_capture` object.
}
\description{
Provides assignment (`[<-`) for replacing elements of a `code_capture` object.
}
