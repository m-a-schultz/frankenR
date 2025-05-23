% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{simplify_capture}
\alias{simplify_capture}
\title{Simplify a capture by removing unused constant assignments}
\usage{
simplify_capture(capture)
}
\arguments{
\item{capture}{A `code_capture` object.}
}
\value{
A simplified `code_capture` object.
}
\description{
Removes constant assignments to variables that are never used later.
}
