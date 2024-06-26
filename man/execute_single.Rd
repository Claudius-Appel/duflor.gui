% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_single.R
\name{execute_single}
\alias{execute_single}
\title{execute analysis on a single image, and return its results}
\usage{
execute_single(file, input, DATA, DEBUGKEYS, FLAGS)
}
\arguments{
\item{file}{file to analyse}

\item{input}{respective shiny-component}

\item{DATA}{respective shiny-component}

\item{DEBUGKEYS}{respective shiny-component}

\item{FLAGS}{respective shiny-component}
}
\value{
results_object, see \code{\link[=update_resultsObject]{update_resultsObject()}}
}
\description{
For single-image analysis, see \code{\link[=execute_multiple]{execute_multiple()}}
}
\keyword{internal}
