% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_multiple.R
\name{execute_multiple}
\alias{execute_multiple}
\title{execute analysis on multiple images, and return their results}
\usage{
execute_multiple(files, input, DATA, DEBUGKEYS, FLAGS)
}
\arguments{
\item{files}{files to analyse}

\item{input}{respective shiny-component}

\item{DATA}{respective shiny-component}

\item{DEBUGKEYS}{respective shiny-component}

\item{FLAGS}{respective shiny-component}
}
\value{
results_object, see \code{\link[=update_resultsObject]{update_resultsObject()}}
}
\description{
For single-image analysis, see \code{\link[=execute_single]{execute_single()}}
}
\keyword{internal}
