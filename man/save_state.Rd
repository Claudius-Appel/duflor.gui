% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_state.R
\name{save_state}
\alias{save_state}
\title{Handle saving application-state to file}
\usage{
save_state(input, DATA, DEBUGKEYS, FLAGS, volumes)
}
\arguments{
\item{input}{respective shiny-component}

\item{DATA}{respective shiny-component}

\item{DEBUGKEYS}{respective shiny-component}

\item{FLAGS}{respective shiny-component}

\item{volumes}{volumes present on the machine}
}
\value{
path to the saved state-file
}
\description{
\itemize{
\item assumes \code{input$save_state} to be properly formatted, as returned by \code{\link[shinyFiles:shinyFiles-observers]{shinyFiles::shinyFileSave()}}
\item assumes \code{volumes} to be the return-value of \code{\link[shinyFiles:getVolumes]{shinyFiles::getVolumes()}}
}
}
\keyword{internal}
