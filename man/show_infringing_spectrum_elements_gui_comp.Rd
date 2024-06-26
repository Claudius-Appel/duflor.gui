% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_infringing_spectrum_elements_gui_comp.R
\name{show_infringing_spectrum_elements_gui_comp}
\alias{show_infringing_spectrum_elements_gui_comp}
\title{generate and call a modal-dialogue which displays the prospective changes in HSV-spectrums}
\usage{
show_infringing_spectrum_elements_gui_comp(input, DATA, changes)
}
\arguments{
\item{input}{respective shiny-component}

\item{DATA}{respective shiny-component}

\item{changes}{results-object from \code{\link[=validate_custom_HSV_values]{validate_custom_HSV_values()}}}
}
\value{
.
}
\description{
Additionally, it queries whether or not they should be applied, or reset to previous value.
}
\keyword{internal}
