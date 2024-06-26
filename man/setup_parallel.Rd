% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_parallel.R
\name{setup_parallel}
\alias{setup_parallel}
\title{setup parallel backend for a given number of cores.}
\usage{
setup_parallel(used_cores, cluster_type)
}
\arguments{
\item{used_cores}{number of cores to be designated to the cluster}

\item{cluster_type}{either \code{PSOCK} or \code{FORK}. Generally \code{FORK} is recommended above \code{PSOCK}, however it is not available on all systems. See note below}
}
\value{
.
}
\description{
setup parallel backend for a given number of cores.
}
\note{
On Windows, only the \code{PSOCK}-cluster type is available.
}
\keyword{internal}
