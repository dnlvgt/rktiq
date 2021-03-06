% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{replace_nan}
\alias{replace_nan}
\title{NaN-Ersetzung}
\usage{
replace_nan(x, y = NA)
}
\arguments{
\item{x}{Vektor, in dem ersetzt wird.}

\item{y}{Wert, mit dem \emph{NaN} ersetzt wird (Default: \emph{NA}).}
}
\value{
Ersetzter Vektor \code{x}.
}
\description{
Ersetzt in einem Vektor evtl. vorhandene \emph{NaN}-Werte.
}
\keyword{internal}
