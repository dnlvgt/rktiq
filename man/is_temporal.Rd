% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{is_temporal}
\alias{is_temporal}
\title{Vektor mit Zeitinformationen?}
\usage{
is_temporal(x, is_strict = FALSE)
}
\arguments{
\item{x}{Vektor, der ueberprueft wird.}

\item{is_strict}{Logischer Wert, ob streng ausschliesslich auf
\code{is.POSIXct} getestet wird (Default: \emph{FALSE}).}
}
\value{
Logischer Wert, ob \code{x} ein Vektor mit Zeitinformationen ist.
}
\description{
Ueberprueft, ob in einem Vektor Zeitinformationen vorliegen.
}
\keyword{internal}
