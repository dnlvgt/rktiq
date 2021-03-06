% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{find_mode}
\alias{find_mode}
\title{Modus}
\usage{
find_mode(x, binwidth = 1)
}
\arguments{
\item{x}{Numerischer Vektor, dessen Modus bestimmt wird.}

\item{binwidth}{Numerischer Wert mit der bei der Histogrammberechnung
verwendeten Bin-Breite (Default: 1).}
}
\value{
Numerischer Wert mit dem Modus von \code{x}.
}
\description{
Bestimmt fuer einen Vektor (naeherungsweise) den am haeufigsten auftretenden
Wert (Modus). Dafuer wird mit der einfache Peak der Haeufigkeitsverteilung
herangezogen, die mit einem Histogramm beschrieben wird.
}
\keyword{internal}
