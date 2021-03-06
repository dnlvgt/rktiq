% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event.R
\name{include}
\alias{include}
\title{Paarweise beinhaltende Ereignisse?}
\usage{
include(x_start, x_end, y_start, y_end, is_strict = FALSE)
}
\arguments{
\item{x_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.}

\item{x_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.}

\item{y_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.}

\item{y_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.}

\item{is_strict}{Logischer Wert, ob eine minimale zeitliche Differenz
zwischen den Ereignissen vorliegen muss, damit sie als einander beinhaltend
gelten. Anderenfalls gelten Ereignisse mit identischem Start/Ende ebenfalls
als beinhaltend (Default: \emph{FALSE}, d.h. die laxere Definition gilt).}
}
\value{
Logischer Vektor, welche Ereignisse aus \code{x} irgendwelche
  Ereignisse aus \code{y} beinhalten.
}
\description{
Stellt fuer zwei Mengen von Ereignissen \code{x} und \code{y} fest, welche
sich paarweise zeitlich beinhalten (d.h. welche Ereignisse aus \code{y}
liegen komplett innerhalb von Ereignissen aus \code{x}). Die Ereignisse
werden dabei durch ihre Start-/Endzeitpunkte beschrieben.
}
\seealso{
Other Testfunktionen: 
\code{\link{equal}()},
\code{\link{nearby}()},
\code{\link{overlap}()}
}
\concept{Testfunktionen}
\keyword{internal}
