% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event.R
\name{assert_test}
\alias{assert_test}
\title{Argumente-Check (Testfunktionen)}
\usage{
assert_test(
  x_start = 0,
  x_end = 0,
  y_start = 0,
  y_end = 0,
  is_strict = NA,
  threshold_in_sec = 0
)
}
\arguments{
\item{x_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.}

\item{x_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.}

\item{y_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.}

\item{y_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.}

\item{is_strict}{Logischer Wert, ob ein minimaler zeitlicher Abstand zwischen
den Ereignissen vorliegen muss, damit sie als angrenzend gelten.
Anderenfalls gelten Ereignisse ohne Abstand (d.h. \code{x_end == y_start}
bzw \code{y_end == x_start} ebenfalls als angrenzend (Default:
\emph{FALSE}, d.h. die laxere Definition gilt).}

\item{threshold_in_sec}{Numerischer Vektor (Laenge 1 oder 2), wie viele
Sekunden zwei Ereignisse maximal auseinander liegen duerfen, um noch als
angrenzend zu gelten (Toleranzbereich). Bei Laenge 2 wird der erste Wert
als Toleranzbereich vor dem Ereignis und der zweite Wert als
Toleranzbereich nache dem Ereignis betrachtet. Bei Laenge 1 ist der
Toleranzbereich vor und nach dem Ereignis identisch. Sollte Toleranzbereich
\emph{NA} sein, wird er ignoriert.}
}
\description{
Hilfsfunktion zum Ueberpruefen einer Reihe von Argumenten, ob sie mit
korrekten Typen und sinnvollen Werten uebergeben wurden. Sobald ein Verstoss
festgestellt wird, wird die Ausfuehrung unterbrochen. Da diese Test
wiederholt in den Ereignis-Testfunktionen auftreten, wurden sie in diese
Funktion ausgelagert.
}
\seealso{
Other Argument-Funktionen: 
\code{\link{assert_sample}()}
}
\concept{Argument-Funktionen}
\keyword{internal}
