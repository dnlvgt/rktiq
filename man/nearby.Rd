% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event.R
\name{nearby}
\alias{nearby}
\title{Paarweise angrenzende Ereignisse?}
\usage{
nearby(x_start, x_end, y_start, y_end, threshold_in_sec, is_strict = FALSE)
}
\arguments{
\item{x_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.}

\item{x_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.}

\item{y_start}{Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.}

\item{y_end}{Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.}

\item{threshold_in_sec}{Numerischer Vektor (Laenge 1 oder 2), wie viele
Sekunden zwei Ereignisse maximal auseinander liegen duerfen, um noch als
angrenzend zu gelten (Toleranzbereich). Bei Laenge 2 wird der erste Wert
als Toleranzbereich vor dem Ereignis und der zweite Wert als
Toleranzbereich nache dem Ereignis betrachtet. Bei Laenge 1 ist der
Toleranzbereich vor und nach dem Ereignis identisch. Sollte Toleranzbereich
\emph{NA} sein, wird er ignoriert.}

\item{is_strict}{Logischer Wert, ob ein minimaler zeitlicher Abstand zwischen
den Ereignissen vorliegen muss, damit sie als angrenzend gelten.
Anderenfalls gelten Ereignisse ohne Abstand (d.h. \code{x_end == y_start}
bzw \code{y_end == x_start} ebenfalls als angrenzend (Default:
\emph{FALSE}, d.h. die laxere Definition gilt).}
}
\value{
Logischer Vektor, welche Ereignisse aus \code{x} an irgendwelche
  Ereignissen aus \code{y} angrenzen.
}
\description{
Stellt fuer zwei Mengen von Ereignissen fest, welche paarweise zeitlich
aneinander angrenzen (d.h. aufeinander folgen). Die Ereignisse werden dabei
durch ihre Start-/Endzeitpunkte beschrieben.
}
\seealso{
Other Testfunktionen: 
\code{\link{equal}()},
\code{\link{include}()},
\code{\link{overlap}()}
}
\concept{Testfunktionen}
\keyword{internal}
