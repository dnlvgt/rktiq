% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{sample_random}
\alias{sample_random}
\alias{sample_random_seq}
\title{Zufaellige Ereignisse}
\usage{
sample_random(
  n,
  int_start,
  int_end,
  offtime = NULL,
  event_length_in_sec,
  event_overlap_in_sec = 0,
  .seed = NULL,
  .max_run = 1000
)

sample_random_seq(
  n,
  int_start,
  int_end,
  offtime = NULL,
  event_length_in_sec,
  event_overlap_in_sec = 0,
  from_start_to_end = TRUE,
  .seed = NULL
)
}
\arguments{
\item{n}{Numerischer Wert mit Anzahl der zu samplenden Ereignisse}

\item{int_start}{POSIXct-Zeitstempel mit Startzeitpunkt des Sample-Zeitraums.}

\item{int_end}{POSIXct-Zeitstempel mit Endzeitpunkt des Sample-Zeitraums.}

\item{offtime}{Dataframe, der die Offtime-Ereignisse beinhaltet. Die
Ereignisse sind anhand ihrer Start-/Endzeitpunkte beschrieben, die jeweils
in den Spalten \emph{start} und \emph{end} uebergeben werden.}

\item{event_length_in_sec}{Numerischer Wert mit der Laenge der Ereignisse (in
Sekunden).}

\item{event_overlap_in_sec}{Numerischer Wert mit der Laenge des Intervalls
(in Sekunden), in dem sich zwei aufeinanderfolgende Ereignisse ueberlappen
duerfen (Default: 0, d.h. keine Ueberlappung erlaubt).}

\item{.seed}{Numerischer Wert mit Seed-Wert, mit dem der Zufallsgenerator
initialisiert wird. Darf auch \emph{NULL} sein, dann wird zur
Initialisierung die aktuelle Systemzeit herangezogen (Default: \emph{NULL},
d.h. bei jedem Aufruf sollten unterschiedliche Ergebnisse erzeugt werden).}

\item{.max_run}{Numerischer Wert mit Anzahl der Versuche, die je Ereignis
unternommen werden sollen, um es im Zeitraum zu platzieren (Default: 1E3).}

\item{from_start_to_end}{Logischer Wert, ob Ereignisse von \code{int_start}
nach \code{int_end} erzeugt werden oder andersrum (Default: \emph{TRUE},
d.h. es wird bei \code{int_start} begonnen).}
}
\value{
Dataframe mit zufaelligen Ereignissen aus Zeitraum.
}
\description{
Bestimmt in einem Zeitraum zufaellig eine feste Anzahl an Ereignissen
(Sampling). Dabei wird wiederholt ein Ereignis an zufaelliger Position
innerhalb des Zeitraums platziert (\code{sample_random}) bzw. sequentiell die
Menge aller im Zeitraum moeglichen Ereignisse gebildet und anschliessend
daraus eine zufaellige Stichprobe gezogen (\code{sample_random_seq}).
}
\details{
Der Gesamtzeitraum wird durch einen Start- und einen Endzeitpunkt festgelegt.
In diesem Zeitraum koennen zusaetzliche Offtime-Intervalle definiert werden,
in denen kein Sampling stattfindet (d.h. erzeugte Ereignisse ueberlappen sich
nicht mit Offtimes). Neben der Ereignislaenge kann auch eine potentielle
zeitliche Ueberlappung zwischen Ereignissen gesteuert werden.
}
\seealso{
Other Sampling-Funktionen: 
\code{\link{random_event}()},
\code{\link{sample_balanced}()}
}
\concept{Sampling-Funktionen}
