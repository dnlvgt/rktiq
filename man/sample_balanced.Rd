% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{sample_balanced}
\alias{sample_balanced}
\alias{sample_balanced_seq}
\title{Zufaellige (klassenbalancierte) Ereignisse}
\usage{
sample_balanced(
  n,
  int_start,
  int_end,
  offtime = NULL,
  target_event,
  target_cut_in_sec,
  event_length_in_sec,
  event_overlap_in_sec = 0,
  include_tail = FALSE,
  .seed = NULL,
  .max_run = 1000
)

sample_balanced_seq(
  n,
  int_start,
  int_end,
  offtime = NULL,
  target_event,
  target_cut_in_sec,
  event_length_in_sec,
  event_overlap_in_sec = 0,
  from_start_to_end = FALSE,
  from_target = TRUE,
  include_tail = FALSE,
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

\item{target_event}{Dataframe, der die Zielereignisse beinhaltet. Die
Ereignisse sind anhand ihrer Start-/Endzeitpunkte beschrieben, die jeweils
in den Spalten \emph{start} und \emph{end} uebergeben werden.}

\item{target_cut_in_sec}{Numerischer Wert mit der Laenge des Zeitfensters (in
Sekunden), das vor jedem Zielereignis liegt und den Bereich der potentiell
positiven Ereignisse umfasst.}

\item{event_length_in_sec}{Numerischer Wert mit der Laenge der Ereignisse (in
Sekunden).}

\item{event_overlap_in_sec}{Numerischer Wert mit der Laenge des Intervalls
(in Sekunden), in dem sich zwei aufeinanderfolgende Ereignisse ueberlappen
duerfen (Default: 0, d.h. keine Ueberlappung erlaubt).}

\item{include_tail}{Logischer Wert, ob Ereignisse nach dem letzten
Zielereignis platziert werden koennen (Default: \emph{FALSE}).}

\item{.seed}{Numerischer Wert mit Seed-Wert, mit dem der Zufallsgenerator
initialisiert wird. Darf auch \emph{NULL} sein, dann wird zur
Initialisierung die aktuelle Systemzeit herangezogen (Default: \emph{NULL},
d.h. bei jedem Aufruf sollten unterschiedliche Ergebnisse erzeugt werden).}

\item{.max_run}{Numerischer Wert mit Anzahl der Versuche, die je Ereignis
unternommen werden sollen, um es im Zeitraum zu platzieren (Default: 1E3).}

\item{from_start_to_end}{Logischer Wert, ob Ereignisse von \code{int_start}
nach \code{int_end} erzeugt werden oder andersrum (Default: \emph{FALSE},
d.h. es wird bei \code{int_end} begonnen).}

\item{from_target}{Logischer Wert, ob Ereignisse von den Zielereignissen
weglaufend erzeugt werden oder zu ihnen hinlaufend (Default: \emph{TRUE},
d.h. sie laufen von den Zielereignissen weg). Die Richtung ist abhaengig
von \code{from_start_to_event}).}
}
\value{
Benannte Liste mit Dataframes mit zufaelligen, klassenbalancierten
  Ereignissen aus Zeitraum.
}
\description{
Bestimmt in einem Zeitraum zufaellig eine feste Anzahl an Ereignissen
(Sampling). Diese Ereignisse beziehen sich auf eine Menge an Zielereignissen
(z.B. Stoerungen) und werden dabei in positive und negative Ereignisse
unterschieden. Zur Platzierung der Ereignisse wird auf die Funktion
\code{sample_random} zurueckgegriffen (\code{sample_balanced}) bzw. wird
sequentiell die Menge aller im Zeitraum moeglichen Ereignisse gebildet und
anschliessend daraus eine zufaellige Stichprobe gezogen
(\code{sample_balanced_seq}).
}
\details{
Waehrend positive Ereignisse nahe vor einem Zielereignis liegen, befinden
sich negative Ereignisse ausserhalb dieser Bereiche (i.d.R. also weit vor
einem bzw. nach einem Zielereignis). Die Ereignisse werden so von der
Funktion platziert, dass das Verhaeltnis von positiven und negativen
Ereignissen gleichverteilt ist und somit eine Klassenbalance hergestellt
wird.

Der Gesamtzeitraum wird durch einen Start- und einen Endzeitpunkt festgelegt.
In diesem Zeitraum koennen zusaetzliche Offtime-Intervalle definiert werden,
in denen kein Sampling stattfindet (d.h. erzeugte Ereignisse ueberlappen sich
nicht mit Offtimes). Neben der Ereignislaenge kann auch eine potentielle
zeitliche Ueberlappung zwischen Ereignissen gesteuert werden. Um
Klassenbalance herzustellen, wird weiterhin die Menge der Zielereignisse und
die Laenge des Zeitfensters vor den Zielereignissen benoetigt. Um zusaetzlich
bei \code{sample_random_seq} die zeitliche Ausrichtung der Ereignisse zu
beeinflussen, kann deren Erzeugungsrichtung sowie ihre Orientierung
bezueglich der Zielereignisse angepasst werden.
}
\seealso{
\code{\link{sample_random}}

Other Sampling-Funktionen: 
\code{\link{random_event}()},
\code{\link{sample_random}()}
}
\concept{Sampling-Funktionen}
