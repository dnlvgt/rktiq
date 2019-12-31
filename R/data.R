#' Signal-Beispieldaten
#'
#' Datensatz mit Sensordaten.
#'
#' Die Sensordaten umfassen zwei Signale \emph{A} und \emph{B} in einem Zeitraum
#' von zwei Wochen (\emph{21.08.19 00:00} - \emph{03.09.19 24:00}).
#'
#' @format Tiqqle im langen Format mit 25.343 Zeilen und 3 Spalten:
#' \describe{
#'   \item{time}{POSIXct-Zeitstempel mit dem Zeitpunkt der Signalwertaenderung.}
#'   \item{signal}{Faktor mit der Signal-ID (umfasst 2 Level).}
#'   \item{value}{Numerischer Wert mit den Signalwerten.}
#' }
"signal_sensor"
