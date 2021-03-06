% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/window.R
\name{window_stretch}
\alias{window_stretch}
\title{Fensterstreckung}
\usage{
window_stretch(x, max_in_sec = 60, .progress = FALSE)
}
\arguments{
\item{x}{Liste mit Fenster-Tiqqles, die zeitlich gestreckt werden.}

\item{max_in_sec}{Numerischer Wert, auf welche Laenge (in Sekunden) die
Fenster gestreckt werden (Default: 60, d.h. alle Fenster werden auf 60
Sekunden gestreckt.}

\item{.progress}{Logischer Wert, ob Fortschrittsbalken angezeigt wird.}
}
\value{
Liste mit zeitlich gestreckten Fenster-Tiqqles.
}
\description{
Eine Menge von Fenster-Dataframes wird auf eine einheitliche Laenge gebracht.
Dabei wird der jeweils groesste Zeitstempel so normiert, dass er dem
numerischen Wert \code{max_in_sec} entspricht. Die restlichen Zeitstempel
werden entsprechend angepasst, d.h. sie bekommen einen Zeitstempel zwischen 0
und \code{max_in_sec}. Abschliessend werden die numerischen Zeitinformationen
wieder in POSIXct-Zeitstempel umgewandelt.
}
\details{
Effektiv kann bei bei diesem Verfahren also sowohl eine Streckung als auch
eine Stauchung stattfinden. Signalinformationen gehen dabei nicht verloren.
}
\section{Future}{
 Da dieser Verarbeitungsschritt u.U. etwas laenger dauern
  kann, kann hierfuer mittels \code{future::plan} eine parallele Verarbeitung
  zugeschaltet werden. Durch das Argument \code{.progress} kann ggf. ein
  Forschrittsbalken angezeigt werden.
}

\seealso{
Other Fensterfunktionen: 
\code{\link{window_align}()},
\code{\link{window_crop}()},
\code{\link{window_merge}()},
\code{\link{window_truncate}()}
}
\concept{Fensterfunktionen}
