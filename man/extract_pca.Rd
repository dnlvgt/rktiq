% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{prepare_pca}
\alias{prepare_pca}
\alias{extract_pca}
\alias{pca}
\title{PCA-Merkmale (pca)}
\usage{
prepare_pca(
  x,
  window_length_in_sec,
  is_jittered = TRUE,
  rotation = NULL,
  .seed = 0,
  ...
)

extract_pca(x, rotation, nr_components = 3, ...)
}
\arguments{
\item{x}{Dataframe, der das zu verarbeitetende Signal enthaelt und mindestens
eine Spalte \emph{value} besitzt.}

\item{window_length_in_sec}{Numerischer Wert mit der einheitlichen Laenge der
Signalfenster.}

\item{is_jittered}{Logischer Wert, ob Signalwerte minimal verrauscht werden
(Default: \emph{TRUE}).}

\item{rotation}{\code{prcomp}-Objekt (Liste) mit den PCA-Rotationen, die ggf.
vorberechnet wurden und weitergereicht werden sollen (Default: \emph{NULL},
d.h. Rotationen werden neu berechnet).}

\item{.seed}{Numerischer Wert mit Seed-Wert, mit dem der Zufallsgenerator
fuer das Verrauschen initialisiert wird (Default: \emph{0}).}

\item{...}{Weitere Argumente, die durchgereicht werden koennen.}

\item{nr_components}{Numerischer Wert mit der Anzahl der zu verwendenen
PCA-Komponenten, d.h. der resultierenden Dimensionen (Default: 3).}
}
\value{
Liste mit vorbereiteten Argumenten.

Dataframe mit berechneten Merkmalen.
}
\description{
Berechnet fuer ein Signal eine niedrigdimensionale Projektion basierend auf
den Eigenfunktionen der Hauptkomponentenanalyse (PCA).
}
\details{
Fuer jede Komponente \emph{i} werden die zugehoerigen Merkmale \emph{pc_i}
berechnet, z.B. \emph{pc_1}, \emph{pc_2} und \emph{pc_3} fuer
\code{nr_components == 3}.

Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
beachten, dass sie eine einheitliche Laenge und eine regulaere Abtastung
besitzen sollten, da sonst keine sinnvollen Eigenfunktionen berechnet werden.
}
\section{Functions}{
\itemize{
\item \code{prepare_pca}: Bereitet PCA-Merkmale vor.
}}

\seealso{
Other Merkmalsfunktionen: 
\code{\link{extract_fft_peak}()},
\code{\link{extract_fit_linear}()},
\code{\link{extract_form_bin}()},
\code{\link{extract_raw}()},
\code{\link{extract_stat}()},
\code{\link{prepare_hist_equi}()}
}
\concept{Merkmalsfunktionen}
