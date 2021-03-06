% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_fft_peak}
\alias{extract_fft_peak}
\alias{fft_peak}
\title{FFT-Merkmale (fft_peak)}
\usage{
extract_fft_peak(x, f_in_hz = 1, is_smoothed = TRUE, nr_peaks = 5, ...)
}
\arguments{
\item{x}{Dataframe, der das zu verarbeitetende Signal enthaelt und mindestens
eine Spalte \emph{value} besitzt.}

\item{f_in_hz}{Numerischer Wert mit der Samplingfrequenz des Signals in Hertz
(Default: 1).}

\item{is_smoothed}{Logischer Wert, ob Amplituden des Frequenzspektrums
geglaettet werden (Default: \emph{TRUE}).}

\item{nr_peaks}{Numerischer Wert mit der Anzahl der zu bestimmenden Spitzen
im Frequenzspektrum (Default: 5).}

\item{...}{Weitere Argumente, die durchgereicht werden koennen.}
}
\value{
Dataframe mit berechneten Merkmalen.
}
\description{
Berechnet fuer ein Signal das mittlere Frequenzspektrum mittels
Fast-Fourier-Transformation und bestimmt die am staerksten auftretenden
Frequenzspitzen. Bei Bedarf kann das Frequenzspektrum zuvor noch geglaettet
werden.
}
\details{
Fuer jede gefundene Spitze \emph{i} werden die zugehoerigen Merkmale
\emph{peak\strong{i}_freq} (Frequenz in Hz) und \emph{peak\strong{i}_amp}
(relative Amplitude) berechnet, z.B. \emph{peak1_amp}, \emph{peak1_freq},
\emph{peak2_amp} and \emph{peak2_freq} fuer \code{nr_peaks = 2}.

Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
beachten, dass sie eine regulaere Abtastung besitzen sollten, da sonst kein
sinnvolles Frequenzspektrum berechnet wird.
}
\seealso{
Other Merkmalsfunktionen: 
\code{\link{extract_fit_linear}()},
\code{\link{extract_form_bin}()},
\code{\link{extract_raw}()},
\code{\link{extract_stat}()},
\code{\link{prepare_hist_equi}()},
\code{\link{prepare_pca}()}
}
\concept{Merkmalsfunktionen}
