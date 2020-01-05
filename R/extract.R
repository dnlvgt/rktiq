# Funktionen zum Berechnen von Merkmalen

# Vorbereitungsfunktionen ------------------------------------------------------

#' @describeIn extract_hist Bereitet Histogramme mit festen Bins vor.
#'
#' @inheritParams feature_dummy
#' @param nr_bins Numerischer Wert mit der Anzahl der zu bestimmenden Bins
#'   (Default: 1).
#' @param breaks Numerischer Vektor mit den Positionen der Bin-Grenzen, der ggf.
#'   vorberechnet wurde und weitergereicht werden soll (Default: \emph{NULL},
#'   d.h. Breaks werden neu berechnet).
#'
#' @return Liste mit vorbereiteten Argumenten.
#'
#' @importFrom magrittr %>%
prepare_hist_equi <- function(x,
                              nr_bins = 1,
                              breaks = NULL,
                              ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.count(nr_bins),
                          breaks %is_null_or% assertthat::is.number)
  
  if (is.null(breaks)) {
    
    res <-
      x$value %>%
      range(na.rm = TRUE) %>%
      {
        seq(.[1], .[2],
            length.out = nr_bins + 1)
      }
    
  } else {
    
    res <- NULL
  }
  
  list(breaks = res)
}

#' @describeIn extract_hist Bereitet Histogramme mit flexiblen Bins vor.
#'
#' @inheritParams prepare_hist_equi
#'
#' @return Liste mit vorbereiteten Argumenten.
#'
#' @importFrom magrittr %>%
prepare_hist_vary <- function(x,
                              nr_bins = 1,
                              breaks = NULL,
                              ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.count(nr_bins),
                          breaks %is_null_or% assertthat::is.number)
  
  if (is.null(breaks)) {
    
    res <-
      stats::quantile(x$value,
                      probs = seq(0, 1,
                                  length.out = nr_bins + 1),
                      names = FALSE)
    
  } else {
    
    res <- NULL
  }
  
  list(breaks = res)
}

#' @describeIn extract_pca Bereitet PCA-Merkmale vor.
#'
#' @inheritParams feature_dummy
#' @param window_length_in_sec Numerischer Wert mit der einheitlichen Laenge der
#'   Signalfenster.
#' @param is_jittered Logischer Wert, ob Signalwerte minimal verrauscht werden
#'   (Default: \emph{TRUE}).
#' @param rotation \code{prcomp}-Objekt (Liste) mit den PCA-Rotationen, die ggf.
#'   vorberechnet wurden und weitergereicht werden sollen (Default: \emph{NULL},
#'   d.h. Rotationen werden neu berechnet).
#' @param .seed Numerischer Wert mit Seed-Wert, mit dem der Zufallsgenerator
#'   fuer das Verrauschen initialisiert wird (Default: \emph{0}).
#'
#' @return Liste mit vorbereiteten Argumenten.
#'
#' @importFrom magrittr %>%
prepare_pca <- function(x,
                        window_length_in_sec,
                        is_jittered = TRUE,
                        rotation = NULL,
                        .seed = 0,
                        ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.number(window_length_in_sec),
                          assertthat::is.flag(is_jittered),
                          rotation %is_null_or% is.list,
                          assertthat::is.number(.seed))
  
  if (is.null(rotation)) {
    
    set.seed(.seed)
    
    res <-
      x$value %>%
      {
        if (is_jittered) {
          
          jitter(., factor = 1E-10)
          
        } else .
      } %>%
      matrix(ncol = window_length_in_sec,
             byrow = TRUE) %>%
      stats::prcomp()
    
  } else {
    
    res <- NULL
  }
  
  list(rotation = res)
}

# Extraktionsfunktionen --------------------------------------------------------

#' FFT-Merkmale (fft_peak)
#'
#' Berechnet fuer ein Signal das mittlere Frequenzspektrum mittels
#' Fast-Fourier-Transformation und bestimmt die am staerksten auftretenden
#' Frequenzspitzen. Bei Bedarf kann das Frequenzspektrum zuvor noch geglaettet
#' werden.
#'
#' Fuer jede gefundene Spitze \emph{i} werden die zugehoerigen Merkmale
#' \emph{peak\strong{i}_freq} (Frequenz in Hz) und \emph{peak\strong{i}_amp}
#' (relative Amplitude) berechnet, z.B. \emph{peak1_amp}, \emph{peak1_freq},
#' \emph{peak2_amp} and \emph{peak2_freq} fuer \code{nr_peaks = 2}.
#'
#' Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
#' beachten, dass sie eine regulaere Abtastung besitzen sollten, da sonst kein
#' sinnvolles Frequenzspektrum berechnet wird.
#'
#' @inheritParams feature_dummy
#' @param f_in_hz Numerischer Wert mit der Samplingfrequenz des Signals in Hertz
#'   (Default: 1).
#' @param is_smoothed Logischer Wert, ob Amplituden des Frequenzspektrums
#'   geglaettet werden (Default: \emph{TRUE}).
#' @param nr_peaks Numerischer Wert mit der Anzahl der zu bestimmenden Spitzen
#'   im Frequenzspektrum (Default: 5).
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#' @aliases fft_peak
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
extract_fft_peak <- function(x,
                             f_in_hz = 1,
                             is_smoothed = TRUE,
                             nr_peaks = 5,
                             ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.number(f_in_hz),
                          assertthat::is.flag(is_smoothed),
                          assertthat::is.count(nr_peaks))
  
  res <- tibble::tibble()
  
  if (!is.null(x) & !any(is.na(x$value))) {
    
    try({
      
      spec <-
        x$value %>%
        diff() %>%
        seewave::meanspec(f = f_in_hz,
                          norm = FALSE,
                          plot = FALSE)
      
      if (is_smoothed) {
        
        spec[, 2] <- stats::smooth(spec[, 2],
                                   twiceit = TRUE)
      }
      
      peaks <-
        spec %>%
        seewave::fpeaks(nmax = nr_peaks,
                        plot = FALSE) %>%
        tibble::as_tibble()
      
      if (nrow(peaks) == nr_peaks) {
        
        peaks <- dplyr::arrange(peaks, dplyr::desc(.data$amp))
        
        res <-
          peaks %>%
          dplyr::mutate(id = name_seq(dplyr::n(),
                                      prefix = "peak")) %>%
          tidyr::pivot_longer(-"id") %>%
          tidyr::pivot_wider(names_from = c("id", "name"))
      }
    },
    silent = TRUE)
  }
  
  res
}

#' Linear-Fit-Merkmale (fit_linear)
#'
#' Berechnet fuer ein Signal eine lineare Regression, welche die Signalwerte
#' approximiert. Die resultierenden Merkmale heissen \emph{slope} (Anstieg) und
#' \emph{intercept} (Schnittpunkt y-Achse).
#'
#' Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
#' beachten, dass sie eine regulaere Abtastung besitzen sollten, da sonst
#' verfaelschte Regressionsparameter berechnet werden.
#'
#' @inheritParams feature_dummy
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#' @aliases fit_linear
#'
#' @importFrom magrittr %>%
extract_fit_linear <- function(x) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))
  
  res <- tibble::tibble()
  
  if (!is.null(x) & !all(is.na(x$value))) {
    
    model <- stats::lm(x$value ~ seq_len(length(x$value)))
    
    res <- tibble::tibble(
      
      slope      = model$coefficients[2],
      intercept  = model$coefficients[1]
    )
  }
  
  res
}

#' Binaere Form-Merkmale (form_bin)
#'
#' Berechnet fuer ein binaeres Signal (d.h. es treten nur zwei verschiedene
#' Signalwerte auf) die Anzahl von Flanken und die Laengen der Plateaus.
#' Waehrend eine Flanke hierbei den Uebergang zwischen unterschiedlichen Werten
#' bezeichnet, besteht ein Plateau fuer die Dauer eines unveraenderlichen Werts.
#'
#' Die resultierenden Flanken-Merkmale heissen \emph{edge_pos} (Anzahl Flanken
#' von negativ zu positiv), \emph{edge_neg} (Anzahl Flanken von positiv zu
#' negativ), \emph{edge_posneg} (Anzahl Flanken von negativ zu positiv und
#' wieder zurueck), \emph{edge_negpos} (Anzahl Flanken von positiv zu negativ
#' und wieder zurueck).
#'
#' Die resultierenden Plateau-Merkmale heissen \emph{plateau_pos} (summierte
#' Laengen der positiven Plateaus), \emph{plateau_pos} (summierte Laengen der
#' negativen Plateaus) sowie \emph{plateau_*_mean} (mittlere Laengen der
#' jeweiligen Plateaus) und \emph{plateau_*_var} (Laengenvarianz der jeweiligen
#' Plateaus).
#'
#' @inheritParams feature_dummy
#' @param value_pos Numerischer Wert, der den positiven Signalwert beschreibt
#'   (Default: 1).
#' @param value_neg Numerischer Wert, der den negativen Signalwert beschreibt
#'   (Default: 1).
#' @param is_relative Logischer Wert, ob Plateau-Merkmale standardisiert werden,
#'   d.h. relativ zur Signalfensterlaenge definiert sind (Default: \emph{TRUE}).
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#'
#' @aliases form_bin
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
extract_form_bin <- function(x,
                             value_pos = 1,
                             value_neg = 0,
                             is_relative = TRUE) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.number(value_pos),
                          assertthat::is.number(value_neg),
                          assertthat::is.flag(is_relative))
  
  res <- tibble::tibble()
  
  if (!is.null(x)) {
    
    window_length <- ifelse(is_relative,
                            diff_time(min(x$time), max(x$time)),
                            1)
    res <-
      x %>%
      dplyr::filter(value_has_changed(.data$value)) %>%
      dplyr::mutate(
        edge_pos =
          .data$value == value_pos & dplyr::lag(.data$value) == value_neg,
        edge_neg =
          .data$value == value_neg & dplyr::lag(.data$value) == value_pos,
        edge_posneg = .data$edge_neg & dplyr::lag(.data$edge_pos),
        edge_negpos = .data$edge_pos & dplyr::lag(.data$edge_neg),
        plateau =
          diff_time(.data$time,
                    dplyr::lead(.data$time, default = max(.data$time))) /
          window_length,
        plateau_pos =
          ifelse(.data$value == value_pos, .data$plateau, NA_real_),
        plateau_neg =
          ifelse(.data$value == value_neg, .data$plateau, NA_real_)) %>%
      dplyr::mutate_at(
        c("plateau_pos", "plateau_neg"),
        list(mean = mean, var = stats::var), na.rm = TRUE) %>%
      dplyr::mutate_at(
        dplyr::vars(tidyselect::matches("_(pos|neg|posneg|negpos)$")),
        sum, na.rm = TRUE) %>%
      dplyr::select(tidyselect::matches("^(edge|plateau)_")) %>%
      utils::head(1)
  }
  
  res
}

#' Histogramm-Merkmale (hist_equi, hist_vary)
#'
#' Berechnet fuer ein Signal ein Histogramm mit den Haeufigkeiten der
#' gruppierten Signalwerte (Bins). Dabei werden zur Gruppierung die im Argument
#' \code{breaks} uebergebenen Bin-Grenzen verwendet.
#'
#' Waehrend bei der Variante \code{extract_hist_equi} eine bestimmte Anzahl
#' gleich grosser Bins verwendet wird, verwendet die Variante
#' \code{extract_hist_vary} unterschiedlich grosse Bins, die auf der Lage der
#' Quantile beruhen. Zur Bestimmung der jeweiligen Bin-Grenzen werden mittels
#' der Funktionen \code{prepare_hist_equi} und \code{prepare_hist_vary} die
#' noetigen Argumente vorberechnet.
#'
#' Fuer jeden Bin \emph{i} wird das zugehoerigen Merkmal \emph{bin\strong{i}}
#' berechnet, z.B. \emph{bin1}, \emph{bin2} and \emph{bin3} fuer
#' \code{length(breaks) == 3}.
#'
#' Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
#' beachten, dass sie eine regulaere Abtastung besitzen sollten, da sonst ein
#' verfaelschtes Histogramm berechnet wird.
#'
#' @inheritParams feature_dummy
#' @param is_relative Logischer Wert, ob eine relative Haeufigkeit berechnet
#'   wird oder eine absolute (Default: \emph{TRUE}, d.h. es wird relativ
#'   gezaehlt).
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#'
#' @importFrom magrittr %>%
extract_hist <- function(x,
                         breaks,
                         is_relative = TRUE,
                         ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.count(breaks),
                          assertthat::is.flag(is_relative))
  
  res <- tibble::tibble()
  
  if (!is.null(x)) {
    
    res <-
      x$value %>%
      graphics::hist(plot = FALSE, breaks = breaks) %>%
      .$count %>%
      t()
    
    if (is_relative) {
      
      res <- res / length(x$value)
    }
    
    colnames(res) <- name_seq(ncol(res),
                              prefix = "bin")
    
    res <- tibble::as_tibble(res)
  }
  
  res
}

#' @describeIn extract_hist Berechnet Histogramme mit festen Bins.
#'
#' @aliases hist_equi
#'
#' @importFrom magrittr %>%
extract_hist_equi <- function(...) {
  
  extract_hist(...)
}

#' @describeIn extract_hist Berechnet Histogramme mit flexiblen Bins.
#'
#' @aliases hist_vary
#'
#' @importFrom magrittr %>%
extract_hist_vary <- function(...) {
  
  extract_hist(...)
}

#' Roh-Merkmale (raw)
#'
#' Gibt fuer ein Signal unveraendert die Signalwerte zurueck, z.b. die Merkmale
#' \emph{value1}, \emph{value2} und \emph{value3} fuer eine Signal mit
#' \code{nrow(x) == 3}.
#'
#' @inheritParams feature_dummy
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#' @aliases raw
#'
#' @importFrom magrittr %>%
extract_raw <- function(x) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))
  
  res <- tibble::tibble()
  
  if (!is.null(x)) {
    
    res <- purrr::map_dfc(x$value, identity)
    
    names(res) <- name_seq(ncol(res),
                           prefix = "value")
  }
  
  res
}

#' PCA-Merkmale (pca)
#'
#' Berechnet fuer ein Signal eine niedrigdimensionale Projektion basierend auf
#' den Eigenfunktionen der Hauptkomponentenanalyse (PCA).
#'
#' Fuer jede Komponente \emph{i} werden die zugehoerigen Merkmale \emph{pc_i}
#' berechnet, z.B. \emph{pc_1}, \emph{pc_2} und \emph{pc_3} fuer
#' \code{nr_components == 3}.
#'
#' Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
#' beachten, dass sie eine einheitliche Laenge und eine regulaere Abtastung
#' besitzen sollten, da sonst keine sinnvollen Eigenfunktionen berechnet werden.
#'
#' @inheritParams feature_dummy
#' @param nr_components Numerischer Wert mit der Anzahl der zu verwendenen
#'   PCA-Komponenten, d.h. der resultierenden Dimensionen (Default: 3).
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#' @aliases pca
#'
#' @importFrom magrittr %>%
extract_pca <- function(x,
                        rotation,
                        nr_components = 3,
                        ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          is.list(rotation),
                          assertthat::is.number(nr_components))
  
  res <- tibble::tibble()
  
  if (!is.null(x)) {
    
    try({
      
      res <-
        x$value %>%
        matrix(nrow = 1) %>%
        stats::predict(rotation, .) %>%
        tibble::as_tibble() %>%
        dplyr::select(seq_len(nr_components))
      
      names(res) <- name_seq(nr_components,
                             prefix = "pc")
    })
  }
  
  res
}

#' Statistik-Merkmale (stat)
#'
#' Berechnet fuer ein Signal die folgendenen statistischen Kennzahlen:
#' \emph{min} (Minimum), \emph{max} (Maximum), \emph{mean} (Mittelwert),
#' \emph{median} (Median) und \emph{var} (Varianz).
#'
#' Bei der Uebergabe der Signalfenster an Funktion \code{feature} ist zu
#' beachten, dass sie eine regulaere Abtastung besitzen sollten, da sonst
#' verfaelschte statistsche Kennzahlen berechnet werden.
#'
#' @inheritParams feature_dummy
#'
#' @return Dataframe mit berechneten Merkmalen.
#'
#' @family Merkmalsfunktionen
#' @aliases stat
#'
#' @importFrom magrittr %>%
extract_stat <- function(x) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))
  
  res <- tibble::tibble()
  
  if (!is.null(x)) {
    
    what <- c("min", "max", "mean", "median", "var")
    
    try(
      suppressWarnings({
        
        res <-
          what %>%
          purrr::map_dfc(do.call,
                         list(x$value, na.rm = TRUE)) %>%
          stats::setNames(what)
      }))
  }
  
  res
}
