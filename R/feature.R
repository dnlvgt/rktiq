# Funktionen zum Berechnen von Merkmalen

# Merkmalsfunktionen -----------------------------------------------------------

#' Merkmale fuer Fenster
#'
#' Fuer eine Menge von Fenstern werden Merkmalsbeschreibungen extrahiert.
#' Hierbei wird zunaechst ein Vorbereitungs- und anschließend ein
#' Extraktionsschritt durchgefuehrt. Die resultierenden Merkmale stellen eine
#' kompakte Repraesentation des Signalverlaufs dar und koennen als
#' Trainingsdaten fuer maschinelles Lernen verwendet werden.
#'
#' Die zu berechnenden Merkmale werden mittels des Arguments \code{feat}
#' ausgewaehlt. Dieses erwartet eine benannte Liste mit je einem Eintrag pro
#' Merkmalstyp. Waehrend der Name des Listeneintrags den Namen des zu
#' berechnenden Merkmalstyp angibt (und somit die aufzurufenden Funktionen),
#' beschreibt das Listenelement die hierfuer zu verwendenden Funktionsargumente.
#' Ein Listenelement besteht dabei wiederum aus einer benannten Liste. Der Namen
#' des Eintrags beschreibt den zu setzenden Argumentnamen, der Wert des Eintrags
#' den entsprechenden zu uebergebenden Argumentwert. Falls kein zusaetzlicher
#' Argumentwert existiert oder die Default-Werte verwendet werden sollen, wird
#' hier eine leere Liste \code{list()} oder der Wert \emph{NULL} uebergeben.
#'
#' Die verfuegbaren Merkmalstypen koennen mit der Hilfsfunktion
#' \code{feature_all} aufgelistet werden. Aktuell sind dies:
#' \itemize{
#'   \item \link{fft_peak}
#'   \item \link{fit_linear}
#'   \item \link{form_bin}
#'   \item \link{hist_equi}
#'   \item \link{hist_vary}
#'   \item \link{raw}
#'   \item \link{pca}
#'   \item \link{stat}
#' }
#'
#' In den jeweiligen Funktionen sind die verfuegbaren Argumente dokumentiert.
#' Fuer einige Merkmalsberechnungen ist es noetig, im Vorbereitungsschritt
#' gewisse Argumente global vorzuberechnen. Dieses Verhalten kann durch das
#' Argument \code{arg_global} gesteuert werden.
#'
#' Das Ergebnis der Merkmalsberechnung liegt intern als geschachtelte Liste vor.
#' Diese Merkmalsliste beinhaltet fuer jedes Fenster einen Eintrag mit einem
#' Dataframe, der fuer jedes Signal wiederum eine benannte Liste mit den
#' berechneten Merkmale umfasst. Diese Liste kann mittels der Argumente
#' \code{as_df} und \code{as_wide} in einen Dataframe im passenden Format
#' konvertiert werden.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Liste mit Fenster-Tiqqles (bzw. ein Einzel-Tiqqle), fuer die
#'   Merkmale berechnet werden.
#' @param feat Benannte Liste mit den zu berechnenden Merkmalen und den
#'   zugehoerigen Argumenten. Siehe Details fuer ausfuehrlichere Erlaeuterungen.
#' @param arg_global Logischer Wert, ob evtl. benoetigte Funktionsargumente
#'   global ueber alle Fenster vorberechnet werden oder lokal fuer die
#'   Einzelfenster (Default: \emph{TRUE}, d.h. sie werden global berechnet).
#' @param as_df Logischer Wert, ob Merkmale als Dataframe zurueckgegeben werden
#'   oder als Liste (Default: \emph{TRUE}, d.h. Merkmals-Dataframe wird
#'   erzeugt).
#' @param as_wide Logischer Wert, ob Merkmals-Dataframe ggf. im breiten Format
#'   zurueckgegeben wird oder im langen Format (Default: \emph{TRUE}, d.h.
#'   Merkmals-Dataframe wird im breiten Format erzeugt).
#' @param ... Weitere Argumente, die an Funktion \code{feature_df} durchgereicht
#'   werden.
#'
#' @return Liste oder Dataframe mit berechneten Merkmalen.
#'
#' @seealso \code{\link{feature_all}}, code{\link{feature_df}},
#'   \code{\link{feature_wide}}
#'
#' @importFrom magrittr %>%
#' @export
feature <- function(x,
                    feat,
                    arg_global = TRUE,
                    as_df = TRUE,
                    as_wide = TRUE,
                    .progress = TRUE,
                    ...) {

  group_by_signal <- function(x,
                              .progress) {

    # Für jedes Fenster...
    furrr::future_map(x,
                      function(x) {

                        #TODO: Pruefen, ob auch fuer wide moeglich
                        if (is_wide(x)) {

                          x <- as_long(x, remove_na = TRUE)
                        }

                        # Schachtelt Signale
                        x %>%
                          arrange2(.data$time) %>%
                          tidyr::nest(data = c("time", "value"))
                      },
                      .progress = .progress)
  }

  # Checkt die Datentypen der Argumente
  stopifnot(is.list(x),
            is.list(feat),
            is.logical(arg_global),
            is.logical(as_df),
            is.logical(as_wide),
            is.logical(.progress))

  # Checkt die Inhalte der Argumente
  if (length(x) == 0) {

    stop("Fenster x darf nicht leer sein.")
  }

  if (length(feat) == 0) {

    stop("Merkmalsliste feat darf nicht leer sein.")
  }

  if (is.null(names(feat))) {

    stop("Merkmalsliste feat darf nicht leer sein.")
  }

  # Einzelnen Tiqqle in Liste packen
  if (is_tiqqle(x)) {

    x <- list(x)
  }

  # Vorbereitungsschritt: Argumente vorberechnen
  prepare <-
    x %>%
    {
      if (arg_global) {

        dplyr::bind_rows(.) %>%
          list()

      } else .

    } %>%
    group_by_signal(.progress = .progress) %>%
    furrr::future_map(                                    # Für jedes Fenster...
      ~ dplyr::mutate(.,
        arg = purrr::map(data,                            # Für jedes Signal...
                         function(x) {

                           purrr::imap(feat,              # Für jedes Merkmal...
                                       ~ call_fun(fun    = .y,
                                                  x      = x,
                                                  arg1   = .x,
                                                  prefix = "prepare_"))
                          })) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$signal, .data$arg),
      .progress = .progress)

  # Recycling der globalen Argumente
  if (arg_global) {

    prepare <- rep(prepare, length(x))
  }

  # Extract-Schritt: Merkmale berechnen
  res <-
    x %>%
    group_by_signal(.progress = .progress)  %>%
    furrr::future_map2(
      prepare,                 # Für jedes Fenster mit zugehörigen Argumenten...
      ~  dplyr::left_join(.x, .y,
                          by = "signal") %>%
        dplyr::mutate(
          feature =
            purrr::map2(.data$data, .data$arg,            # Für jedes Signal...
                        function(x, arg2) {

                          purrr::imap(feat,               # Für jedes Merkmal...
                                      ~ call_fun(fun    = .y,
                                                 x      = x,
                                                 arg1   = .x,
                                                 arg2   = arg2,
                                                 prefix = "extract_"))
                        })) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$signal, .data$feature),
      .progress = .progress)

  # In Dataframe umwandeln
  if (as_df) {

    res <- feature_df(res, ...)

    if (as_wide) {

      res <- feature_wide(res)
    }
  }

  res
}

#' Alle Merkmalstypen
#'
#' Listet alle verfuegbaren Merkmalstypen auf, die als potentielle Argumente an
#' Funktion \code{feature} uebergeben werden koennen.
#'
#' @return Character-Vektor mit den verfuegbaren Merkmalstypen.
#'
#' @importFrom magrittr %>%
#' @export
feature_all <- function() {

  ls(envir = asNamespace("rktiq"),
     pattern = "^extract_") %>%
    stringr::str_remove_all("^extract_")
}

#' Merkmale in Kontextfenstern
#'
#' Stellt fuer eine Menge von Merkmalen einen zeitlichen Verlauf her, der als
#' Kontext bezeichnet wird. Ein einzelnes Kontextfenster umfasst hierbei die mit
#' der Funktion \code{feature} fuer ein Signalfenster berechneten Merkmale und
#' entspricht in dem resultierenden breiten Merkmals-Dataframe einer Zeile. Der
#' Gesamtkontext umfasst hierbei die Menge der gleitenden Kontextfenster, deren
#' Zusammensetzung und Anordnung mittels \code{window_length} (Fensterlaenge)
#' und \code{window_step} (Schrittweite) angegeben werden.
#'
#' Auf diese Weise werden mehrere Kontextfenster in einer Zeile des neuen
#' Merkmals-Dataframe zusammengefasst. Durch die Benennung der auf diese Weise
#' entstehenden Spalten koennen Rueckschluesse auf die urspruengliche Benennung
#' gezogen werden. Das im aktuellen Kontext zeitlich gesehen letzte
#' Kontextfenster hat hierbei den Index 0, das vorherige Fenster entsprechend
#' den Index 1 usw. Falls der Ausgangs-Dataframe eine Spalte mit dem Namen
#' \emph{id} besitzt, so wird diese beibehalten und angepasst.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Dataframe mit Merkmalen im breiten Format.
#' @param window_length Numerischer Wert mit der Fensterlaenge, d.h. Anzahl der
#'   Kontextfenster, die zusammen einen Kontext bilden.
#' @param window_step Numerischer Wert mit der Schrittweite, um welche die
#'   Fenster zeitlich weitergeschoben werden.
#' @param prefix String mit Prefix, der den neuen Merkmalsnamen (Spalten)
#'   vorangestellt wird (Default: \emph{"context"}).
#'
#' @return Dataframe mit Merkmalen in Kontextfenstern.
#'
#' @importFrom magrittr %>%
#' @export
feature_context <- function(x,
                            window_length,
                            window_step,
                            prefix = "context",
                            .progress = FALSE) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(window_length),
            is.numeric(window_step),
            is.character(prefix),
            is.logical(.progress))

  # Checkt die Inhalte der Argumente
  if (nrow(x) == 0) {

    stop("Dataframe x darf nicht leer sein.")
  }

  if (window_length <= 0) {

    stop("Fensterl\u00e4nge window_length muss positiv sein")
  }

  if (window_step <= 0) {

    stop("Schrittweite window_step muss positiv sein")
  }

  # Schiebt das Kontextfenster über die Merkmale...
  seq(from = 1,
      to   = nrow(x) - window_length + 1,
      by   = window_step) %>%
    furrr::future_map2_dfr(
      .x = .,
      .y = . + window_length - 1,
      function(from, to) {

        context_id <- NULL
        x <- dplyr::slice(x, from:to)

        # Sichert aktuell gültige ID
        if (rlang::has_name(x, "id")) {

          context_id <- utils::tail(x$id, 1)
          x <- dplyr::select(x, -.data$id)
        }

        # Transformiert die Merkmale und benennt sie um
        x <-
          x %>%
          dplyr::mutate(id = paste0(prefix,
                                    abs(dplyr::row_number() - dplyr::n()))) %>%
          tidyr::pivot_longer(-"id") %>%
          tidyr::pivot_wider(names_from = c("id", "name")) %>%
          dplyr::mutate(id = context_id)

        # Stellt aktuell gültige ID an den Anfang
        if (rlang::has_name(x, "id")) {

          x <- dplyr::select(x,
                             .data$id,
                             tidyselect::everything())
        }

        x
      })
}

#' Konvertiert Merkmalsliste in Dataframe
#'
#' Eine Merkmalsliste, wie sie von der Funktion \code{feature} erzeugt wird,
#' wird in einen Dataframe im langen Format umgewandelt.
#'
#' Der resultierende Dataframe umfasst die Spalten \emph{feature_set}
#' (Merkmalsmenge), \emph{feature} (Merkmalsnamen), \emph{signal} (Signalnamen
#' fuer den das Merkmal berechnet wurde) sowie \emph{value} (Merkmalswert).
#' Zusaetzlich kann eine Spalte \emph{id} hinzugefuegt werden, welche die
#' Fenster-ID beinhaltet.
#'
#' Mit der ergaenzenden Funktion \code{feature_wide} kann der Merkmals-Dataframe
#' in ein breites Format konvertiert werden.
#'
#' @param x Liste mit den berechneten Merkmalen.
#' @param add_id Logischer Wert, ob eine ID-Spalte hinzugefuegt werden soll
#'   (Default: \emph{TRUE}).
#'
#' @return Merkmals-Dataframe im langen Format.
#'
#' @seealso \code{\link{feature}}, \code{\link{feature_wide}}
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
feature_df <- function(x,
                       add_id = TRUE) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.list(x),
            is.logical(add_id))

  res <-
    x %>%
    purrr::imap_dfr(
      function(x, id) {

        res <-
          purrr::map2_dfr(
            x$feature, x$signal,
            function(feature, signal) {

              res <-
                names(feature) %>%
                {
                  purrr::map2_dfr(
                    .,
                    name_unique(.),
                    function(feat, name) {

                      feature[[feat]] %>%
                        tidyr::pivot_longer(cols = tidyselect::everything(),
                                            names_to = "feature") %>%
                        dplyr::mutate(feature_set = name)
                    })
                }

              if (nrow(res) > 0) {

                res <-
                  res %>%
                  dplyr::mutate(signal = signal) %>%
                  dplyr::select(.data$feature_set, feature, signal, .data$value)
              }

              res
            }
          )

        if (add_id) {

          res <- dplyr::mutate(res, id = id)
        }

        res
      }
    )

  if (nrow(res) == 0) {

    return(tibble::tibble())
  }

  if (add_id) {

    # dict <-
    #   length(x) %>%
    #   name_seq()
    #
    # names(dict) <- seq_along(x)

    res <-
      res %>%
      # dplyr::mutate(id = dict[.data$id]) %>%
      dplyr::select(.data$id, tidyselect::everything())
  }

  res %>%
    dplyr::mutate_if(is.character, factor)
}

#' Konvertiert langen Merkmals-Dataframe in breites Format
#'
#' Ein Merkmals-Dataframe wird vom langen ins breite Format umgewandelt.
#'
#' Das lange Format, wie es von der Funktion \code{feature_df} erzeugt wird,
#' besitzt hierbei die Spalten \emph{feature_set} (Merkmalsmenge),
#' \emph{feature} (Merkmalsnamen), \emph{signal} (Signalnamen fuer den das
#' Merkmal berechnet wurde) sowie \emph{value} (Merkmalswert). Zusaetzlich kann
#' die Spalte \emph{id} mit der Fenster-ID vorhanden sein. Das resultierende
#' breite Format besitzt fuer jede Kombination aus Merkmalsmenge/-name und
#' Signal eine entsprechende Spalte mit dem berechneten Merkmalswert. Diese
#' Spalten sind nach dem Schema \emph{[feature_set]-[feature]-[signal]} benannt.
#' Falls vorhanden, wird die Spalte \emph{id} uebernommen.
#'
#' @param x Merkmals-Dataframe im langen Format, der konvertiert wird.
#'
#' @return Merkmals-Dataframe im breiten Format.
#'
#' @seealso \code{\link{feature}}, \\code{\link{feature_df}}
#'
#' @keywords internal
feature_wide <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

  if (nrow(x) == 0) {

    return(tibble::tibble())
  }

  tidyr::pivot_wider(x,
                     names_from = c("feature_set", "feature", "signal"),
                     names_sep = "-")
}

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(nr_bins),
            is.null(breaks) || is.numeric(breaks))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(nr_bins),
            is.null(breaks) || is.numeric(breaks))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(window_length_in_sec),
            is.logical(is_jittered),
            is.null(rotation) || is.list(rotation),
            is.numeric(.seed))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(f_in_hz),
            is.logical(is_smoothed),
            is.numeric(nr_peaks))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.numeric(breaks),
            is.logical(is_relative))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x),
            is.list(rotation),
            is.numeric(nr_components))

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

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

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

# Hilfsfunktionen --------------------------------------------------------------

#' Dynamischer Funktionsaufruf
#'
#' Hilfsfunktion zum Aufrufen einer Funktion. Der Funktionsname wird dabei als
#' Teilstrings uebergeben sowie die Funktionsargumente als einzelne Listen bzw.
#' als Signal-Dataframe. Aus diesen Bestandteilen wird dann dynamisch ein
#' Funktionsaufruf erzeugt und ausgefuehrt. Dieser Mechanismus ermoeglicht ein
#' flexibles Erweitern und Aufrufen von Vorbereitungs-/Extraktionsfunktionen zur
#' Merkmalsberechnung.
#'
#' @inheritParams feature_dummy
#' @param fun String mit dem (Stamm-)Namen der aufzurufenden Funktion.
#' @param arg1 Benannte Liste mit dem ersten Teil der (ggf. vorgegebenen)
#'   Argumente (Default: \emph{NULL}, d.h. es werden keine weiteren Argumente
#'   beruecksichtigt).
#' @param arg2 Benannte Liste mit dem zweiten Teil der (ggf. vorberechneten)
#'   Argumente (Default: \emph{NULL}, d.h. es werden keine weiteren Argumente
#'   beruecksichtigt).
#' @param prefix String mit Prefix, der dem (Stamm-)Namen beim Funktionsaufruf
#'   vorangestellt wird (Default: \emph{""}, d.h. \code{fun} wird unveraendert
#'   aufgerufen).
#'
#' @return Rueckgabewerte der aufgerufenen Funktion.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
call_fun <- function(fun,
                     x,
                     arg1 = NULL,
                     arg2 = NULL,
                     prefix = "") {

  # Checkt die Datentypen der Argumente
  stopifnot(is.character(fun),
            is.data.frame(x),
            is.character(prefix))

  res <- NULL

  # Name der aufzurufenden Funktion
  fun_action <- paste0(prefix, fun)

  # Falls Funktion vorhanden...
  if (exists(fun_action, mode = "function")) {

    # Argumente der Funktion
    args <-
      list(x = x) %>%    # Signal-Dataframe
      c(arg1,            # zusätzliche Argumente
        arg2[[fun]]) %>%
      purrr::compact()   # leere Einträge verwerfen

    # Funktion aufrufen
    res <- do.call(fun_action, args)
  }

  res
}

#' Doku-Dummyfunktion
#'
#' Hilfsfunktion zu Dokumentationszwecken.
#'
#' @name feature_dummy
#' @param x Dataframe, der das zu verarbeitetende Signal enthaelt und mindestens
#'   eine Spalte \emph{value} besitzt.
#' @param ... Weitere Argumente, die durchgereicht werden koennen.
#'
#' @keywords internal
NULL
