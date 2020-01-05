# Funktionen zum Verarbeiten von Fenstern

# Extraktionsfunktionen --------------------------------------------------------

#' Fenster aus Signalen
#'
#' Schneidet fuer eine Ereignis-Menge die zugehoerigen Signale aus einem
#' Signal-Dataframe. Dabei werden die Ereignisse durch ihre paarweisen Start-
#' und Endzeitpunkte definiert. Diese Funktion ist ein Wrapper um \code{crop}.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Tiqqle aus dem die Fenster extrahiert werden.
#' @param start POSIXct-Vektor mit Startzeitpunkten der Fenster.
#' @param end POSIXct-Vektor mit Endzeitpunkten der Fenster.
#' @param fun Funktion, die direkt nach der Extraktion zusaetzlich auf die
#'   resultierende Fenster angewendet wird (Default: \emph{NULL}, d.h. es wird
#'   keine weitere Funktion ausgefuehrt).
#' @param ... Weitere Argumente, die ggf. an Funktion \code{fun} durchgereicht
#'   werden.
#'
#' @return Liste mit extrahierten Fenster-Tiqqles.
#'
#' @family Fensterfunktionen
#' @seealso crop
#'
#' @export
window_crop <- function(x,
                        start,
                        end,
                        fun = NULL,
                        .progress = TRUE,
                        ...) {

  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          is_temporal(start),
                          is_temporal(end),
                          fun %is_null_or% is.function,
                          assertthat::is.flag(.progress))

  res <-
    furrr::future_map2(start, end,
                       ~ crop(x, .x, .y),
                       .progress = .progress)

  if (!is.null(fun)) {

    res <- furrr::future_map(res,
                             fun, ...,
                             .progress = .progress)
  }

  res
}

# Korrekturfunktionen ----------------------------------------------------------

#' Fensterausrichtung
#'
#' Eine Menge von Fenster-Dataframes wird auf eine gemeinsame zeitliche Achse
#' gebracht. Dabei wird fuer jedes Fenster die absolute Zeitinformation in eine
#' relative ueberfuehrt.
#'
#' Von allen Zeitstempeln wird der kleinste vorliegende Zeitstempel abgezogen
#' und somit ein neuer (numerischer) Ursprungszeitpunkt 0 geschaffen. Bei Bedarf
#' laesst sich dieser wiederum in einen POSIXct-Zeitstempel mit Ursprung
#' 1970-01-01 umwandeln.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Liste mit Fenster-Tiqqles, die zeitlich ausgerichtet werden.
#' @param ... Weitere Argumente, die an \code{\link{relative_time}}
#'   durchgereicht werden.
#'
#' @return Liste mit zeitlich ausgerichteten Fenster-Tiqqles.
#'
#' @family Fensterfunktionen
#' @seealso \code{\link{relative_time}}
#'
#' @export
window_align <- function(x,
                         .progress = FALSE,
                         ...) {

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          purrr::every(x, is_valid),
                          assertthat::is.flag(.progress))

  furrr::future_map(x,
                    function(x, ...) {

                      dplyr::mutate(x, time = relative_time(.data$time, ...))
                    },
                    ...,
                    .progress = .progress)
}

#' Fensterstreckung
#'
#' Eine Menge von Fenster-Dataframes wird auf eine einheitliche Laenge gebracht.
#' Dabei wird der jeweils groesste Zeitstempel so normiert, dass er dem
#' numerischen Wert \code{max_in_sec} entspricht. Die restlichen Zeitstempel
#' werden entsprechend angepasst, d.h. sie bekommen einen Zeitstempel zwischen 0
#' und \code{max_in_sec}. Abschliessend werden die numerischen Zeitinformationen
#' wieder in POSIXct-Zeitstempel umgewandelt.
#'
#' Effektiv kann bei bei diesem Verfahren also sowohl eine Streckung als auch
#' eine Stauchung stattfinden. Signalinformationen gehen dabei nicht verloren.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Liste mit Fenster-Tiqqles, die zeitlich gestreckt werden.
#' @param max_in_sec Numerischer Wert, auf welche Laenge (in Sekunden) die
#'   Fenster gestreckt werden (Default: 60, d.h. alle Fenster werden auf 60
#'   Sekunden gestreckt.
#'
#' @return Liste mit zeitlich gestreckten Fenster-Tiqqles.
#'
#' @family Fensterfunktionen
#'
#' @importFrom magrittr %>%
#' @export
window_stretch <- function(x,
                           max_in_sec = 60,
                           .progress = FALSE) {

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          purrr::every(x, is_valid),
                          assertthat::is.number(max_in_sec),
                          assertthat::is.flag(.progress))

  x %>%
    window_align(as_datetime = FALSE,
                 .progress = .progress) %>%
    furrr::future_map(
      function(x) {

        dplyr::mutate(x,
                      time =
                        (.data$time / max(.data$time) * max_in_sec) %>%
                        lubridate::as_datetime())
      },
      .progress = .progress)
}

#' Fensterbeschneidung
#'
#' Eine Menge von Fenster-Dataframes wird auf eine einheitliche Laenge gebracht.
#' Dabei wird zunaechst das kuerzeste Fenster bestimmt. Dessen Laenge wird dann
#' auf die restlichen Fenster angewendet und evtl. vorhandene Signalaenderungen
#' nach diesem Zeitpunkt verworfen. Bei diesem Ansatz gehen also
#' Signalinformationen verloren.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Liste mit Fenster-Tiqqles, die zeitlich beschnitten werden.
#'
#' @return Liste mit zeitlich beschnittenen Fenster-Tiqqles.
#'
#' @family Fensterfunktionen
#'
#' @importFrom magrittr %>%
#' @export
window_truncate <- function(x,
                            .progress = FALSE) {

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          purrr::every(x, is_valid),
                          assertthat::is.flag(.progress))

  x <- window_align(x, .progress = .progress)

  min_time <-
    x$time %>%
    purrr::map_dbl(max) %>%
    min() %>%
    lubridate::as_datetime()

  x %>%
    furrr::future_map(crop, end = min_time,
                      .progress = .progress)
}

# Verschmelzungsfunktionen -----------------------------------------------------

#' Fensterverschmelzung
#'
#' Eine Menge von Fenster-Dataframes wird zu einem einzigen Fenster-Dataframe
#' zusammengefasst. Dabei werden die Signalwerte der Einzelfenster punktweise
#' (d.h. jeweils identische Signale und Zeitpunkte) mittels einer
#' Verschmelzungsfunktion (z.B. Mittelwertbildung) aggregiert.
#'
#' Prinzipiell laesst sich hierfuer auch eine andere Verschmelzungsfunktion
#' einsetzen. Diese vorher zu definierende Funktion bekommt als erstes Argument
#' den numerischen Vektor der punktweisen Signalwerte uebergeben und gibt einen
#' einzelnen numerischen Rueckgabewert zurueck.
#'
#' @param x Liste mit Fenster-Tiqqles, die verschmolzen werden.
#' @param fun Funktion, die zum Verschmelzen der Fenster verwendet wird
#'   (Default: \code{mean}, d.h. Mittelwertbildung).
#' @param na.rm Logischer Wert, ob vorhandene \emph{NA}-Werte ignoriert werden
#'   (Default: \emph{TRUE}, d.h. sie werden nicht beruecksichtigt).
#' @param ... Weitere Argumente, die an Funktion \code{fun} durchgereicht
#'   werden.
#'
#' @return Dataframe mit verschmolzenen Fenster-Tiqqles.
#'
#' @family Fensterfunktionen
#'
#' @export
window_merge <- function(x,
                         fun = mean,
                         na.rm = TRUE,
                         ...) {

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          purrr::every(x, is_valid),
                          is.function(fun),
                          assertthat::is.flag(na.rm))

  x %>%
    dplyr::bind_rows() %>%
    regularize(insert_missing = FALSE,
               fill_gap = TRUE) %>%
    window_merge_aggregate(fun, na.rm, ...)
}

#' Fensterzusammenfassung
#'
#' Hilfsfunktion zum Verschmelzen von Fenstern (\code{\link{window_merge}}). Ein
#' Fenster-Dataframe wird zusammengefasst und evtl. fehlende Zeitpunkte mit
#' entsprechenden Signalwerten aufgefuellt.
#'
#' @param x Fenster-Tiqqle, der verschmolzen wird.
#' @param fun Funktion, die zum Verschmelzen der Fenster verwendet wird.
#' @param na.rm Logischer Wert, ob vorhandene \emph{NA}-Werte ignoriert werden.
#' @param ... Weitere Argumente, die an Funktion \code{fun} durchgereicht
#'   werden.
#'
#' @return Dataframe mit zusammengefassten Fenstern.
#'
#' @keywords internal
#'
#' @seealso \code{\link{window_merge}}
window_merge_aggregate <- function(x,
                                   fun,
                                   na.rm,
                                   ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          is.function(fun),
                          assertthat::is.flag(na.rm))

  UseMethod("window_merge_aggregate")
}

#' @describeIn window_merge_aggregate Fasst Fenster im langen Format beim
#'   Verschmelzen zusammen
#'
#' @importFrom magrittr %>%
#' @export
window_merge_aggregate.tiqqle_long <- function(x,
                                               fun,
                                               na.rm,
                                               ...) {

  x %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$time, .data$signal) %>%
    dplyr::summarize(value = fun(.data$value, na.rm = na.rm, ...)) %>%
    dplyr::ungroup() %>%
    as_tiqqle("long")
}

#' @describeIn window_merge_aggregate Fasst Fenster im breiten Format beim
#'   Verschmelzen zusammen
#'
#' @importFrom magrittr %>%
#' @export
window_merge_aggregate.tiqqle_wide <- function(x,
                                               fun,
                                               na.rm,
                                               ...) {

  x %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$time) %>%
    dplyr::summarize_all(fun, na.rm = na.rm, ...) %>%
    purrr::map_dfc(replace_nan, y = NA) %>%
    as_tiqqle("wide") %>%
    condense()
}

# Hilfsfunktionen --------------------------------------------------------------

#' Doku-Dummyfunktion
#'
#' Hilfsfunktion zu Dokumentationszwecken.
#'
#' @section Future: Da dieser Verarbeitungsschritt u.U. etwas laenger dauern
#'   kann, kann hierfuer mittels \code{future::plan} eine parallele Verarbeitung
#'   zugeschaltet werden. Durch das Argument \code{.progress} kann ggf. ein
#'   Forschrittsbalken angezeigt werden.
#'
#' @name future_dummy
#' @param .progress Logischer Wert, ob Fortschrittsbalken angezeigt wird.
#'
#' @keywords internal
NULL
