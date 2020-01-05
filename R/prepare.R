# Funktionen zum Vorbereiten von Signal-Dataframes

# Harmonisierungsfunktionen ----------------------------------------------------

#' Harmonisierter Dataframe
#'
#' Ueberfuehrt einen Signal-Dataframe in einen standardisierten Tiqqle. Dabei
#' wird angenommen, dass der urspruengliche Dataframe ein langes Format besitzt.
#'
#' @param x Dataframe, der in die standardisierte Form gebracht werden soll.
#' @param time_var Spaltenname (Symbol oder String) der Zeitvariable im
#'   Dataframe \code{x} (Default: \emph{"time"}).
#' @param signal_var Spaltenname (Symbol oder String) der
#'   Signalnamen-Variable im Dataframe \code{x} (Default: \emph{"signal"}).
#' @param value_var Spaltenname (Symbol oder String) der Signalwert-Variable
#'   im Dataframe \code{x} (Default: \emph{"value"}).
#' @param format String, welches Format der erzeugte Tiqqle hat. Erlaubte Werte:
#'   \emph{"long"} oder \emph{"wide"} (Default: \emph{"long"}, d.h. langes
#'   Format).
#' @param ... Weitere Argumente, die ggf. an Funktion \code{as_wide}
#'   durchgereicht werden.
#'
#' @return Tiqqle im gewuenschten Format.
#'
#' @family Harmonisierungsfunktionen
#' @seealso \code{\link{as_wide}}
#'
#' @importFrom magrittr %>%
#' @export
harmonize <- function(x,
                      time_var = "time",
                      signal_var = "signal",
                      value_var = "value",
                      format = "long",
                      ...) {

  # Checkt Argumente, Pt. 1
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.string(format),
                          
                          format %in% c("long", "wide"))

  time_var   <- rlang::ensym(time_var)
  signal_var <- rlang::ensym(signal_var)
  value_var  <- rlang::ensym(value_var)
  
  # Checkt Argumente, Pt. 2
  assertthat::assert_that(is_temporal(x[[time_var]]))

  res <-
    x %>%
    # Spalten umbenennen
    dplyr::transmute(time = !!time_var,
                     signal = as.factor(!!signal_var),
                     value = as.numeric(!!value_var)) %>%
    # Ggf. zeitlich sortieren
    arrange2(.data$time) %>%
    # Format setzen
    as_tiqqle("long")

  if (format == "wide") {

    res <- as_wide(res, ...)
  }

  res
}

#' Harmonisierter Ereignis-Dataframe
#'
#' Ueberfuehrt einen Ereignis-Dataframe in ein standardisiertes Format.
#'
#' @param x Dataframe, der in die standardisierte Form gebracht werden soll.
#' @param start_var Spaltenname (Symbol oder String) der Variable mit dem
#'   Zeitpunkt des Eintretens des Ereignisses im Dataframe \code{x} (Default:
#'   \emph{"start"}).
#' @param end_var Spaltenname (Symbol oder String) der Variabele mit dem
#'   Zeitpunkt des Aufhebens des Ereignisses im Dataframe \code{x} (Default:
#'   \emph{"end"}).
#' @param ... Weitere Spaltennamen (Symbole oder Character), die als
#'   Zusatzinformationen zu den Ereignissen als Faktoren beibehalten werden.
#'
#' @return Dataframe mit den Ereignissen.
#'
#' @family Harmonisierungsfunktionen
#'
#' @importFrom magrittr %>%
#' @export
harmonize_event <- function(x,
                            start_var = "start",
                            end_var   = "end",
                            ...) {

  # Checkt Argumente, Pt. 1
  assertthat::assert_that(is.data.frame(x))

  start_var <- rlang::ensym(start_var)
  end_var   <- rlang::ensym(end_var)
  info_vars <- rlang::ensyms(...)
  
  # Checkt Argumente, Pt. 2
  assertthat::assert_that(is_temporal(x[[start_var]]),
                          is_temporal(x[[end_var]]))

  x %>%
    # Spalten umbenennen
    dplyr::transmute(start = !!start_var,
                     end   = !!end_var,
                     !!!info_vars) %>%
    # Infovariablen in Faktoren umwandeln
    dplyr::mutate_at(dplyr::vars(-"start", -"end"), as.factor) %>%
    # Ggf. zeitlich sortieren
    arrange2(.data$start)
}

# Transformationsfunktionen ----------------------------------------------------

#' Signalwert-Bereinigung
#'
#' Korrigiert Signalwert um einen bestimmten Offset, der zu einem gewissen
#' Zeitpunkt wechseln kann.
#'
#' @param x Tiqqle, der korrigiert wird.
#' @param which Character-Vektor, der das anzupassende Signal beinhaltet.
#' @param when POSIXct-Zeitstempel mit Zeitpunkt des Schwellwertwechsels
#'   (Default: \emph{NULL}, d.h. ein Schwellwertwechsel findet nicht statt).
#' @param op Funktion, die zum Anpassen der Signalwerte verwendet wird (Default:
#'   \code{`-`}, d.h. Schwellwert wird vom Signalwert subtrahiert).
#' @param value_before Numerischer Wert mit dem vor Zeitpunkt \code{when}
#'   gueltigen Schwellwert.
#' @param value_after Numerischer Wert mit dem ab Zeitpunkt \code{when}
#'   gueltigen Schwellwert (Default: \emph{NULL}, d.h. \code{value_after}
#'   identisch zu \code{value_before}).
#'
#' @return Tiqqle mit korrigiertem Signalwert.
#'
#' @family Transformationsfunktionen
#'
#' @export
limit <- function(x,
                  which,
                  when = NULL,
                  op = `-`,
                  value_before,
                  value_after = NULL) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::not_empty(x),
                          is.character(which),
                          when %is_null_or% is_temporal,
                          is.function(op),
                          assertthat::is.number(value_before),
                          value_after %is_null_or% assertthat::is.number)

  UseMethod("limit")
}

#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @export
limit.tiqqle_long <- function(x,
                              which,
                              when = NULL,
                              op = `-`,
                              value_before,
                              value_after = NULL) {

  res <-
    x %>%
    dplyr::filter(.data$signal == which) %>%
    dplyr::mutate(value = op(.data$value,
                             ifelse(.data$time < (when %||% Inf),
                                    value_before,
                                    value_after %||% value_before)))

  x %>%
    dplyr::filter(.data$signal != which) %>%
    dplyr::bind_rows(res) %>%
    arrange2(.data$time)
}

#' @export
limit.tiqqle_wide <- function(x,
                              which,
                              when = NULL,
                              op = `-`,
                              value_before,
                              value_after = NULL) {

  dplyr::mutate_at(x,
                   which,
                   ~ op(., ifelse(.data$time < (when %||% Inf),
                                  value_before,
                                  value_after %||% value_before)))
}

#' Signalnormalisierung
#'
#' Normalisiert die Signale eines Dataframes, indem deren Werte in einen
#' einheitlichen Wertebereich ueberfuehrt werden. Standardmaessig wird eine
#' 0-1-Normierung angewandt.
#'
#' Prinzipiell laesst sich hierfuer auch eine andere Normalisierungsfunktion
#' einsetzen. Diese vorher zu definierende Funktion bekommt als erstes Argument
#' den numerischen Vektor der Signalwerte uebergeben und gibt einen entsprechend
#' normierten Vektor zurueck.
#'
#' @param x Tiqqle, der normalisiert wird.
#' @param fun Funktion, die zur Normalisierung der Signalwerte verwendet wird
#'   (Default: \code{\link{norm_range}}).
#' @param ... Weitere Argumente, die an Funktion \code{fun} durchgereicht
#'   werden.
#'
#' @return Normalisierter Tiqqle \code{x}.
#'
#' @family Reduktionsfunktionen
#' @seealso \code{\link{norm_range}}
#'
#' @export
normalize <- function(x,
                      fun = norm_range,
                      ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::not_empty(x),
                          is.function(fun))
  
  UseMethod("normalize")
}

#' @importFrom magrittr %>%
#' @export
normalize.tiqqle_long <- function(x,
                                  fun = norm_range,
                                  ...) {

  x %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$signal) %>%
    dplyr::mutate(value = fun(.data$value, ...)) %>%
    dplyr::ungroup() %>%
    as_tiqqle("long")
}

#' @export
normalize.tiqqle_wide <- function(x,
                                  fun = norm_range,
                                  ...) {

  dplyr::mutate_at(x,
                   .vars = dplyr::vars(-"time"),
                   .funs = fun, ...)
}

# Schneidefunktionen -----------------------------------------------------------

#' Zeitlich beschnittene Signale
#'
#' Schneidet aus einem Dataframe mit einer Gesamtsignalmenge eine Reihe von
#' zeitlichen Fenstern aus. Dabei ist diese Teilmenge durch jeweils einen Start-
#' und einen Endzeitpunkt definiert. Falls einer dieser beiden Zeitpunkte leer
#' bleibt, wird automatisch der kleinste bzw. groesste auftretende Zeitstempel
#' verwendet (Platzhalter).
#'
#' Um fehlende Signaleintraege am Anfang/Ende des geschnittenen Fensters zu
#' vermeiden, koennen an diesen Zeitpunkten zusaetzliche Signalwerte eingefuegt
#' werden. Hierfuer wird der zu diesem Zeitpunkt letzte gueltige Wert bestimmt
#' und hinzugefuegt (d.h. \emph{Last observation carried forward}). Da u.U. fuer
#' den Startzeitpunkt des Fensters kein vorheriger Wert existiert, kann in
#' diesem Fall auch der naechste gueltige Signalwert eingesetzt werden (d.h.
#' \emph{Next observation carried backward}).
#'
#' @param x Tiqqle, der geschnitten wird.
#' @param start POSIXct-Vektor mit Startzeitpunkten der Fenster (Default:
#'   \emph{NULL}, d.h. der kleinste vorhandene Zeitstempel wird angenommen).
#' @param end POSIXct-Vektor mit Endzeitpunkten der Fenster (Default:
#'   \emph{NULL}, d.h. der groesste vorhandene Zeitstempel wird angenommen).
#' @param pad_start Logischer Wert, ob geschnittener Dataframe am Anfang mit den
#'   jeweils letzten gueltigen Signalwerten aufgefuellt werden soll (Default:
#'   \emph{TRUE}, d.h. Signale werden aufgefuellt).
#' @param pad_end Logischer Wert, ob geschnittener Dataframe am Ende mit den
#'   jeweils letzten gueltigen Signalwerten aufgefuellt werden soll (Default:
#'   \emph{TRUE}, d.h. Signale werden aufgefuellt).
#' @param fill_na Logischer Wert, ob beim Auffuellen am Anfang des Dataframes
#'   (Argument \code{pad_start == TRUE}) bei evtl. Auftreten von
#'   \emph{NA}-Werten diese durch den naechsten gueltigen Signalwert ersetzt
#'   werden (Default: \emph{FALSE}, d.h. Signale werden nicht rueckwaerts
#'   aufgefuellt).
#'
#' @return Tiqqle mit zeitlich beschnittenen (und ggf. aufgefuellten)
#'   Signalen aus \code{x}.
#'
#' @importFrom rlang %||%
#' @export
crop <- function(x,
                 start = NULL,
                 end = NULL,
                 pad_start = TRUE,
                 pad_end = TRUE,
                 fill_na = FALSE) {

  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          is_null_or(start, purrr::every, .p = is_temporal),
                          is_null_or(end, purrr::every, .p = is_temporal),
                          assertthat::is.flag(pad_start),
                          assertthat::is.flag(pad_end),
                          assertthat::is.flag(fill_na))
              
  # Zurueck, wenn nix zu tun ist
  if (is.null(start) && is.null(end)) {

    return(x)
  }

  # Stellt sicher, dass x zeitlich sortiert ist
  x <- arrange2(x, .data$time)

  # Gueltiger Start
  start <- start %||% min(x$time, na.rm = FALSE)

  # Gueltiges Ende
  end <- end %||% max(x$time, na.rm = FALSE)

  purrr::map2_dfr(start, end,

                  function(start, end) {

                    res <- vector("list", 3)

                    # Letzten gueltigen Wert vor Start
                    if (pad_start) {

                      res[[1]] <- last_value(x,
                                             before = start, fill_na = fill_na)
                    }

                    # Letzten gueltigen Wert vor Ende
                    if (pad_end) {

                      res[[3]] <- last_value(x,
                                             before = end, fill_na = fill_na)
                    }

                    # Werte zwischen Start und Ende
                    res[[2]] <- dplyr::filter(x,
                                              .data$time > start,
                                              .data$time < end)

                    # Teile zusammenfuegen
                    dplyr::bind_rows(res)
                  })
}

#' Letzter gueltiger Signalwert
#'
#' Bestimmt den letzten Wert pro Signal, der vor einem bestimmten Zeitpunkt
#' gueltig war. Falls dieser nicht gefunden wird (d.h. resultierendes
#' \emph{NA}), kann der naechste nach diesem Zeitpunkt gueltige Wert
#' herangezogen werden.
#'
#' @param x Tiqqle, aus dem der letzte gueltige Wert bestimmt wird.
#' @param before POSIXct-Zeitstempel vor dem der gueltige Wert bestimmt wird.
#' @param fill_na Logischer Wert, ob evtl. entstehende \emph{NA}-Werte mit dem
#'   naechsten gueltigen Signalwert ersetzt werden (Default: \emph{FALSE}, d.h.
#'   sie werden nicht ersetzt).
#'
#' @return Tiqqle mit letzten gueltigen Signalwerten aus \code{x}.
#'
#' @keywords internal
last_value <- function(x,
                       before,
                       fill_na = FALSE) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::not_empty(x),
                          is_temporal(before),
                          assertthat::is.flag(fill_na))

  UseMethod("last_value")
}

#' @describeIn last_value Letzter gueltiger Wert im langen Format
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
last_value.tiqqle_long <- function(x,
                                   before,
                                   fill_na = FALSE) {

  # Alle auftretenden Signale
  x1 <- tibble::tibble(time = before,
                       signal = unique(x$signal))

  # Alle vorher auftretenden Werte
  x2 <-
    x %>%
    dplyr::filter(.data$time <= before) %>%
    dplyr::select(.data$signal, .data$value)

  # Letzter gueltiger Wert pro Signal
  res <-
    dplyr::left_join(x1, x2, by = "signal") %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$time, .data$signal) %>%
    dplyr::summarize(value = dplyr::last(.data$value)) %>%
    dplyr::ungroup()

  # Falls noetig, NAs durch naechsten gueltigen Wert ersetzen
  if (fill_na && purrr::some(res$value, is.na)) {

    res <-
      x %>%
      dplyr::filter(.data$time > before) %>%
      dplyr::select(.data$signal, .data$value) %>%
      dplyr::left_join(res, ., by = "signal") %>%
      # EVTL: parallel moeglich
      dplyr::group_by(.data$time, .data$signal) %>%
      # Entscheiden, welcher Wert uebernommen wird
      dplyr::summarize(value =
                         is.na(.data$value.x) %>%
                         ifelse(.data$value.y, .data$value.x) %>%
                         dplyr::first()) %>%
      dplyr::ungroup()
  }

  as_tiqqle(res, "long")
}

#' @describeIn last_value Letzten gueltiger Wert im breiten Format
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
last_value.tiqqle_wide <- function(x,
                                   before,
                                   fill_na = FALSE) {

  # Letzter gueltiger Wert pro Signal
  res <-
    x %>%
    dplyr::filter(.data$time <= before) %>%
    fill_signal_wide(fill_na = fill_na,
                     direction = "forward") %>%
    utils::tail(1)

  # Falls noetig, NAs durch naechsten gueltigen Wert ersetzen
  if (fill_na && purrr::some(res, is.na)) {

    res2 <-
      x %>%
      dplyr::filter(.data$time > before) %>%
      fill_signal_wide(fill_na = fill_na,
                       direction = "backward") %>%
      utils::head(1)

    # Entscheiden, welcher Wert uebernommen wird
    # EVTL: parallel moeglich
    res <- purrr::map2_dfc(res, res2,
                           ~ ifelse(is.na(.x), .y, .x))
  }

  dplyr::bind_cols(tibble::tibble(time = before),
                   res %>% dplyr::select(-.data$time)) %>%
    as_tiqqle("wide")
}

# Reduktionsfunktionen ---------------------------------------------------------

#' Ohne wiederholte Signalwerte
#'
#' Ueberprueft, ob sich in einem Dataframe aufeinanderfolgende Signalwerte
#' veraendert haben oder nicht. Falls nicht, werden die redundanten Werte
#' entfernt. Auf diese Weise wird ein Dataframe mit wiederholungsfreien
#' Signalwerten erzeugt.
#'
#' @param x Tiqqle, der reduziert wird.
#'
#' @return Reduzierter Tiqqle \code{x}.
#'
#' @seealso \code{\link{value_has_changed}}
#'
#' @family Reduktionsfunktionen
#'
#' @export
condense <- function(x) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x))

  UseMethod("condense")
}

#' @importFrom magrittr %>%
#' @export
condense.tiqqle_long <- function(x) {

  x %>%
    arrange2(.data$time) %>%
    # EVTL: parallel moeglich
    tidyr::nest(data = c("time", "value")) %>%
    dplyr::mutate(
      data = purrr::map(.data$data,
                        ~ dplyr::filter(.,
                                        value_has_changed(.data$value)))) %>%
    tidyr::unnest("data") %>%
    dplyr::select(.data$time, .data$signal, .data$value) %>%
    as_tiqqle("long")
}

#' @importFrom magrittr %>%
#' @export
condense.tiqqle_wide <- function(x) {

  x %>%
    arrange2(.data$time) %>%
    tidyr::fill(-"time") %>%
    {
      dplyr::filter(.,
                    dplyr::select(., -.data$time) %>%
                      value_has_changed())
    } %>%
    as_tiqqle("wide")
}

#' Ohne konstante Signale
#'
#' Filtert konstante Signale aus einem Dataframe aus. Konstant heisst dabei,
#' dass im gesamten Zeitraum nur ein einziger Signalwert auftritt.
#'
#' @param x Tiqqle, der gefiltert wird.
#' @param ... Weitere Argumente, die an Funktion \code{\link{is_constant}}
#'   durchgereicht werden.
#'
#' @return Tiqqle \code{x} ohne konstante Signale.
#'
#' @family Reduktionsfunktionen
#' @seealso \code{\link{is_constant}}
#'
#' @export
remove_constant <- function(x,
                            ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::not_empty(x))

  UseMethod("remove_constant")
}

#' @importFrom magrittr %>%
#' @export
remove_constant.tiqqle_long <- function(x,
                                        ...) {

  x %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$signal) %>%
    dplyr::filter(!is_constant(.data$value, ...)) %>%
    dplyr::ungroup() %>%
    as_tiqqle("long")
}

#' @importFrom magrittr %>%
#' @export
remove_constant.tiqqle_wide <- function(x,
                                        ...) {

  dplyr::select_if(x,
                   function(x, ...) {

                     !is_constant(x, ...)
                   },
                   ...)
}

# Zeitfunktionen ---------------------------------------------------------------

#' Regularisierte zeitliche Signalaufloesung
#'
#' Fuellt fuer jedes Signal evtl. vorhandende zeitliche Luecken auf. Dabei wird
#' fuer jeden im Dataframe auftretenden Zeitstempel ein entsprechender
#' Signalwert eingefuegt. Darueber hinaus koennen auch alle Luecken aufgefuellt
#' werden, die nur implizit vorliegen (d.h. Zeitstempel fuer die kein regulaerer
#' Signalwert existiert).
#'
#' @param x Tiqqle, der regularisiert wird.
#' @param insert_missing Logischer Wert, ob auch Zeitstempel und Signalwerte
#'   eingefuegt werden, fuer die kein expliziter regulaerer Wert vorliegt
#'   (Default: \emph{TRUE}, d.h. alle Zeitstempel werden aufgefuellt).
#' @param fill_gap Logischer Wert, ob entstehende \emph{NA}-Werte mittels
#'   \code{\link{fill}} aufgefuellt werden (Default: \emph{FALSE}, d.h. sie
#'   werden nicht veraendert).
#' @param by String mit dem zu verwendenden Zeitintervall (Default:
#'   \emph{"sec"}, d.h. Sekundenintervall). Siehe \code{\link{seq.POSIXt}} fuer
#'   Details.
#'
#' @return Regularisierter Tiqqle \code{x}.
#'
#' @family Zeitfunktionen
#' @seealso \code{\link{seq.POSIXt}}, \code{\link{fill}}
#'
#' @importFrom magrittr %>%
#' @export
regularize <- function(x,
                       insert_missing = TRUE,
                       fill_gap = FALSE,
                       by = "sec") {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::is.flag(insert_missing),
                          assertthat::is.flag(fill_gap),
                          assertthat::is.string(by))

  UseMethod("regularize")
}

#' @importFrom magrittr %>%
#' @export
regularize.tiqqle_long <- function(x,
                                   insert_missing = TRUE,
                                   fill_gap = FALSE,
                                   by = "sec") {

  if (insert_missing) {

    res <-
      x$time %>%
      {
        seq(min(.),
            max(.),
            by = by)
      }
  } else {

    res <- unique(x$time)
  }

  x1 <-
    tibble::tibble(time = res) %>%
    arrange2(.data$time)

  x2 <-
    tibble::tibble(signal = unique(x$signal))

  res <-
    tidyr::crossing(x1, x2) %>%
    dplyr::left_join(x, by = c("time", "signal"))

  if (fill_gap) {

    res <-
      res %>%
      arrange2(.data$time) %>%
      # EVTL: parallel moeglich
      dplyr::group_by(.data$signal) %>%
      tidyr::fill("value") %>%
      dplyr::ungroup()
  }

  as_tiqqle(res, "long")
}

#' @export
regularize.tiqqle_wide <- function(x,
                                   insert_missing = TRUE,
                                   fill_gap = FALSE,
                                   by = "sec") {

  res <- arrange2(x, .data$time)

  if (insert_missing) {

    res <-
      x$time %>%
      {
        seq(min(.),
            max(.),
            by = by)
      } %>%
      tibble::tibble(time = .) %>%
      dplyr::left_join(res, by = "time")
  }

  if (fill_gap) {

    res <- tidyr::fill(res, -"time")
  }

  as_tiqqle(res, "wide")
}

#' Aggregierte zeitliche Signalaufloesung
#'
#' Fasst Signalwerte in einem hoeheren zeitlichen Intervall zusammen. Dabei
#' werden die urspruenglichen Zeitstempel zunaechst einer hoeheren
#' Zeitaufloesung zugeordnet. Anschliessend werden Signalwerte mit demselben
#' (neuen) Zeitstempel mittels einer Aggregationsfunktion zusammengefasst.
#' Standardmaessig wird hier der erste Signalwert beibehalten.
#'
#' Prinzipiell laesst sich hierfuer auch eine andere Aggregationsfunktion
#' einsetzen (z.B. Mittelwertbildung). Diese vorher zu definierende Funktion
#' bekommt als erstes Argument den numerischen Vektor der Signalwerte uebergeben
#' und gibt einen entsprechend aggregierten numerischen Signalwert zurueck.
#'
#' @param x Tiqqle, der zeitlich aggregiert wird.
#' @param interval_sec Numerischer Wert mit der Einheit (in Sekunden) der neuen
#'   Zeitaufloesung (Default: 1, d.h. Signale werden in 1-Sekunden-Intervalle
#'   aggregiert).
#' @param fun Funktion, die zur Aggregation der Signalwerte verwendet wird
#'   (Default: \code{dplyr::first}, d.h. der erste im Intervall auftretende
#'   Signalwert wird beibehalten).
#' @param ... Weitere Argumente, die an Funktion \code{fun} durchgereicht
#'   werden.
#'
#' @return Aggregierter Tiqqle \code{x}.
#'
#' @family Zeitfunktionen
#'
#' @importFrom magrittr %>%
#' @export
thicken <- function(x,
                    interval_sec = 1,
                    fun = dplyr::first,
                    ...) {

  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::is.number(interval_sec),
                          is.function(fun))

  UseMethod("thicken")
}

#' @importFrom magrittr %>%
#' @export
thicken.tiqqle_long <- function(x,
                                interval_sec = 1,
                                fun = dplyr::first,
                                ...) {

  x %>%
    thicken_prepare(interval_sec) %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$time, .data$signal) %>%
    dplyr::summarize(value = fun(.data$value, ...)) %>%
    dplyr::ungroup() %>%
    as_tiqqle("long")
}

#' @importFrom magrittr %>%
#' @export
thicken.tiqqle_wide <- function(x,
                                interval_sec = 1,
                                fun = dplyr::first,
                                ...) {

  x %>%
    thicken_prepare(interval_sec) %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$time) %>%
    dplyr::summarize_all(fun, ...) %>%
    dplyr::ungroup() %>%
    as_tiqqle("wide")
}

# Hilfsfunktionen --------------------------------------------------------------

#' Letzter gueltiger Signalwert im breiten Format
#'
#' Hilfsfunktion zur Bestimmung des letzten oder naechsten Signalwerts im
#' breiten Format.
#'
#' @param x Tiqqle, aus dem der gueltige Wert bestimmt wird.
#' @param fill_na Logischer Wert, ob evtl. entstehende \emph{NA}-Werte mit dem
#'   naechsten gueltigen Signalwert ersetzt werden (Default: \emph{TRUE}, d.h.
#'   sie werden ersetzt).
#' @param direction String mit zeitlicher Ersetzungsrichtung - entweder
#'   \emph{"forward"} (Default: letzter gueltiger Wert ) oder \emph{"backward"}
#'   (naechster gueltiger Wert).
#'
#' @return Tiqqle mit letzten gueltigen Signalwerten aus \code{x}.
#'
#' @seealso \code{\link{last_value.tiqqle_wide}}
#'
#' @keywords internal
fill_signal_wide <- function(x,
                             fill_na = TRUE,
                             direction = "forward") {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::not_empty(x),
                          assertthat::is.flag(fill_na),
                          assertthat::is.string(direction),
                          
                          direction %in% c("forward", "backward"))
  
  y <- dplyr::slice(x,
                    ifelse(direction == "forward",
                           dplyr::n(),
                           1))
  
  if (nrow(x) == 0) {
    
    # NAs einfuegen, falls keine Werte existieren
    x <- x[1, ]
    
  } else if (fill_na && purrr::some(y, is.na)) {
    
    # Auffuellen, falls noetig
    x <-
      tidyr::fill(x,
                  .direction = ifelse(direction == "forward",
                                      "down",
                                      "up"))
  }
  
  dplyr::slice(x,
               ifelse(direction == "forward",
                      dplyr::n(),
                      1))
}

#' Vorbereitung zeitlicher Signalaggregation
#'
#' Hilfsfunktion zur Vorbereitung der zeitlichen Signalaggregation
#' (\code{\link{thicken}}).
#'
#' @inheritParams thicken
#'
#' @return Zur Aggregation vorbereiteter Tiqqle \code{x}.
#'
#' @seealso \code{\link{thicken}}
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
thicken_prepare <- function(x,
                            interval_sec = 1) {

  unit <- paste(interval_sec, "sec")

  x %>%
    arrange2(.data$time) %>%
    dplyr::mutate(time = lubridate::round_date(.data$time, unit))
}
