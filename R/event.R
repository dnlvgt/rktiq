# Funktionen zum Verarbeiten von Ereignissen

# Ereignisfunktionen -----------------------------------------------------------

#' Reduziert ueberlappende Ereignisse
#'
#' Fasst mehrere sich zeitlich ueberlappende Einzelereignisse zu einem
#' Gesamtereignis zusammen. Dabei reicht das resultierende Gesamtereignis vom
#' kleinsten Startzeitpunkt bis zum groessten Endzeitpunkt der Einzelereignisse.
#' Die Ereignisse werden anhand ihrer Start-/Endzeitpunkte beschrieben, die
#' jeweils in den Spalten \emph{start} und \emph{end} uebergeben werden.
#'
#' @param x Dataframe, der die zu reduzierende Menge an Ereignissen beinhaltet.
#'
#' @return Dataframe mit den reduzierten Ereignissen aus \code{x}.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
event_condense <- function(x) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

  # Relevante Spalten beibehalten
  x <- dplyr::select(x, .data$start, .data$end)

  # Gibt es (vorhergehend) ueberlappende Ereignisse?
  res <-
    x %>%
    {
      tidyr::crossing(rename2(., suffix = "_x"),
                      rename2(., suffix = "_y"))
    } %>%
    dplyr::filter(overlap(.data$start_x, .data$end_x,
                          .data$start_y, .data$end_y))

  # Zurueck, falls nicht
  if (nrow(res) == 0) {

    return(x)
  }

  # Mehrere ueberlappende Ereignisse reduzieren
  res %>%
    dplyr::group_by(.data$start_x, .data$end_x) %>%
    dplyr::summarize(start_y = min(.data$start_y),
                     end_y   = max(.data$end_y)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(start = pmin(.data$start_x, .data$start_y),
                     end   = pmax(.data$end_x, .data$end_y)) %>%
    dplyr::distinct() %>%
    # Reduziert die verbleibenden (nachfolgend) ueberlappenden Ereignisse
    event_merge()
}

#' Invertiert Ereignisse
#'
#' Bestimmt fuer eine Ereignismenge die Differenzmenge. Dabei wird als
#' Differenz- ereignis der Zeitraum zwischen zwei gegebenen Ereignissen
#' angenommen. Die Ereignisse werden anhand ihrer Start-/Endzeitpunkte
#' beschrieben, die jeweils in den Spalten \emph{start} und \emph{end}
#' uebergeben werden. Zusaetzlich kann noch ein impliziter Invertierungszeitraum
#' uebergeben werden, mit dem der Anfang und das Ende explizit definiert wird.
#' Ansonsten entspricht der Anfang dem ersten und das Ende dem letzten
#' Differenzereignis.
#'
#' @param x Dataframe, der die zu invertierende Menge an Ereignissen beinhaltet.
#' @param int_start POSIXct-Zeitstempel mit impliziten Startzeitpunkt des
#'   Invertierungszeitraums (Default: \emph{NULL}, d.h. nur explizite
#'   Ereignisse).
#' @param int_end POSIXct-Zeitstempel mit impliziten Endzeitpunkt des
#'   Invertierungszeitraums (Default: \emph{NULL}, d.h. nur explizite
#'   Ereignisse).
#'
#' @return Dataframe mit den invertierten Ereignissen aus \code{x}.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
event_invert <- function(x,
                         int_start = NULL,
                         int_end = NULL) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          is_null_or(int_start, is_temporal, is_strict = TRUE),
                          is_null_or(int_end, is_temporal, is_strict = TRUE))
  x %%
    event_condense() %>%
    dplyr::arrange(.data$start, .data$end) %>%
    {
      tibble::tibble(start = c(int_start %||% NA, .$end),
                     end   = c(.$start, int_end %||% NA))
    } %>%
    stats::na.omit() %>%
    dplyr::mutate_all(lubridate::as_datetime) %>%
    dplyr::filter(.data$start < .data$end)
}

#' Ereignisse in Signalen
#'
#' Sucht in einer Signalmenge nach bestimmten Ereignissen. Dabei ist ein
#' Ereignis durch den Zeitpunkt des Erfuellens einer bestimmten Bedingungen
#' (bezueglich des Signalwerts) gekennzeichnet (z.B. \code{value == 1} oder
#' \code{value != 0}). Sollte die Bedingung an mehreren direkt
#' aufeinanderfolgenden Zeitpunkten erfuellt sein, wird nur der erste Zeitpunkt
#' zurueckgegeben.
#'
#' @param x Tiqqle, der durchsucht werden soll.
#' @param which Character-Vektor, der die zu durchsuchenden Signale beinhaltet.
#' @param op Funktion, die zum Testen der Signalwerte verwendet wird (Default:
#'   \code{`==`}, d.h. es wird auf Gleichheit getestet - z.B. \code{value ==
#'   test_value}).
#' @param test_value Wert, der den Signalwert beschreibt, auf den getestet wird.
#' @param label String mit dem Namen des Ereignisses, das ggf. dem Ergebnis als
#'   zusaetzliche Spalte hinzugefuegt wird  (Default: \emph{NULL}, d.h. keine
#'   Namensspalte).
#'
#' @return Dataframe mit den gefundenen Ereignissen.
#'
#' @family Ereignisfunktionen
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
event_detect <- function(x,
                         which,
                         op = `==`,
                         test_value,
                         label = NULL) {

  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          is.character(which),
                          is.function(op),
                          assertthat::is.scalar(test_value),
                          label %is_null_or% assertthat::is.string)

  res <- x[0, ]

  #TODO: Pruefen, ob auch fuer wide moeglich
  if (is_wide(x)) {

    # Wandelt DF in langes Format um
    x <-
      x %>%
      dplyr::select(.data$time, dplyr::one_of(which)) %>%
      as_long(remove_na = TRUE) %>%
      condense()
  }

  if (is_long(x)) {

    # Waehlt Signale aus
    res <- dplyr::filter(x, .data$signal %in% which)

    if (nrow(res) > 0) {

      res <-
        res %>%
        # Stellt Sortierung sicher
        arrange2(.data$time) %>%
        # Bestimmt Menge der potentiellen Ereignisse
        dplyr::mutate(.test = op(.data$value, test_value)) %>%
        dplyr::group_by(.data$signal) %>%
        first_row_per(.data$.test) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$.test) %>%
        dplyr::select(-.data$.test) %>%
        arrange2(.data$time) %>%
        dplyr::mutate(label = label)
    }
  }

  res
}

#' Zueinandergehoerige Ereignisse
#'
#' Ordnet jedem Ereignis aus Menge \code{x} ein passendes Ereignis aus \code{y}
#' zu. Dabei wird die Zeitdifferenz zwischen den Zeitstempeln der beiden Mengen
#' betrachtet und die am haeufigsten auftretende Differenz ermittelt. Darauf
#' aufbauend werden alle Zeitdifferenzen verworfen, die ausserhalb eines
#' Toleranzfensters um die haeufigste Differenz liegen.
#'
#' @param x Dataframe mit der Ereignismenge \emph{x}.
#' @param y Dataframe mit der Ereignismenge \emph{y}.
#' @param time_var_x Spaltenname (Symbol oder String) der Zeitvariable in
#'   Dataframe \code{x} (Default: \emph{"time"}).
#' @param time_var_y Spaltenname (Symbol oder String) der Zeitvariable in
#'   Dataframe \code{y} (Default: \emph{"time"}).
#' @param tolerance_in_sec Numerischer Wert mit der Laenge des Toleranzfensters
#'   in Sekunden (Default: 10, d.h. Toleranzfenster von 10 Sekunden).
#'
#' @return Dataframe mit den zugeordneten Zeitpunkten.
#'
#' @family Ereignisgfunktionen
#' @seealso \code{\link{diff_time}}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
event_match_freq <- function(x,
                             y,
                             time_var_x = "time",
                             time_var_y = "time",
                             tolerance_in_sec = 10) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::not_empty(x),
                          is.data.frame(y),
                          assertthat::not_empty(x),
                          assertthat::is.number(tolerance_in_sec))
  
  time_var_x <-
    rlang::enquo(time_var_x) %>%
    rlang::as_name() %>%
    paste0("_x")
  
  time_var_y <-
    rlang::enquo(time_var_y) %>%
    rlang::as_name() %>%
    paste0("_y")
  
  x <- rename2(x, suffix = "_x")
  y <- rename2(y, suffix = "_y")

  # Jede Kombination aus Zeitstempeln
  tidyr::crossing(x, y) %>%
    # Zeitdifferenz zwischen x und y
    dplyr::mutate(.offset = diff_time(.data[[time_var_x]],
                                      .data[[time_var_y]])) %>%
    # Ereignisse verwerfen, ...
    # deren Zeitdifferenz außerhalb des Toleranzfensters liegt
    dplyr::filter(
      abs(.data$.offset -
            find_mode(.data$.offset,
                      binwidth = tolerance_in_sec / 2)) <= tolerance_in_sec) %>%
    # Falls mehrere Kandidaten existieren...
    # ...verwende das Ereignis mit der geringsten Zeitdifferenz
    dplyr::group_by(.data[[time_var_x]]) %>%
    dplyr::arrange(abs(.data$.offset)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.offset)
}

#' Aufeinanderfolgende Ereignisse
#'
#' Ordnet jedem Startereignis aus Menge \code{x} ein passendes Endereignis aus
#' \code{y} zu. Dabei wird standardmaessig davon ausgegangen, dass der
#' zugehoerige Startzeitpunkt der (zeitlich gesehen) letzte ist, der vor dem
#' Endzeitpunkt liegt. Falls gewuenscht, werden die Spalten des resultierenden
#' Dataframes umbenannt, um eine eindeutige Zuordnung zu ermoeglichen.
#'
#' @section Anmerkung:
#' Prinzipiell liesse sich diese Funktion auch mit einem Join realisieren,
#' allerdings kommt es dabei zu Speicherplatzproblemen.
#'
#' @inheritSection future_dummy Future
#'
#' @inheritParams future_dummy
#' @param x Dataframe mit der Startereignismenge \code{x}.
#' @param y Dataframe mit der Endereignismenge \code{y}.
#' @param time_var_x Spaltenname (Symbol oder String) der Zeitvariable in
#'   Dataframe \code{x} (Default: \emph{"time"}).
#' @param time_var_y Spaltenname (Symbol oder String) der Zeitvariable in
#'   Dataframe \code{y} (Default: \emph{"time"}).
#' @param last_before Logischer Wert, ob letzter Startzeitpunkt vor Endzeitpunkt
#'   bestimmt werden soll oder erster Startzeitpunkt nach Endzeitpunkt (Default:
#'   \emph{TRUE}, d.h. letzter Zeitpunkt wird gefunden).
#' @param is_strict Logischer Wert, ob eine minimale zeitliche Differenz
#'   zwischen den Ereignissen vorliegen muss, damit sie als aufeinanderfolgend
#'   gelten. Anderenfalls gelten Ereignisse mit identischem Start/Ende ebenfalls
#'   als aufeinanderfolgend (Default: \emph{TRUE}, d.h. die strengere Definition
#'   gilt).
#' @param remove_na Logischer Wert, ob \emph{NA}-Eintraege im resultierenden
#'   Dataframe entfernt werden. D.h. Elemente aus \code{y}, denen kein Zeitpunkt
#'   aus \code{x} zugeordnet werden kann, werden verworfen (Default:
#'   \emph{TRUE}, d.h. \emph{NA}-Werte werden entfernt).
#'
#' @return Dataframe mit den zugeordneten Zeitpunkten.
#'
#' @family Ereignisfunktionen
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
event_match_seq <- function(x,
                            y,
                            time_var_x = "time",
                            time_var_y = "time",
                            last_before = TRUE,
                            is_strict   = TRUE,
                            remove_na   = TRUE,
                            .progress   = FALSE) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          is.data.frame(y),
                          assertthat::is.flag(last_before),
                          assertthat::is.flag(is_strict),
                          assertthat::is.flag(remove_na),
                          assertthat::is.flag(.progress))

  time_var_x <-
    rlang::enquo(time_var_x) %>%
    rlang::as_name() %>%
    paste0("_x")

  time_var_y <-
    rlang::enquo(time_var_y) %>%
    rlang::as_name() %>%
    paste0("_y")

  x <- rename2(x, suffix = "_x")
  y <- rename2(y, suffix = "_y")

  if (nrow(x) == 0 || nrow(y) == 0)
    return(dplyr::bind_cols(x, y))

  y[[time_var_y]] %>%
    furrr::future_map_dfr(

      function(.time) {

        # Letztes Startereignis vor dem aktuellen Endereignis...
        # ...oder erstes Startereignis nach dem aktuellen Endereignis
        op1 <- ifelse(last_before,
                      ifelse(is_strict, `<`, `<=`),
                      ifelse(is_strict, `>`, `>=`))
        op2 <- ifelse(last_before,
                      utils::tail, utils::head)

        res <-
          x %>%
          dplyr::filter(op1(.data[[time_var_x]], .time)) %>%
          op2(1)

        # Dummy-Eintrag, falls kein passendes Startereignis gefunden wurde
        if (nrow(res) == 0) {

          res <- x[1, ]
          res[,] <- NA
        }

        res
      },
      .progress = .progress) %>%
    dplyr::bind_cols(y) %>%
    arrange2(time_var_x) %>%
    {
      if (remove_na) {

        dplyr::filter(.,
                      !is.na(.data[[time_var_x]]),
                      !is.na(.data[[time_var_y]]))
      } else .
    }
}

#' Verschmilzt angrenzende Ereignisse
#'
#' Fasst mehrere zeitlich angrenzende Einzelereignisse zu einem Gesamtereignis
#' zusammen. Als angrenzend werden dabei Ereignisse betrachtet, deren zeitlicher
#' Abstand unterhalb eines Schwellwerts liegt (Toleranzbereich). Das
#' resultierende Gesamtereignis reicht vom kleinsten Startzeitpunkt bis zum
#' groessten Endzeitpunkt der Einzelereignisse. Die Ereignisse werden anhand
#' ihrer Start-/Endzeitpunkte beschrieben, die jeweils in den Spalten
#' \emph{start} und \emph{end} uebergeben werden.
#'
#' @param x Dataframe, der die zu verschmelzende Menge an Ereignissen
#'   beinhaltet.
#' @param threshold_in_sec Numerischer Wert, wie viele Sekunden zwei Ereignisse
#'   maximal auseinander liegen duerfen, um noch als angrenzend zu gelten
#'   (Toleranzbereich) (Default: 0, d.h. sie muessen direkt benachbart sein).
#'
#' @return Dataframe mit den verschmolzenen Ereignissen aus \code{x}.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
event_merge <- function(x,
                        threshold_in_sec = 0) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.number(threshold_in_sec))

  x %>%
    # Sortierung sicherstellen
    dplyr::arrange(.data$start, .data$end) %>%
    # Verschmilzt die angrenzenden Ereignisse
    dplyr::mutate(.group =
                    dplyr::lag(.data$end) %>%
                    diff_time(.data$start) %>%
                    magrittr::is_greater_than(threshold_in_sec) %>%
                    tidyr::replace_na(TRUE) %>%
                    cumsum()) %>%
    dplyr::group_by(.data$.group) %>%
    dplyr::summarize(start = min(.data$start),
                     end   = max(.data$end)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.group)
}

#' Klassen fuer Ereignisse
#'
#' Fuegt einer Menge von Ereignissen bestimmte Klasseninformationen hinzu. Diese
#' wird bezueglich einer Menge an Zielereignissen ermittelt, indem das naechste
#' nachfolgende Zielereignis bestimmt wird.
#'
#' @param x Dataframe, der die zu klassierende Menge an Ereignissen beinhaltet.
#' @param target_event Dataframe, der die Zielereignisse beinhaltet. Die
#'   Ereignisse sind anhand ihrer Start-/Endzeitpunkte beschrieben, die jeweils
#'   in den Spalten \emph{start} und \emph{end} uebergeben werden.
#' @param target_cut_in_sec Numerischer Wert mit der Laenge des Zeitfensters (in
#'   Sekunden), das vor jedem Zielereignis liegt und den Bereich der potentiell
#'   positiven Ereignisse umfasst (Default: \emph{NULL}, d.h. ist nicht
#'   gesetzt).
#' @param add_id Logischer Wert, ob eine ID-Spalte hinzugefuegt werden soll
#'   (Default: \emph{TRUE}).
#' @param keep_label Logischer Wert, ob bereits existierende Klassenlabel von
#'   den Zielereignissen uebernommen werden oder ggf. neue berechnet werden
#'   sollen (Default: \emph{FALSE}, d.h. anhand von \code{target_cut_in_sec}
#'   werden neue binaere Label berechnet).
#' @param keep_time Logischer Wert, ob Zeitabstand zum naechsten Zielereignis
#'   als Spalte \emph{time_in_sec} beibehalten wird und ggf. als numerische
#'   Regressionsgroesse verwendet werden kann (Default: \emph{TRUE}).
#'
#' @return Dataframe mit den um Klassen ergaenzten Ereignissen aus \code{x}.
#'
#' @family Ereignisfunktionen
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
event_target <- function(x,
                         target_event,
                         target_cut_in_sec = NULL,
                         add_id = TRUE,
                         keep_label = FALSE,
                         keep_time = TRUE) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          is.data.frame(target_event),
                          target_cut_in_sec %is_null_or% assertthat::is.number,
                          assertthat::is.flag(add_id),
                          assertthat::is.flag(keep_label),
                          assertthat::is.flag(keep_time),
                          rlang::has_name(target_event, "label") ||
                            !is.null(target_cut_in_sec))

  # Zugehörige Paare Ereignis/Zielereignis
  res <-
    event_match_seq(target_event, x,
                    time_var_x = "start",
                    time_var_y = "end",
                    last_before = FALSE,
                    is_strict = FALSE) %>%
    dplyr::mutate(time_in_sec = diff_time(.data$end_y, .data$start_x))

  if (keep_label) {

    res <- dplyr::rename(res,
                         label = .data$label_x)
  } else {

    res <- dplyr::mutate(res,
                         label = .data$time_in_sec <= target_cut_in_sec)
  }

  res <- dplyr::select(res,
                       start = .data$start_y,
                       end   = .data$end_y,
                       time_in_sec = .data$time_in_sec,
                       label = .data$label)

  if (add_id) {

    res <- dplyr::mutate(res,
                         id = dplyr::row_number())
  }

  if (!keep_time) {

    res <- dplyr::select(res, -.data$time_in_sec)
  }

  res
}

# Ereignistests ------------------------------------------------------------------

#' Testet Ereignisse auf Bedingungen (x vs. y)
#'
#' Stellt fuer zwei Mengen von Ereignissen fest, ob bestimmte zeitliche
#' Bedingungen zwischen ihnen erfuellt sind (z.B. ueberlappend, beinhaltend,
#' angrenzend, identisch). Die Ereignisse werden anhand ihrer
#' Start-/Endzeitpunkte beschrieben, die jeweils in den Spalten \emph{start} und
#' \emph{end} uebergeben werden.
#'
#' Da einige Tests nicht kommutativ sind und somit die Reihenfolge der
#' Ereignismengen entscheidend sein kann (z.B. bei \code{include}), kann mittels
#' Argument \code{swap_xy} die Reihenfolge veraendert werden.
#'
#' @param x Dataframe, der die zu testende Menge an Ereignissen beinhaltet.
#' @param y Dataframe, der die fuer die Tests herangezogene Menge an Ereignissen
#'   beinhaltet.
#' @param condition Funktion, die zum Testen der beiden Ereignismengen verwendet
#'   wird.
#' @param swap_xy Logischer Wert, ob Reihenfolge der Ereignismengen beim
#'   Berechnen der Testergebnisse zu \emph{y vs. x} veraendert wird (Default:
#'   \emph{FALSE}, d.h. Reihenfolge bleibt bei \emph{x vs. y}).
#' @param ... Weitere Argumente, die an Funktion \code{condition} durchgereicht
#'   werden.
#'
#' @return Logischer Vektor, welche Ereignisse von \code{x} die Bedingung
#'   erfuellen.
#'
#' @keywords internal
#'
#' @family Ereignisfunktionen
#' @seealso \code{\link{overlap}}, \code{\link{include}}, \code{\link{nearby}},
#'   \code{\link{equal}}
#'
#' @importFrom magrittr %>%
event_test <- function(x,
                       y,
                       condition,
                       swap_xy = FALSE,
                       ...) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          is.data.frame(y),
                          is.function(condition),
                          assertthat::is.flag(swap_xy))
  
  # Testfunktion
  testfun <- function(y_start, y_end, x, ...) {

    if (swap_xy) {

      condition(y_start, y_end,
                x$start, x$end, ...)

    } else {

      condition(x$start, x$end,
                y_start, y_end, ...)
    }
  }

  res <-
    purrr::map2(y$start, y$end,
                testfun,
                x = x,
                ...)

  if (length(res) > 0) {

    res <- purrr::reduce(res, `|`)

  } else {

    res <- rep(FALSE,
               ifelse(swap_xy, nrow(y), nrow(x)))
  }

  res
}

#' Paarweise identische Ereignisse?
#'
#' Stellt fuer zwei Mengen von Ereignissen fest, welche paarweise zeitlich
#' identisch sind. Die Ereignisse werden dabei durch ihre Start-/Endzeitpunkte
#' beschrieben.
#'
#' @param x_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.
#' @param x_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.
#' @param y_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.
#' @param y_end Vektor mit den Endzeitpunkten von Ereignisbenge \emph{y}.
#'
#' @return Logischer Vektor, welche Ereignisse aus \code{x} identisch sind mit
#'   Ereignis aus \code{y}.
#'
#' @family Testfunktionen
#'
#' @keywords internal
equal <- function(x_start,
                  x_end,
                  y_start,
                  y_end) {

  # Checkt Argumente
  assert_test(x_start,
              x_end,
              y_start,
              y_end)

  (x_start == y_start) &
    (x_end == y_end)
}

#' Paarweise beinhaltende Ereignisse?
#'
#' Stellt fuer zwei Mengen von Ereignissen \code{x} und \code{y} fest, welche
#' sich paarweise zeitlich beinhalten (d.h. welche Ereignisse aus \code{y}
#' liegen komplett innerhalb von Ereignissen aus \code{x}). Die Ereignisse
#' werden dabei durch ihre Start-/Endzeitpunkte beschrieben.
#'
#' @param x_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.
#' @param x_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.
#' @param y_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.
#' @param y_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.
#' @param is_strict Logischer Wert, ob eine minimale zeitliche Differenz
#'   zwischen den Ereignissen vorliegen muss, damit sie als einander beinhaltend
#'   gelten. Anderenfalls gelten Ereignisse mit identischem Start/Ende ebenfalls
#'   als beinhaltend (Default: \emph{FALSE}, d.h. die laxere Definition gilt).
#'
#' @return Logischer Vektor, welche Ereignisse aus \code{x} irgendwelche
#'   Ereignisse aus \code{y} beinhalten.
#'
#' @family Testfunktionen
#'
#' @keywords internal
include <- function(x_start,
                    x_end,
                    y_start,
                    y_end,
                    is_strict = FALSE) {

  # Checkt Argumente
  assert_test(x_start,
              x_end,
              y_start,
              y_end,
              is_strict)

  op1 <- ifelse(is_strict, `<`, `<=`)
  op2 <- ifelse(is_strict, `>`, `>=`)

  op1(x_start, y_start) &
    op2(x_end, y_end)
}

#' Paarweise angrenzende Ereignisse?
#'
#' Stellt fuer zwei Mengen von Ereignissen fest, welche paarweise zeitlich
#' aneinander angrenzen (d.h. aufeinander folgen). Die Ereignisse werden dabei
#' durch ihre Start-/Endzeitpunkte beschrieben.
#'
#' @param x_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.
#' @param x_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.
#' @param y_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.
#' @param y_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.
#' @param threshold_in_sec Numerischer Vektor (Laenge 1 oder 2), wie viele
#'   Sekunden zwei Ereignisse maximal auseinander liegen duerfen, um noch als
#'   angrenzend zu gelten (Toleranzbereich). Bei Laenge 2 wird der erste Wert
#'   als Toleranzbereich vor dem Ereignis und der zweite Wert als
#'   Toleranzbereich nache dem Ereignis betrachtet. Bei Laenge 1 ist der
#'   Toleranzbereich vor und nach dem Ereignis identisch. Sollte Toleranzbereich
#'   \emph{NA} sein, wird er ignoriert.
#' @param is_strict Logischer Wert, ob ein minimaler zeitlicher Abstand zwischen
#'   den Ereignissen vorliegen muss, damit sie als angrenzend gelten.
#'   Anderenfalls gelten Ereignisse ohne Abstand (d.h. \code{x_end == y_start}
#'   bzw \code{y_end == x_start} ebenfalls als angrenzend (Default:
#'   \emph{FALSE}, d.h. die laxere Definition gilt).
#'
#' @return Logischer Vektor, welche Ereignisse aus \code{x} an irgendwelche
#'   Ereignissen aus \code{y} angrenzen.
#'
#' @family Testfunktionen
#'
#' @keywords internal
nearby <- function(x_start,
                   x_end,
                   y_start,
                   y_end,
                   threshold_in_sec,
                   is_strict = FALSE) {

  # Checkt Argumente
  assert_test(x_start,
              x_end,
              y_start,
              y_end,
              is_strict,
              threshold_in_sec)

  # Korrigiert einstelligen Toleranzbereich
  if (length(threshold_in_sec) == 1) {

    threshold_in_sec <- rep(threshold_in_sec, 2)
  }

  op1 <- ifelse(is_strict, `<`, `<=`)
  op2 <- ifelse(is_strict, `>`, `>=`)

  (!is.na(threshold_in_sec[1]) &
     op1(y_end, x_start) &
     (y_end >= x_start - threshold_in_sec[1])) |
    (!is.na(threshold_in_sec[2]) &
       op2(y_start, x_end) &
       (y_start <= x_end + threshold_in_sec[2]))
}

#' Paarweise ueberlappende Ereignisse?
#'
#' Stellt fuer zwei Mengen von Ereignissen fest, welche sich paarweise zeitlich
#' ueberlappen. Die Ereignisse werden dabei durch ihre Start-/Endzeitpunkte
#' beschrieben.
#'
#' @param x_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{x}.
#' @param x_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{x}.
#' @param y_start Vektor mit den Startzeitpunkten von Ereignismenge \emph{y}.
#' @param y_end Vektor mit den Endzeitpunkten von Ereignismenge \emph{y}.
#' @param is_strict Logischer Wert, ob eine minimale zeitliche Schnittmenge
#'   zwischen den Ereignissen vorliegen muss, um als einander ueberlappend zu
#'   gelten. Anderenfalls gelten direkt angrenzende Ereignisse ebenfalls als
#'   ueberlappend (Default: \emph{TRUE}, d.h. die strenge Definition gilt).
#'
#' @return Logischer Vektor, welche Ereignisse aus \code{x} sich mit
#'   irgendwelchen Ereignissen aus \code{y} ueberlappen.
#'
#' @family Testfunktionen
#'
#' @keywords internal
overlap <- function(x_start,
                    x_end,
                    y_start,
                    y_end,
                    is_strict = TRUE) {

  # Checkt Argumente
  assert_test(x_start,
              x_end,
              y_start,
              y_end,
              is_strict)

  op1 <- ifelse(is_strict, `<`, `<=`)
  op2 <- ifelse(is_strict, `>`, `>=`)

  op1(y_start, x_end) &
    op2(y_end, x_start)
}

# Hilfsfunktionen --------------------------------------------------------------

#' Argumente-Check (Testfunktionen)
#'
#' Hilfsfunktion zum Ueberpruefen einer Reihe von Argumenten, ob sie mit
#' korrekten Typen und sinnvollen Werten uebergeben wurden. Sobald ein Verstoss
#' festgestellt wird, wird die Ausfuehrung unterbrochen. Da diese Test
#' wiederholt in den Ereignis-Testfunktionen auftreten, wurden sie in diese
#' Funktion ausgelagert.
#'
#' @inheritParams nearby
#'
#' @family Argument-Funktionen
#'
#' @keywords internal
assert_test <- function(x_start = 0,
                        x_end = 0,
                        y_start = 0,
                        y_end = 0,
                        is_strict = NA,
                        threshold_in_sec = 0) {

  # Checkt Argumente
  assertthat::assert_that(is_temporal(x_start),
                          is_temporal(x_end),
                          is_temporal(y_start),
                          is_temporal(y_end),
                          assertthat::is.flag(is_strict),
                          assertthat::is.number(threshold_in_sec),
                          length(threshold_in_sec) %in% 1:2)

  invisible(NULL)
}
