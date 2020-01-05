# Sampling von Ereignissen ------------------------------------------------------

#' Zufaellige (klassenbalancierte) Ereignisse
#'
#' Bestimmt in einem Zeitraum zufaellig eine feste Anzahl an Ereignissen
#' (Sampling). Diese Ereignisse beziehen sich auf eine Menge an Zielereignissen
#' (z.B. Stoerungen) und werden dabei in positive und negative Ereignisse
#' unterschieden. Zur Platzierung der Ereignisse wird auf die Funktion
#' \code{sample_random} zurueckgegriffen (\code{sample_balanced}) bzw. wird
#' sequentiell die Menge aller im Zeitraum moeglichen Ereignisse gebildet und
#' anschliessend daraus eine zufaellige Stichprobe gezogen
#' (\code{sample_balanced_seq}).
#'
#' Waehrend positive Ereignisse nahe vor einem Zielereignis liegen, befinden
#' sich negative Ereignisse ausserhalb dieser Bereiche (i.d.R. also weit vor
#' einem bzw. nach einem Zielereignis). Die Ereignisse werden so von der
#' Funktion platziert, dass das Verhaeltnis von positiven und negativen
#' Ereignissen gleichverteilt ist und somit eine Klassenbalance hergestellt
#' wird.
#'
#' Der Gesamtzeitraum wird durch einen Start- und einen Endzeitpunkt festgelegt.
#' In diesem Zeitraum koennen zusaetzliche Offtime-Intervalle definiert werden,
#' in denen kein Sampling stattfindet (d.h. erzeugte Ereignisse ueberlappen sich
#' nicht mit Offtimes). Neben der Ereignislaenge kann auch eine potentielle
#' zeitliche Ueberlappung zwischen Ereignissen gesteuert werden. Um
#' Klassenbalance herzustellen, wird weiterhin die Menge der Zielereignisse und
#' die Laenge des Zeitfensters vor den Zielereignissen benoetigt. Um zusaetzlich
#' bei \code{sample_random_seq} die zeitliche Ausrichtung der Ereignisse zu
#' beeinflussen, kann deren Erzeugungsrichtung sowie ihre Orientierung
#' bezueglich der Zielereignisse angepasst werden.
#'
#' @inheritParams sample_random
#' @param target_event Dataframe, der die Zielereignisse beinhaltet. Die
#'   Ereignisse sind anhand ihrer Start-/Endzeitpunkte beschrieben, die jeweils
#'   in den Spalten \emph{start} und \emph{end} uebergeben werden.
#' @param target_cut_in_sec Numerischer Wert mit der Laenge des Zeitfensters (in
#'   Sekunden), das vor jedem Zielereignis liegt und den Bereich der potentiell
#'   positiven Ereignisse umfasst.
#' @param include_tail Logischer Wert, ob Ereignisse nach dem letzten
#'   Zielereignis platziert werden koennen (Default: \emph{FALSE}).
#'
#' @return Benannte Liste mit Dataframes mit zufaelligen, klassenbalancierten
#'   Ereignissen aus Zeitraum.
#'
#' @family Sampling-Funktionen
#' @seealso \code{\link{sample_random}}
#'
#' @importFrom magrittr %>%
#' @export
sample_balanced <- function(n,
                            int_start,
                            int_end,
                            offtime = NULL,
                            target_event,
                            target_cut_in_sec,
                            event_length_in_sec,
                            event_overlap_in_sec = 0,
                            include_tail = FALSE,
                            .seed = NULL,
                            .max_run = 1E3) {

  # Checkt Argumente
  assert_sample(n = n,
                int_start = int_start,
                int_end = int_end,
                offtime = offtime,
                target_event = target_event,
                target_cut_in_sec = target_cut_in_sec,
                event_length_in_sec = event_length_in_sec,
                event_overlap_in_sec = event_overlap_in_sec,
                include_tail = include_tail,
                .seed = .seed,
                .max_run = .max_run)

  # Sorgt ggf. dafür, dass kein Ereignis ...
  # ...nach dem letzten Zielereignis kommen kann
  if (!include_tail) {

    int_end <- min(int_end, max(target_event$end))
  }

  # Offtimes für die Bestimmung der negativen Ereignisse
  offtime_neg <-
    target_event %>%
    dplyr::mutate(start = .data$start - target_cut_in_sec - event_length_in_sec)

  # Offtimes für die Bestimmung der positiven Ereignisse
  offtime_pos <-
    offtime_neg %>%
    event_invert(int_start, int_end) %>%
    dplyr::bind_rows(target_event)

  # Bestimmt positive und negative Ereignisse und gibt sie in einer Liste zurück
  list(offtime_pos, offtime_neg) %>%
    # Offtimes vorbereiten
    purrr::map(~ dplyr::bind_rows(., offtime) %>%
                 event_condense() %>%
                 event_merge()) %>%
    purrr::map2(c(ceiling(n / 2), floor(n / 2)),
                .,
                ~ sample_random(n = .x,
                                int_start = int_start,
                                int_end   = int_end,
                                offtime = .y,
                                event_length_in_sec  = event_length_in_sec,
                                event_overlap_in_sec = event_overlap_in_sec,
                                .seed = .seed,
                                .max_run = .max_run) %>%
                  arrange2(.data$start)) %>%
    stats::setNames(c("pos", "neg"))
}

#' @rdname sample_balanced
#'
#' @param from_start_to_end Logischer Wert, ob Ereignisse von \code{int_start}
#'   nach \code{int_end} erzeugt werden oder andersrum (Default: \emph{FALSE},
#'   d.h. es wird bei \code{int_end} begonnen).
#' @param from_target Logischer Wert, ob Ereignisse von den Zielereignissen
#'   weglaufend erzeugt werden oder zu ihnen hinlaufend (Default: \emph{TRUE},
#'   d.h. sie laufen von den Zielereignissen weg). Die Richtung ist abhaengig
#'   von \code{from_start_to_event}).
#'
#' @importFrom magrittr %>%
#' @export
sample_balanced_seq <- function(n,
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
                                .seed = NULL) {
  # Checkt Argumente
  assert_sample(n = n,
                int_start = int_start,
                int_end = int_end,
                offtime = offtime,
                target_event = target_event,
                target_cut_in_sec = target_cut_in_sec,
                event_length_in_sec = event_length_in_sec,
                event_overlap_in_sec = event_overlap_in_sec,
                from_start_to_end = from_start_to_end,
                from_target = from_target,
                include_tail = include_tail,
                .seed = .seed)

  # Seed für Reproduzierbarkeit
  set.seed(.seed %||% Sys.time())

  # Sorgt ggf. dafür, dass kein Ereignis ...
  # nach dem letzten Zielereignis kommen kann
  if (!include_tail) {

    int_end <- min(int_end, max(target_event$end))
  }

  offtime <-
    dplyr::bind_rows(offtime, target_event) %>%
    event_condense() %>%
    event_merge()

  postime <-
    target_event %>%
    dplyr::mutate(end   = .data$start,
                  start =
                    .data$start - target_cut_in_sec - event_length_in_sec) %>%
    dplyr::select(.data$start, .data$end)

  if (from_target) {

    res <- event_invert(target_event, int_start, int_end)

  } else {

    res <- tibble::tibble(start = int_start,
                          end   = int_end)
  }

  res <-
    purrr::map2_dfr(
      res$start, res$end,
      ~ seq_event(.x, .y,
                  event_length_in_sec  = event_length_in_sec,
                  event_overlap_in_sec = event_overlap_in_sec,
                  from_start_to_end    = from_start_to_end)) %>%
    arrange2(.data$start) %>%
    {
      dplyr::filter(., !event_test(., offtime,
                                   condition = overlap))
    } %>%
    {
      dplyr::mutate(.,
                    .group = ifelse(event_test(., postime,
                                               condition = include,
                                               swap_xy = TRUE),
                                    "pos", "neg"))
    } %>%
    split(.$.group)

  list(res$pos, res$neg) %>%
    purrr::map2(c(ceiling(n / 2), floor(n / 2)),
                ~ dplyr::select(.x, -.data$.group) %>%
                  dplyr::sample_n(size = .y) %>%
                  arrange2(.data$start)) %>%
    stats::setNames(c("pos", "neg"))
}

#' Zufaellige Ereignisse
#'
#' Bestimmt in einem Zeitraum zufaellig eine feste Anzahl an Ereignissen
#' (Sampling). Dabei wird wiederholt ein Ereignis an zufaelliger Position
#' innerhalb des Zeitraums platziert (\code{sample_random}) bzw. sequentiell die
#' Menge aller im Zeitraum moeglichen Ereignisse gebildet und anschliessend
#' daraus eine zufaellige Stichprobe gezogen (\code{sample_random_seq}).
#'
#' Der Gesamtzeitraum wird durch einen Start- und einen Endzeitpunkt festgelegt.
#' In diesem Zeitraum koennen zusaetzliche Offtime-Intervalle definiert werden,
#' in denen kein Sampling stattfindet (d.h. erzeugte Ereignisse ueberlappen sich
#' nicht mit Offtimes). Neben der Ereignislaenge kann auch eine potentielle
#' zeitliche Ueberlappung zwischen Ereignissen gesteuert werden.
#'
#' @param n Numerischer Wert mit Anzahl der zu samplenden Ereignisse
#' @param int_start POSIXct-Zeitstempel mit Startzeitpunkt des Sample-Zeitraums.
#' @param int_end POSIXct-Zeitstempel mit Endzeitpunkt des Sample-Zeitraums.
#' @param offtime Dataframe, der die Offtime-Ereignisse beinhaltet. Die
#'   Ereignisse sind anhand ihrer Start-/Endzeitpunkte beschrieben, die jeweils
#'   in den Spalten \emph{start} und \emph{end} uebergeben werden.
#' @param event_length_in_sec Numerischer Wert mit der Laenge der Ereignisse (in
#'   Sekunden).
#' @param event_overlap_in_sec Numerischer Wert mit der Laenge des Intervalls
#'   (in Sekunden), in dem sich zwei aufeinanderfolgende Ereignisse ueberlappen
#'   duerfen (Default: 0, d.h. keine Ueberlappung erlaubt).
#' @param .seed Numerischer Wert mit Seed-Wert, mit dem der Zufallsgenerator
#'   initialisiert wird. Darf auch \emph{NULL} sein, dann wird zur
#'   Initialisierung die aktuelle Systemzeit herangezogen (Default: \emph{NULL},
#'   d.h. bei jedem Aufruf sollten unterschiedliche Ergebnisse erzeugt werden).
#' @param .max_run Numerischer Wert mit Anzahl der Versuche, die je Ereignis
#'   unternommen werden sollen, um es im Zeitraum zu platzieren (Default: 1E3).
#'
#' @return Dataframe mit zufaelligen Ereignissen aus Zeitraum.
#'
#' @family Sampling-Funktionen
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @export
sample_random <- function(n,
                          int_start,
                          int_end,
                          offtime = NULL,
                          event_length_in_sec,
                          event_overlap_in_sec = 0,
                          .seed = NULL,
                          .max_run = 1E3) {

  # Checkt Argumente
  assert_sample(n = n,
                int_start = int_start,
                int_end = int_end,
                offtime = offtime,
                event_length_in_sec = event_length_in_sec,
                event_overlap_in_sec = event_overlap_in_sec,
                .seed = .seed,
                .max_run = .max_run)

  # Seed für Reproduzierbarkeit
  set.seed(.seed %||% Sys.time())

  res <-
    tibble::tibble(start = rep(lubridate::as_datetime(NA_integer_), n),
                   end   = .data$start)

  offtime <- offtime %||% res[0, ]

  for (i in seq_len(n)) {

    x <- random_event(int_start,
                      int_end,
                      offtime,
                      event_length_in_sec,
                      .max_run)

    if (!is.null(x)) {

      offtime <-
        x %>%
        dplyr::mutate(start = .data$start + event_overlap_in_sec,
                      end   = .data$end   - event_overlap_in_sec) %>%
        dplyr::filter(.data$start <= .data$end) %>%
        dplyr::bind_rows(offtime, .)

      res[i, ] <- x
    }
  }

  arrange2(res, .data$start)
}

#' @rdname sample_random
#'
#' @inheritParams seq_event
#'
#' @export
sample_random_seq <- function(n,
                              int_start,
                              int_end,
                              offtime = NULL,
                              event_length_in_sec,
                              event_overlap_in_sec = 0,
                              from_start_to_end = TRUE,
                              .seed = NULL) {

  # Checkt Argumente
  assert_sample(n = n,
                int_start = int_start,
                int_end = int_end,
                offtime = offtime,
                event_length_in_sec = event_length_in_sec,
                event_overlap_in_sec = event_overlap_in_sec,
                from_start_to_end = from_start_to_end,
                .seed = .seed)

  # Seed für Reproduzierbarkeit
  set.seed(.seed %||% Sys.time())

  res <- seq_event(int_start,
                   int_end,
                   event_length_in_sec,
                   event_overlap_in_sec,
                   from_start_to_end)

  if (!is.null(offtime)) {

    res <-
      dplyr::filter(res,
                    !event_test(res, offtime,
                                condition = overlap))
  }

  dplyr::sample_n(res, size = n) %>%
    arrange2(.data$start)
}

# Hilfsfunktionen --------------------------------------------------------------

#' Zufaelliges Ereignisse
#'
#' Hilfsfunktion zum zufaelligen Platzieren eines Ereignisses innerhalb eines
#' Zeitraums. Der Gesamtzeitraum wird durch einen Start- und einen Endzeitpunkt
#' festgelegt. In diesem Zeitraum koennen zusaetzliche Offtime-Intervalle
#' definiert werden, fuer die beim Platzieren sichergestellt wird, dass keine
#' Ueberlappung auftritt. D.h. sollte ein Ueberlappung auftreten, wird das
#' Ereignis verworfen und ein neuer Platzierungsversuch gestartet. Um an dieser
#' Stelle einen Deadlock zu vermeiden, gibt es eine Obergrenze fuer die
#' durchzufuehrenden Versuche.
#'
#' @inheritParams sample_random
#'
#' @return Dataframe mit zufaelligem Ereignis im Zeitraum bzw. \emph{NULL}, wenn
#'   Platzierung nicht erfolgreich.
#'
#' @family Sampling-Funktionen
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{sample_random}}
random_event <- function(int_start,
                         int_end,
                         offtime,
                         event_length_in_sec,
                         .max_run = 1E3) {

  # Checkt Argumente
  assertthat::assert_that(is_temporal(int_start, is_strict = TRUE),
                          is_temporal(int_end, is_strict = TRUE),
                          is.data.frame(offtime),
                          assertthat::is.number(event_length_in_sec),
                          assertthat::is.count(.max_run))

  # Mehrere Versuche
  for (i in seq_len(.max_run)) {

    # Zufaelliger Startpunkt
    start <-
      stats::runif(1,
                   min = int_start,
                   max = int_end - event_length_in_sec) %>%
      lubridate::as_datetime()

    # Keine Ueberlappung mit Offtime
    if (!any(overlap(start,
                     start + event_length_in_sec,
                     offtime$start,
                     offtime$end))) {

      return(tibble::tibble(start = start,
                            end   = start + event_length_in_sec))
    }
  }

  warning("Kein Ereignis in .max_run Versuchen gefunden.")

  return(NULL)
}

#' Alle Ereignisse
#'
#' Hilfsfunktion zum sequentiellen Erzeugen aller moeglichen Ereignisse in einem
#' gegebenen Zeitraum. Dabei kann neben dem Start- und Endzeitpunkt die
#' Ereignislaenge und die Ueberlappung sowie die Richtung der Sequenz veraendert
#' werden.
#'
#' @inheritParams sample_random
#' @param from_start_to_end Logischer Wert, ob Ereignisse von \code{int_start}
#'   nach \code{int_end} erzeugt werden oder andersrum (Default: \emph{TRUE},
#'   d.h. es wird bei \code{int_start} begonnen).
#'
#' @return Dataframe mit allen Ereignissen im Zeitraum.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
seq_event <- function(int_start,
                      int_end,
                      event_length_in_sec,
                      event_overlap_in_sec = 0,
                      from_start_to_end = TRUE) {

  # Checkt Argumente
  assert_sample(int_start = int_start,
                int_end = int_end,
                event_length_in_sec = event_length_in_sec,
                event_overlap_in_sec = event_overlap_in_sec,
                from_start_to_end = from_start_to_end)

  # Richtung der Sequenz
  if (from_start_to_end) {

    from <- int_start
    to   <- int_end - event_length_in_sec
    by   <- event_length_in_sec - event_overlap_in_sec

  } else {

    from <- int_end - event_length_in_sec
    to   <- int_start
    by   <- event_overlap_in_sec - event_length_in_sec
  }

  # Alle Ereignisse erzeugen
  tibble::tibble(start = seq(from, to, by),
                 end   = .data$start + event_length_in_sec) %>%
    arrange2(.data$start)
}

#' Argumente-Check (Sampling-Funktionen)
#'
#' Hilfsfunktion zum Ueberpruefen einer Reihe von Argumenten, ob sie mit
#' korrekten Typen und sinnvollen Werten uebergeben wurden. Sobald ein Verstoss
#' festgestellt wird, wird die Ausfuehrung unterbrochen. Da diese Test
#' wiederholt in den Sampling-Funktionen auftreten, wurden sie in diese Funktion
#' ausgelagert.
#'
#' @inheritParams sample_random
#' @inheritParams sample_balanced_seq
#'
#' @family Argument-Funktionen
#'
#' @keywords internal
assert_sample <- function(n = 1,
                          int_start = lubridate::origin,
                          int_end = lubridate::origin + 1,
                          offtime = NULL,
                          event_length_in_sec = 1,
                          event_overlap_in_sec = 0,
                          from_start_to_end = NA,
                          target_event = data.frame(),
                          target_cut_in_sec = 1,
                          include_tail = NA,
                          from_target = NA,
                          .seed = NULL,
                          .max_run = 1) {

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.count(n),
                          is_temporal(int_start, is_strict = TRUE),
                          is_temporal(int_end, is_strict = TRUE),
                          offtime %is_null_or% is.data.frame,
                          assertthat::is.number(event_length_in_sec),
                          assertthat::is.number(event_overlap_in_sec),
                          assertthat::is.flag(from_start_to_end),
                          is.data.frame(target_event),
                          assertthat::is.number(target_cut_in_sec),
                          assertthat::is.flag(include_tail),
                          assertthat::is.flag(from_target),
                          .seed %is_null_or% assertthat::is.number,
                          assertthat::is.count(.max_run),
                          
                          int_start < int_end,
                          event_length_in_sec > 0,
                          event_overlap_in_sec >= 0,
                          event_overlap_in_sec < event_length_in_sec,
                          target_cut_in_sec > 0)

  invisible(NULL)
}
