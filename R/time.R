# Funktionen zum Verarbeiten und Testen von Zeiten

#' Tage in Zeitraum?
#'
#' Bestimmt alle Tage, die in einem bestimmten Zeitraum liegen.
#'
#' @param int_start POSIXct-Zeitstempel mit Startzeitpunkt des Zeitraums.
#' @param int_end POSIXct-Zeitstempel mit Endzeitpunkt des Zeitraums.
#'
#' @return Date-Vektor mit den Tagen im Zeitraum.
#'
#' @importFrom magrittr %>%
#' @export
all_days <- function(int_start,
                     int_end) {

  # Checkt Argumente
  assertthat::assert_that(is_temporal(int_start, is_strict = TRUE),
                          is_temporal(int_end, is_strict = TRUE))

  seq(int_start,
      int_end,
      by = "1 day") %>%
    lubridate::as_date()
}

#' Wochenende?
#'
#' Bestimmt innerhalb eines Zeitraums die Anfangs- und Endzeitpunkte der
#' Wochenenden (Samstag 00:00 Uhr bis Montag 00:00 Uhr).
#'
#' @param int_start POSIXct-Zeitstempel mit Startzeitpunkt des Suchzeitraums.
#' @param int_end POSIXct-Zeitstempel mit Endzeitpunkt des Suchzeitraums.
#' @param is_truncated Logischer Wert, ob Wochenendzeitstempel an den Raendern
#'   des Suchzeitraums beschnitten werden sollen, damit keine Zeitstempel
#'   ausserhalb liegen (Default: \emph{TRUE}).
#'
#' @return Dataframe mit den gefundenen Wochenenden.
#'
#' @importFrom magrittr %>%
#' @export
all_weekends <- function(int_start,
                         int_end,
                         is_truncated = TRUE) {
  
  # Checkt Argumente
  assertthat::assert_that(is_temporal(int_start, is_strict = TRUE),
                          is_temporal(int_end, is_strict = TRUE),
                          assertthat::is.flag(is_truncated))

  res <-
    tibble::tibble(start =
                     seq(from = lubridate::floor_date(int_start,
                                                      unit = "week",
                                                      week_start = 6),
                         to = lubridate::ceiling_date(int_end,
                                                      unit = "week",
                                                      week_start = 6),
                         by = "1 week") %>%
                     lubridate::as_datetime(),
                   end = .data$start + lubridate::days(2))  %>%
    dplyr::filter(.data$end   > int_start,
                  .data$start < int_end)

  if (is_truncated) {

    res <- dplyr::mutate(res,
                         start = pmax(.data$start, int_start),
                         end   = pmin(.data$end, int_end))
  }

  res
}

#' Relative Zeitpunkte
#'
#' Bereinigt Zeitpunkte um die absolute Zeitinformation. Dabei erhaelt der erste
#' Zeitpunkt den Wert 0 und der zweite Zeitpunkt dann entsprechend den Offset
#' zum ersten Zeitpunkt usw.
#'
#' @param x POSIXct-Vektor mit den Zeitpunkten.
#' @param units String mit der zu verwendenden Zeiteinheit (Default:
#'   \emph{"secs"}, d.h. Sekunden).
#' @param POSIXct-Zeitstempel des Ursprungs (= Nullpunkt) (Default:
#'   \{emph{lubridate::origin}).
#' @param as_datetime Logischer Wert, ob Ergebnis als Datetime (Ursprung:
#'   1970-01-01) zurueckgegeben wird (Default: \emph{TRUE}).
#'
#' @return Vektor mit den relativen Zeitpunkten.
#'
#' @seealso \code{\link{origin}}
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
relative_time <- function(x,
                          units = "secs",
                          origin = lubridate::origin,
                          as_datetime = TRUE) {
  
  # Checkt Argumente
  assertthat::assert_that(is_temporal(x, is_strict = TRUE),
                          assertthat::is.string(units),
                          is_temporal(origin, is_strict = TRUE),
                          assertthat::is.flag(as_datetime))

  res <- diff_time(min(x), x, units = units)

  if (as_datetime) {

    res <- lubridate::as_datetime(origin + res)
  }

  res
}





