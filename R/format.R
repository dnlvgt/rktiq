# Funktionen zum Formatieren und Testen von Signal-Dataframes.

# Format-Checks (einzelne Spalten) ---------------------------------------------

#' Vektor mit Zeitinformationen?
#'
#' Ueberprueft, ob in einem Vektor Zeitinformationen vorliegen.
#'
#' @param x Vektor, der ueberprueft wird.
#'
#' @return Logischer Wert, ob \code{x} ein Vektor mit Zeitinformationen ist.
#'
#' @keywords internal
is_temporal <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.atomic(x))

  is.numeric(x) ||
    lubridate::is.POSIXct(x) ||
    hms::is.hms(x)
}

# Format-Checks (einzelne DF) --------------------------------------------------

#' Langer Dataframe?
#'
#' Ueberprueft, ob ein Dataframe im langen Format vorliegt. Das lange Format
#' besitzt hierbei drei Spalten: \emph{time} (Zeitstempel), \emph{signal}
#' (Signalname als Faktor oder String) und \emph{value} (numerischer
#' Signalwert).
#'
#' @inheritParams is_tiqqle
#'
#' @return Logischer Wert, ob Dataframe \code{x} im langen Format vorliegt.
#'
#' @family Format-Checks
#'
#' @export
is_long <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

  ncol(x) == 3 &&
    all(names(x) == c("time", "signal", "value")) &&
    is_temporal(x$time) &&
    (is.factor(x$signal) || is.character(x$signal)) &&
    is.numeric(x$value)
}

#' Tiqqle?
#'
#' Ueberprueft, ob ein Dataframe ein langer oder breiter Tiqqle ist.
#'
#' @param x Dataframe, dessen Format ueberprueft wird.
#'
#' @return Logischer Wert, ob Dataframe \code{x} ein Tiqqle ist.
#'
#' @family Format-Checks
#'
#' @export
is_tiqqle <- function(x) {

  inherits(x, "tbl_df") &&
    (inherits(x, "tiqqle_long") || inherits(x, "tiqqle_wide"))
}

#' Valider Tiqqle?
#'
#' Ueberprueft, ob ein Dataframe ein langer oder breiter Tiqqle ist und ggf.
#' Daten enthaelt.
#'
#' @param x Dataframe (\code{\link{tibble}}), dessen Format ueberprueft wird.
#' @param is_empty Logischer Wert, ob Dataframe auch leer sein darf, um als
#'   valide zu gelten (Default: \emph{FALSE}, d.h. er muss Daten enthalten).
#'
#' @return Logischer Wert, ob Dataframe \code{x} ein valider Tiqqle ist.
#'
#' @family Format-Checks
#'
#' @export
is_valid <- function(x,
                     is_empty = FALSE) {

  UseMethod("is_valid")
}

#' @export
is_valid.tiqqle_long <- function(x,
                                 is_empty = FALSE) {

  # Checkt die Datentypen der Argumente
  stopifnot(tibble::is_tibble(x),
            is.logical(is_empty))

  is_tiqqle(x) &&
    is_long(x) &&
    (is_empty || nrow(x) > 0)
}

#' @export
is_valid.tiqqle_wide <- function(x,
                                 is_empty = FALSE) {

  # Checkt die Datentypen der Argumente
  stopifnot(tibble::is_tibble(x),
            is.logical(is_empty))

  is_tiqqle(x) &&
    is_wide(x) &&
    (is_empty || nrow(x) > 0)
}

#' Breiter Dataframe?
#'
#' Ueberprueft, ob ein Dataframe im breiten Format vorliegt. Das breite Format
#' besitzt hierbei eine Spalte \emph{time} (Zeitstempel) und fuer jedes Signal
#' eine weitere numerische Spalte mit dem Namen des Signals.
#'
#' @inheritParams is_tiqqle
#'
#' @return Logischer Wert, ob Dataframe \code{x} im breiten Format vorliegt.
#'
#' @family Format-Checks
#'
#' @export
is_wide <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(is.data.frame(x))

  ncol(x) > 1 &&
    names(x)[1] == "time" &&
    is_temporal(x$time) &&
    purrr::every(dplyr::select(x, -.data$time),
                 is.numeric)
}

# Format-Checks (mehrere DFs) --------------------------------------------------

#' Lange Dataframes?
#'
#' Ueberprueft, ob in einer Liste von Dataframes alle im langen Format
#' vorliegen. Das lange Format besitzt hierbei drei Spalten: \emph{time}
#' (Zeitstempel), \emph{signal} (Signalname als Faktor) und \emph{value}
#' (numerischer Signalwert).
#'
#' @inheritParams all_are_valid
#'
#' @return Logischer Wert, ob alle Tiqqles in Liste \code{x} im langen Format
#'   vorliegen.
#'
#' @family Format-Checks
#'
#' @keywords internal
all_are_long <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(rlang::is_vector(x))

  purrr::every(x, is_long)
}

#' Valide Dataframes?
#'
#' Ueberprueft, ob in einer Liste von Dataframes alle lange oder breite Tiqqle
#' sind und Daten enthalten.
#'
#' @param x Liste mit Tiqqles, deren Format ueberprueft wird.
#'
#' @return Logischer Wert, ob alle Tiqqles in Liste \code{x} valide sind.
#'
#' @family Format-Checks
#'
#' @keywords internal
all_are_valid <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(rlang::is_vector(x))

  purrr::every(x, is_valid)
}

#' Breite Dataframes?
#'
#' Ueberprueft, ob in einer Liste von Dataframes alle im breiten Format
#' vorliegen. Das breite Format besitzt hierbei eine Spalte \emph{time}
#' (Zeitstempel) und fuer jedes Signal eine weitere numerische Spalte mit dem
#' Namen des Signals.
#'
#' @inheritParams all_are_valid
#'
#' @return Logischer Wert, ob alle Tiqqles in Liste \code{x} im breiten Format
#'   vorliegen.
#'
#' @family Format-Checks
#'
#' @keywords internal
all_are_wide <- function(x) {

  # Checkt die Datentypen der Argumente
  stopifnot(rlang::is_vector(x))

  purrr::every(x, is_wide)
}
