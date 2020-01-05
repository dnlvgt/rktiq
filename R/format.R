# Funktionen zum Testen von Objekten.

# Format-Checks (einzelne Vektoren) --------------------------------------------

#' Erfuellt Vektor erweiterte Bedingung?
#'
#' Ueberprueft, ob ein Vektor entweder NULL ist oder eine zusaetzliche Bedingung
#' erfuellt.
#'
#' @param x Vektor, der ueberprueft wird.
#' @param condition Funktion, die zum erweiterten Testen von \code{x} verwendet
#'   wird.
#' @param ... Weitere Argumente, die an Funktion \code{condition} durchgereicht
#'   werden.
#'
#' @return Logischer Wert, ob \code{x} NULL ist oder die Bedingung erfuellt.
#'
#' @keywords internal
is_null_or <- function(x,
                       condition,
                       ...) {
  
  # Checkt Argumente
  assertthat::assert_that(is.atomic(x),
                          is.function(condition))
  
  is.null(x) ||
    condition(x, ...)
}

#' @rdname is_null_or
#'
#' @keywords internal
`%is_null_or%` <- function(x,
                           condition) {
  
  is_null_or(x, condition)
}

#' Vektor mit Zeitinformationen?
#'
#' Ueberprueft, ob in einem Vektor Zeitinformationen vorliegen.
#'
#' @param x Vektor, der ueberprueft wird.
#' @param is_strict Logischer Wert, ob streng ausschliesslich auf
#'   \code{is.POSIXct} getestet wird (Default: \emph{FALSE}).
#'
#' @return Logischer Wert, ob \code{x} ein Vektor mit Zeitinformationen ist.
#'
#' @keywords internal
is_temporal <- function(x,
                        is_strict = FALSE) {

  # Checkt Argumente
  assertthat::assert_that(is.atomic(x),
                          assertthat::is.flag(is_strict))
  
  res <- lubridate::is.POSIXct(x)
  
  if (!is_strict) {
    
    res <-
      res ||
      is.numeric(x) ||
      hms::is_hms(x)
  }
  
  res
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

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

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
#' Ueberprueft, ob ein Dataframe ein langer oder breiter Tiqqle ist.
#'
#' @param x Dataframe (\code{\link{tibble}}), dessen Format ueberprueft wird.
#'
#' @return Logischer Wert, ob Dataframe \code{x} ein valider Tiqqle ist.
#'
#' @family Format-Checks
#'
#' @export
is_valid <- function(x) {

  # Checkt Argumente
  assertthat::assert_that(tibble::is_tibble(x))
  
  UseMethod("is_valid")
}

#' @export
is_valid.tiqqle_long <- function(x) {

  is_tiqqle(x) &&
    is_long(x)
}

#' @export
is_valid.tiqqle_wide <- function(x) {

  is_tiqqle(x) &&
    is_wide(x)
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

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

  ncol(x) > 1 &&
    names(x)[1] == "time" &&
    is_temporal(x$time) &&
    purrr::every(dplyr::select(x, -.data$time),
                 is.numeric)
}

