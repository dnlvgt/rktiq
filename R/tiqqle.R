# Eigenes S3-Objekt (tiqqle)

# Konstruktoren ----------------------------------------------------------------

#' Dataframe zu Tiqqle
#'
#' Wandelt einen bestehenden Dataframe in einen Tiqqle um.
#'
#' @param x Dataframe, der konvertiert wird.
#' @param format String, welches Format der konvertierte Tiqqle hat. Erlaubte
#'   Werte: \emph{"long"} oder \emph{"wide"} (Default: \emph{"long"}, d.h.
#'   langes Format).
#'
#' @return Tiqqle im gewuenschten Format.
#'
#' @family Tiqqle-Funktionen
#'
#' @importFrom magrittr %>%
#' @export
as_tiqqle <- function(x,
                      format = "long") {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.string(format),
                          
                          format %in% c("long", "wide"))

  x <- tibble::as_tibble(x)

  # Klasse setzen
  class(x) <-
    paste0("tiqqle_", format) %>%
    c("tbl_df", "tbl", "data.frame")

  x
}

#' Tiqqle: ein Tibble
#'
#' Erzeugt einen Tiqqle-Dataframe im langen oder breiten Format. Falls kein
#' bereits existierender Dataframe uebergeben wird, wird ein leerer Tiqqle
#' erzeugt. Ansonsten wird er mit \code{as_tiqqle} konvertiert.
#'
#' @param x Dataframe, der konvertiert wird (Default: \emph{NULL}, d.h. es wird
#'   ein leerer Tiqqle erzeugt).
#' @param format String, welches Format der erzeugte Tiqqle hat. Erlaubte Werte:
#'   \emph{"long"} oder \emph{"wide"} (Default: \emph{"long"}, d.h. langes
#'   Format).
#'
#' @return Tiqqle im gewuenschten Format.
#'
#' @family Tiqqle-Funktionen
#'
#' @export
tiqqle <- function(x = NULL,
                   format = "long") {
  
  # Checkt Argumente, Pt. 1
  assertthat::assert_that(x %is_null_or% is.data.frame,
                          assertthat::is.string(format),
                          
                          format %in% c("long", "wide"))
  
  if (is.null(x)) {
    
    if (format == "long") {
      
      x <- tibble::tibble(time   = as.POSIXct(integer(), origin = NA),
                          signal = factor(),
                          value  = numeric())
      
    } else if (format == "wide") {
      
      x <- tibble::tibble(time   = as.POSIXct(integer(), origin = NA),
                          signal = numeric())
    }
    
  }
  
  # Checkt Argumente, Pt. 2
  assertthat::assert_that(format == "long" && is_long(x),
                          format == "wide" && is_wide(x))

  as_tiqqle(x, format)
}

# Format-Konverter -------------------------------------------------------------

#' Breiten Tiqqle in langes Format
#'
#' Ein Dataframe wird vom breiten ins lange Format umgewandelt. Das breite
#' Format besitzt hierbei eine Spalte \emph{time} (Zeitstempel) und fuer jedes
#' Signal eine weitere numerische Spalte mit dem Namen des Signals. Das
#' resultierende lange Format besitzt drei Spalten: \emph{time} (Zeitstempel),
#' \emph{signal} (Signalname) und \emph{value} (Signalwert).
#'
#' @param x Tiqqle im breiten Format, der konvertiert wird.
#' @param remove_na Logischer Wert, ob die \emph{NA}-Eintraege im resultierenden
#'   langen Tiqqle entfernt werden (Default: \emph{FALSE}, d.h. \emph{NA}-Werte
#'   werden nicht entfernt).
#'
#' @return Tiqqle im langen Format.
#'
#' @family Tiqqle-Funktionen
#'
#' @importFrom magrittr %>%
#' @export
as_long <- function(x,
                    remove_na = FALSE) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::is.flag(remove_na))

  if (is_long(x)) {

    return(x)
  }

  tidyr::pivot_longer(x,
                      cols = -"time",
                      names_to = "signal",
                      values_drop_na = remove_na,
                      names_ptypes = list(signal = factor())) %>%
    as_tiqqle("long")
}

#' Langen Tiqqle in breites Format
#'
#' Ein Dataframe wird vom langen ins breite Format umgewandelt. Das lange Format
#' besitzt hierbei drei Spalten: \emph{time} (Zeitstempel), \emph{signal}
#' (Signalname) und \emph{value} (Signalwert). Das resultierende breite Format
#' besitzt eine Spalte \emph{time} (Zeitstempel) und fuer jedes Signal eine
#' weitere numerische Spalte mit dem Namen des Signals.
#'
#' @param x Tiqqle im langen Format, der konvertiert wird.
#' @param fill_gap Logischer Wert, ob die \emph{NA}-Luecken im resultierenden
#'   breiten Tiqqle mit \code{tidyr::fill} aufgefuellt werden (Default:
#'   \emph{FALSE}, d.h. Luecken werden nicht aufgefuellt).
#'
#' @return Tiqqle im breiten Format.
#'
#' @family Tiqqle-Funktionen
#'
#' @export
as_wide <- function(x,
                    fill_gap = FALSE) {
  
  # Checkt Argumente
  assertthat::assert_that(is_valid(x),
                          assertthat::is.flag(fill_gap))

  if (is_wide(x)) {

    return(x)
  }

  res <- tidyr::pivot_wider(x,
                            names_from = "signal")

  if (fill_gap) {

    res <- tidyr::fill(res, -"time")
  }

  as_tiqqle(res, "wide")
}
