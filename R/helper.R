# Kleine (aber feine) Hilfsfunktionen.

#' Sortiert Dataframe (falls noetig)
#'
#' Ueberprueft, ob Dataframe bereits nach bestimmter Spalte sortiert ist. Falls
#' nicht, wird eine Sortierung durchgefuehrt.
#'
#' Erweiterte Variante von \code{\link{arrange}}.
#'
#' @param x Dataframe, der ggf. sortiert wird.
#' @param var Symbol oder String der Spalte, nach der ggf. sortiert wird.
#'
#' @return Sortierter Dataframe \code{x}.
#'
#' @seealso \code{\link{arrange}}
#'
#' @keywords internal
arrange2 <- function(x,
                     var) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

  col <- dplyr::pull(x, {{ var }})

  if (is.unsorted(col, na.rm = TRUE)) {

    x <- x[order(col), ]
  }

  x
}

#' Zeitdifferenz
#'
#' Berechnet die Zeitdifferenz zwischen zwei Vektoren in bestimmter Zeiteinheit.
#' Das Ergebnis wird als numerischer Wert zurueckgegeben.
#'
#' @param x POSIXct-Vektor mit Zeitpunkt-Menge \emph{x}.
#' @param y POSIXct-Vektor mit Zeitpunkt-Menge \emph{x}.
#' @param units String mit der zu verwendenden Zeiteinheit (Default:
#'   \emph{"secs"}, d.h. Sekunden).
#'
#' @return Numerischer Vektor mit Zeitdifferenzen.
#'
#' @seealso \code{\link{difftime}}
#'
#' @keywords internal
diff_time <- function(x,
                      y,
                      units = "secs") {

  # Checkt Argumente
  assertthat::assert_that(is_temporal(x, is_strict = TRUE),
                          is_temporal(y, is_strict = TRUE),
                          assertthat::is.string(units))

  difftime(y, x, units = units) %>%
    as.numeric()
}

#' Modus
#'
#' Bestimmt fuer einen Vektor (naeherungsweise) den am haeufigsten auftretenden
#' Wert (Modus). Dafuer wird mit der einfache Peak der Haeufigkeitsverteilung
#' herangezogen, die mit einem Histogramm beschrieben wird.
#'
#' @param x Numerischer Vektor, dessen Modus bestimmt wird.
#' @param binwidth Numerischer Wert mit der bei der Histogrammberechnung
#'   verwendeten Bin-Breite (Default: 1).
#'
#' @return Numerischer Wert mit dem Modus von \code{x}.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
find_mode <- function(x,
                      binwidth = 1) {

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.number(x),
                          assertthat::is.number(binwidth))

  breaks <- seq(min(x),
                max(x) + binwidth,
                by = binwidth)

  x %>%
    graphics::hist(breaks = breaks,
                   plot = FALSE) %>%
    {
      .$breaks[which.max(.$counts)]
    }
}

#' Erste Zeile pro Gruppe aus Dataframe
#'
#' Filtert aus einem Dataframe die jeweils gruppenweise erste Zeile heraus.
#' Dabei wird die urspruengliche (zeitliche) Sortierung beruecksichtigt, d.h.
#' pro sequenziellem Auftreten der Gruppenvariable wird eine Zeile ausgewaehlt.
#' Dieses Verhalten unterscheidet sich von \code{x \%>\% group_by(var) \%>\%
#' slice(1)}.
#'
#' Falls der Dataframe bereits nach einem zusaetzlichen Kriterium gruppiert ist
#' (z.B. nach Signal), bleibt die bestehende Gruppierung erhalten und wird um
#' die lokale Gruppierung erweitert.
#'
#' @param x Dataframe, der gefiltert wird und bereits in der gewuenschten
#'   Sortierung vorliegt. Kann ausserdem bereits gruppiert sein.
#' @param var Spaltenname (Symbol) der gruppierenden Variable.
#'
#' @return Dataframe mit den jeweils ersten Zeilen pro Gruppe aus \code{x}.
#'
#' @keywords internal
#'
#' @importFrom rlang .data
first_row_per <- function(x,
                          var) {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

  if (nrow(x) == 0) {

    return(x)
  }

  x %>%
    dplyr::mutate(.group =
                    ({{ var }} != dplyr::lag({{ var }})) %>%
                    tidyr::replace_na(TRUE) %>%
                    cumsum()) %>%
    dplyr::group_by(.data$.group,
                    add = dplyr::is_grouped_df(x)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$.group)
}

#' Konstanter Vektor?
#'
#' Ueberprueft, ob in einem Vektor nur ein einziger Wert vorkommt.
#'
#' @param x Vektor, der ueberprueft wird.
#' @param na.rm Logischer Wert, ob vorhandene \emph{NA}-Werte ignoriert werden
#'   (Default: \emph{TRUE}, d.h. sie werden nicht beruecksichtigt).
#'
#' @return Logischer Wert, ob Vektor \code{x} konstant ist.
#'
#' @keywords internal
is_constant <- function(x,
                        na.rm = TRUE) {

  # Checkt Argumente
  assertthat::assert_that(is.atomic(x),
                          assertthat::is.flag(na.rm))

  dplyr::n_distinct(x, na.rm = na.rm) <= 1
}

#' Selektiert Faktor-Level
#'
#' Waehlt eine Teilmenge der verfuegbaren Level eines Faktors aus. Die Selektion
#' wird hierbei mit einem regulaeren Ausdruck beschrieben.
#'
#' @param x Faktor, dessen Level ausgewaehlt werden.
#' @param pattern String mit regulaerem Ausdruck zur Selektion der Level
#'   (Default: \emph{".*"}, d.h. alle Level werden ausgewaehlt).
#'
#' @return Character-Vektor mit ausgewaehlten Levels aus \code{x}.
#'
#' @keywords internal
lvls_subset <- function(x,
                        pattern = ".*") {

  # Checkt Argumente
  assertthat::assert_that(is.factor(x),
                          assertthat::is.string(pattern))

  x %>%
    levels() %>%
    stringr::str_subset(pattern)
}

#' Durchnummerierte Namen
#'
#' Erzeugt \code{n} durchnummerierte Namen, die mit einem Praefix und einem
#' Suffix versehen sein koennen.
#'
#' @param n Numerischer Wert mit der Anzahl der zu erzeugenden Namen.
#' @param prefix String der den Namen vorangestellt wird (Default: \emph{""},
#'   d.h. vorher wird nichts angefuegt).
#' @param suffix String der den Namen hintenangestellt wird (Default: \emph{""},
#'   d.h. hinten wird nichts angefuegt).
#'
#' @return Character-Vektor der durchnummerierten Namen.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
name_seq <- function(n,
                     prefix = "",
                     suffix = "") {

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.count(n),
                          assertthat::is.string(prefix),
                          assertthat::is.string(suffix))

  seq_len(n) %>%
    stringr::str_pad(width = stringr::str_length(n),
                     pad   = "0") %>%
    paste0(prefix, ., suffix)
}

#' Eindeutige Namen
#'
#' Erzeugt fuer eine Ausgangsmenge von Namen \code{x} eine zugehoerige Zielmenge
#' mit eindeutigen Namen. Einmalig auftretende Namen werden dabei unveraendert
#' uebernommen. Mehrfach auftretende Namen werden durch Anhaengen einer
#' fortlaufenden Nummer eindeutig gemacht.
#'
#' @param x Character-Vektor mit den Ausgangsnamen.
#'
#' @return Character-Vektor der eindeutigen Namen.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
name_unique <- function(x) {

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.string(x))

  x %>%
    tibble::tibble(x = .) %>%
    dplyr::group_by(.data$x) %>%
    dplyr::mutate(n = dplyr::n(),
                  y = paste0(.data$x, ifelse(.data$n == 1,
                                             "", dplyr::row_number()))) %>%
    .$y
}

#' Normiert Vektor (Min-Max)
#'
#' Bildet den Wertebereich eines Vektors in einen neuen Wertebereich ab. Dabei
#' wird eine lineare Abbildung vorgenommen (evtl. vorhandene \emph{NA}-Werte
#' werden beibehalten). Das Mininum und Maximum des neuen Wertebereichs koennen
#' per Argument angepasst werden. Standardmaessig entspricht dies einer
#' 0-1-Normierung.
#'
#' @param x Vektor, der normiert wird. Kann logisch oder numerisch sein.
#' @param min Numerischer Wert, der das neue Minimum angibt (Default: 0).
#' @param max Numerischer Wert, der das neue Maximum angibt (Default: 1).
#'
#' @return Normierter Vektor \code{x}.
#'
#' @keywords internal
norm_range <- function(x,
                       min = 0,
                       max = 1) {

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.number(x) ||
                            assertthat::is.flag(x),
                          assertthat::is.number(min),
                          assertthat::is.number(max),
                          
                          min < max)

  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) *
    (max - min) + min
}

#' Spaltenumbenennung
#'
#' Benennt alle Spalten eines Dataframes um.
#'
#' @param x Dataframe, der umbenannt wird.
#' @param pattern_prefix String mit regulaerem Ausdruck zur Eingrenzung der
#'   Prefix-Position (Default: \emph{""}, d.h. es wird nichts eingegrenzt).
#' @param prefix String, der am Anfang der neuen Spaltennamen steht (Default:
#'   \emph{""}, d.h. es wird nichts vorangestellt).
#' @param pattern_suffix String mit regulaerem Ausdruck zur Eingrenzung der
#'   Suffix-Position (Default: \emph{""}, d.h. es wird nichts eingegrenzt).
#' @param suffix String, der am Ende der neuen Spaltennamen steht (Default:
#'   \emph{""}, d.h. es wird nichts angehaengt).
#'
#' @return Dataframe mit umbenannten Spalten.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
rename2 <- function(x,
                    pattern_prefix = "",
                    prefix = "",
                    pattern_suffix = "",
                    suffix = "") {

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::is.string(pattern_prefix),
                          assertthat::is.string(prefix),
                          assertthat::is.string(pattern_suffix),
                          assertthat::is.string(suffix))

  pattern <-
    c(prefix, suffix) %>%
    stats::setNames(c(paste0("^", pattern_prefix),
                      paste0(pattern_suffix, "$")))

  dplyr::rename_all(x,
                    ~ stringr::str_replace_all(., pattern))
}

#' NaN-Ersetzung
#'
#' Ersetzt in einem Vektor evtl. vorhandene \emph{NaN}-Werte.
#'
#' @param x Vektor, in dem ersetzt wird.
#' @param y Wert, mit dem \emph{NaN} ersetzt wird (Default: \emph{NA}).
#'
#' @return Ersetzter Vektor \code{x}.
#'
#' @keywords internal
replace_nan <- function(x,
                        y = NA) {

  # Checkt Argumente
  assertthat::assert_that(is.atomic(x),
                          assertthat::is.scalar(y))

  ifelse(is.nan(x), y, x)
}

#' Geaenderte Werte?
#'
#' Ueberprueft, in welchen Zeilen eines Dataframes oder Vektors sich die Werte
#' veraendert haben. Dabei wird spaltenweise die Differenz zweier
#' aufeinanderfolgender Werte betrachtet. Abschliessend werden die spaltenweisen
#' Ergebnisse zeilenweise zusammengefasst.
#'
#' @param x Dataframe oder Vektor, der ueberprueft wird.
#'
#' @return Logischer Vektor, in welchen Zeilen von \code{x} Wertaenderungen
#'   auftreten.
#'
#' @keywords internal
value_has_changed <- function(x) {
  
  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x) || is.atomic(x),
                          assertthat::not_empty(x))

  
  # Packt Vektor in Liste
  if (!is.data.frame(x)) {
    
    x <- list(x)
  }

  # Negierte tolerante Gleichheit
  not_near <- purrr::negate(dplyr::near)

  x %>%                            # Stellt spaltenweise Veraenderungen fest
    purrr::map(~ diff(.) %>%       # Differenzbildung aufeinanderfolgender Werte
                 not_near(0) %>%   # Weicht tolerant von 0 ab?
                 c(TRUE, .)) %>%   # Erster Wert ist immer verändert
    purrr::reduce(`|`)             # Ergebnisse zeilenweise zusammenfassen
}
