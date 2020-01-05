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

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          is.list(feat),
                          !is.null(names(feat)),
                          assertthat::is.flag(arg_global),
                          assertthat::is.flag(as_df),
                          assertthat::is.flag(as_wide),
                          assertthat::is.flag(.progress),
                          
                          length(x) > 0,
                          length(feat) > 0)

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

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x),
                          assertthat::not_empty(x),
                          assertthat::is.count(window_length),
                          assertthat::is.count(window_step),
                          assertthat::is.string(prefix),
                          assertthat::is.flag(.progress))

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

  # Checkt Argumente
  assertthat::assert_that(is.list(x),
                          assertthat::is.flag(add_id))

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

  # Checkt Argumente
  assertthat::assert_that(is.data.frame(x))

  if (nrow(x) == 0) {

    return(tibble::tibble())
  }

  tidyr::pivot_wider(x,
                     names_from = c("feature_set", "feature", "signal"),
                     names_sep = "-")
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

  # Checkt Argumente
  assertthat::assert_that(assertthat::is.string(fun),
                          is.data.frame(x),
                          arg1 %is_null_or% is.list,
                          arg2 %is_null_or% is.list,
                          assertthat::is.string(prefix))

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
