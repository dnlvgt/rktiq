# Funktionen zum Visualisieren von Signal-Dataframes.

#' Schachbrett-Signalplot
#'
#' Plottet Signale als Schachbrett. Dabei verlaeuft die Zeit auf der X-Achse und
#' die Signale auf der Y-Achse. An den entsprechenden X-Y-Kreuzungspunkten
#' werden die Signalwerte als farbige Rechtecke angezeigt. Diese entspricht
#' einer Heatmap-Darstellung.
#'
#' Die Reihenfolge der Signale (Y-Achse) laesst sich ueber die Level der
#' Signalspalte festlegen.
#'
#' @inheritParams plot_dummy
#'
#' @return Plot-Objekt (\code{ggplot2}) mit der Schachbrett-Darstellung.
#'
#' @family Plotfunktionen
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_checkered <- function(x,
                           drop_level = TRUE,
                           has_legend = FALSE) {

  # Checkt die Datentypen der Argumente
  stopifnot(is_valid.tiqqle_long(x),
            is.logical(drop_level),
            is.logical(has_legend))

  if (drop_level) {

    x <- droplevels(x)
  }

  x <-
    x %>%
    # EVTL: parallel moeglich
    dplyr::group_by(.data$signal) %>%
    dplyr::mutate(.duration =
                    diff_time(.data$time, dplyr::lead(.data$time))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$.duration))

  p <-
    ggplot2::ggplot(x) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$time,
                                    xmax = .data$time + .data$.duration,
                                    ymin = as.numeric(.data$signal) - 0.5,
                                    ymax = as.numeric(.data$signal) + 0.5,
                                    fill = .data$value)) +
    ggplot2::scale_x_datetime(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(x$signal)),
                                labels = levels(x$signal),
                                expand = c(0, 0)) +
    ggplot2::labs(x = "time",
                  y = "signal",
                  fill = "value")

  if (!has_legend) {

    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

#' Facet-Signalplot
#'
#' Plottet Signale als Liniengraphen in unterschiedliche Facets. Innerhalb einer
#' Facet ist die Zeit auf der X-Achse und die Signalwerte auf der Y-Achse
#' dargstellt. Die Facets werden dabei in einem bestimmten Raster angeordnet.
#'
#' Die Reihenfolge der Signale (Facets) laesst sich ueber die Level der
#' Signalspalte festlegen.
#'
#' @inheritParams plot_dummy
#' @param nrow Numerischer Wert mit der Anzahl der Facet-Rasterzeilen (Default:
#'   \emph{NULL}, d.h. wird automatisch angepasst).
#' @param ncol Numerischer Wert mit der Anzahl der Facet-Rasterspalten (Default:
#'   1, d.h. Raster mit einer Spalte).
#'
#' @return Plot-Objekt (\code{ggplot2}) mit der Facet-Zeitreihendarstellung.
#'
#' @family Plotfunktionen
#'
#' @export
plot_spread <- function(x,
                        nrow = NULL,
                        ncol = 1,
                        drop_level = TRUE) {

  # Checkt die Datentypen der Argumente
  stopifnot(is_valid.tiqqle_long(x),
            is.null(nrow) || is.numeric(nrow),
            is.null(ncol) || is.numeric(ncol),
            is.logical(drop_level))

  if (drop_level) {

    x <- droplevels(x)
  }

  ggplot2::ggplot(x) +
    ggplot2::geom_step(ggplot2::aes(.data$time, .data$value)) +
    ggplot2::facet_wrap(~ .data$signal,
                        nrow = nrow,
                        ncol = ncol,
                        scales = "free_y") +
    ggplot2::labs(x = "time",
                  y = "value")
}

#' Signalplot
#'
#' Plottet Signale als Liniengraphen ueberlagert in einem Facet. Dabei ist die
#' Zeit auf der X-Achse und die Signalwerte auf der Y-Achse dargstellt. Die
#' Signale werden dabei farbig kodiert.
#'
#' Die Reihenfolge der Signale (Farben) laesst sich ueber die Level der
#' Signalspalte festlegen.
#'
#' @inheritParams plot_dummy
#'
#' @return Plot-Objekt (\code{ggplot2}) mit der ueberlagerten
#'   Zeitreihendarstellung.
#'
#' @family Plotfunktionen
#'
#' @export
plot_stacked <- function(x,
                         drop_level = TRUE,
                         has_legend = FALSE) {

  # Checkt die Datentypen der Argumente
  stopifnot(is_valid.tiqqle_long(x),
            is.logical(drop_level),
            is.logical(has_legend))

  if (drop_level) {

    x <- droplevels(x)
  }

  p <-
    ggplot2::ggplot(x) +
    ggplot2::geom_step(ggplot2::aes(.data$time, .data$value,
                                    color = .data$signal)) +
    ggplot2::labs(x = "time",
                  y = "value",
                  color = "signal")

  if (!has_legend) {

    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}

# Hilfsfunktionen --------------------------------------------------------------

#' Doku-Dummyfunktion
#'
#' Hilfsfunktion zu Dokumentationszwecken.
#'
#' @name plot_dummy
#' @param x Tiqqle im langen Format.
#' @param drop_level Logischer Wert, ob nicht vorhandene Signale ausgeblendet
#'   werden (Default: \emph{TRUE}).
#' @param has_legend Logischer Wert, ob eine Legende angezeigt wird (Default:
#'   \emph{FALSE}, d.h. keine Legende).
#'
#' @keywords internal
NULL
