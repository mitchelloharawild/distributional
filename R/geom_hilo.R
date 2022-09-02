#' Ribbon plots for hilo intervals
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is deprecated in favour of the ggdist package and will removed
#' in a future release of this package. Consider using [ggdist::stat_lineribbon()] or
#' [ggdist::geom_lineribbon()] as an appropriate alternative.
#'
#' `geom_hilo_ribbon()` displays the interval defined by a hilo object. The
#' luminance of the shaded area indicates its confidence level. The shade colour
#' can be controlled by the `fill` aesthetic, however the luminance will be
#' overwritten to represent the confidence level.
#'
#' @inheritParams ggplot2::geom_ribbon
#'
#' @seealso
#'   [`geom_hilo_linerange()`] for discrete hilo intervals (vertical lines)
#'
#' @examples
#' dist <- dist_normal(1:3, 1:3)
#' library(ggplot2)
#' ggplot(
#'   data.frame(x = rep(1:3, 2), interval = c(hilo(dist, 80), hilo(dist, 95)))
#' ) +
#'   geom_hilo_ribbon(aes(x = x, hilo = interval))
#'
#' @export
geom_hilo_ribbon <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE, ...) {
  if(!is.null(mapping$hilo)){
    mapping$ymin <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$lower))
    mapping$ymax <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$upper))
    mapping$level <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$level))
  }
  ggplot2::layer(
    geom = GeomHiloRibbon, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomHiloRibbon <- ggplot2::ggproto(
  "GeomHiloRibbon", ggplot2::GeomRibbon,
  required_aes = c(ggplot2::GeomRibbon$required_aes),
  optional_aes = c("hilo", "level"),
  default_aes = ggplot2::aes(
    colour = "grey20", fill = "grey60", size = .5,
    linetype = 1, weight = 1, alpha = 1, level = NA
  ),
  draw_key = function(data, params, size) {
    lwd <- min(data$size, min(size) / 4)
    # Calculate and set colour
    fillcol <- darken_fill(data$fill, 80)

    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      gp = grid::gpar(
        col = fillcol,
        fill = scales::alpha(fillcol, data$alpha),
        lty = data$linetype,
        lwd = lwd * ggplot2::.pt,
        linejoin = "mitre"
      )
    )
  },

  draw_group = function(data, panel_scales, coord) {
    # Calculate colour
    data$fill <- darken_fill(data$fill, data$level)
    data$colour <- NA
    # Compute alpha transparency
    data$alpha <- grDevices::col2rgb(data$fill, alpha = TRUE)[4, ] / 255 * data$alpha

    # Create grobs
    grobs <- lapply(
      split(data, -data$level),
      ggplot2::GeomRibbon$draw_group,
      panel_scales, coord
    )
    ggplot2:::ggname("geom_hilo_ribbon", do.call(grid::grobTree, grobs))
  }
)

#' Line ranges for hilo intervals
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is deprecated in favour of the ggdist package and will removed
#' in a future release of this package. Consider using [ggdist::stat_slabinterval()] or
#' [ggdist::geom_slabinterval()] as an appropriate alternative.
#'
#' `geom_hilo_linerange()` displays the interval defined by a hilo object. The
#' luminance of the shaded area indicates its confidence level. The shade colour
#' can be controlled by the `fill` aesthetic, however the luminance will be
#' overwritten to represent the confidence level.
#'
#' @inheritParams ggplot2::geom_linerange
#'
#' @seealso
#'   [`geom_hilo_ribbon()`] for continuous hilo intervals (ribbons)
#'
#' @examples
#' dist <- dist_normal(1:3, 1:3)
#' library(ggplot2)
#' ggplot(
#'   data.frame(x = rep(1:3, 2), interval = c(hilo(dist, 80), hilo(dist, 95)))
#' ) +
#'   geom_hilo_linerange(aes(x = x, hilo = interval))
#'
#' @export
geom_hilo_linerange <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE, ...) {
  if(!is.null(mapping$hilo)){
    mapping$ymin <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$lower))
    mapping$ymax <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$upper))
    mapping$level <- expr(ggplot2::after_stat(vctrs::vec_data(hilo)$level))
  }
  ggplot2::layer(
    geom = GeomHiloLinerange, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomHiloLinerange <- ggplot2::ggproto(
  "GeomHiloLinerange", ggplot2::GeomLinerange,
  required_aes = c(ggplot2::GeomLinerange$required_aes),
  optional_aes = c("hilo", "level"),
  default_aes = ggplot2::aes(
    colour = "grey20", fill = "grey60", size = 2,
    linetype = 1, weight = 1, alpha = 1, level = NA
  ),
  draw_key = function(data, params, size) {
    lwd <- min(data$size, min(size) / 4)
    # Calculate and set colour
    fillcol <- darken_fill(data$colour, 80)

    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
      gp = grid::gpar(
        col = fillcol,
        fill = scales::alpha(fillcol, data$alpha),
        lty = data$linetype,
        lwd = lwd * ggplot2::.pt,
        linejoin = "mitre"
      )
    )
  },

  draw_panel = function(data, panel_scales, coord) {
    # Calculate colour
    data$colour <- darken_fill(data$colour, data$level)
    data$fill <- NA
    # Compute alpha transparency
    data$alpha <- grDevices::col2rgb(data$colour, alpha = TRUE)[4, ] / 255 * data$alpha

    # Create grobs
    grobs <- lapply(
      split(data, -data$level),
      ggplot2::GeomLinerange$draw_panel,
      panel_scales, coord
    )
    ggplot2:::ggname("geom_hilo_linerange", do.call(grid::grobTree, grobs))
  }
)

#' @importFrom farver convert_colour
#' @importFrom grDevices col2rgb
darken_fill <- function(col, prob) {
  col <- farver::convert_colour(t(grDevices::col2rgb(col)), "RGB", "HSL")
  n_prob <- length(unique(prob))
  col[,3] <- seq(90 - pmin((n_prob - 1)*10, 30), 90, length.out = n_prob)[match(prob, sort(unique(prob)))]
  col <- farver::convert_colour(col, "HSL", "RGB")
  col2hex(col)
}

#' @importFrom grDevices rgb
col2hex <- function(col){
  grDevices::rgb(col,  maxColorValue = 255)
}
