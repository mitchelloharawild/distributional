#' Plot a distribution
#'
#' \lifecycle{deprecated}
#'
#' This function is now defunct and can no longer be used. Instead consider using
#' the {ggdist} package to produce your own distribution plots. You can learn
#' more about how this plot can be produced using {ggdist} here:
#' https://mjskay.github.io/ggdist/articles/slabinterval.html
#'
#' @param x The distribution(s) to plot.
#' @param ... Unused.
#'
#' @keywords internal
#'
#' @export
autoplot.distribution <- function(x, ...){
  lifecycle::deprecate_stop("0.2.0", "distributional::autoplot.distribution()", details = "The autoplot() method for distributions have been replaced by geoms in the {ggdist} package.\nYou can produce a similar plot using ggdist::stat_dist_slab()")
}
