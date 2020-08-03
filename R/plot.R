#' Plot a distribution
#'
#' \lifecycle{deprecated}
#'
#' Visualise distribution(s) by plotting its probability density function
#' ([`density()`]) or cumulative distribution function ([`cdf()`]).
#' Note: This function currently only works for continuous distributions.
#'
#' @param x The distribution(s) to plot.
#' @param type The type of plot to make (must be either `"pdf"` or `"cdf"`).
#' @param n The resolution (number of points) used to display the distribution.
#' @param quantile_range The range of the distribution (specified as quantiles).
#' @param ... Unused.
#'
#' @examples
#' library(ggplot2)
#' dist <- c(dist_normal(mu = 0, sigma = 1), dist_student_t(df = 3))
#' autoplot(dist, type = "pdf")
#' autoplot(dist, type = "cdf")
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.distribution <- function(x, type = c("pdf", "cdf"), n = 100,
                                  quantile_range = c(0.001, 0.999), ...){
  lifecycle::deprecate_warn("0.2.0", "distributional::autoplot.distribution()", details = "The autoplot() method for distributions will be replaced by the {ggdist} package in an upcoming release.")

  type <- match.arg(type)
  lower <- quantile(x, quantile_range[1])
  upper <- quantile(x, quantile_range[2])
  # x_pos <- mapply(function(l, u) seq(l, u, length.out = n), l = lower, u = upper, SIMPLIFY = FALSE)
  x_pos <- rep(list(seq(min(lower), max(upper), length.out = n)), length(x))
  if(type == "pdf"){
    y_val <- mapply(
      function(d, x){
        vapply(x, function(at) density(d, at), numeric(1L))
      }, d = x, x = x_pos, SIMPLIFY = FALSE)
  }
  else if(type == "cdf"){
    y_val <- mapply(
      function(d, x){
        vapply(x, function(q) cdf(d, q), numeric(1L))
      }, d = x, x = x_pos, SIMPLIFY = FALSE)
  }
  x_pos <- do.call("vec_c", x_pos)
  y_val <- do.call("vec_c", y_val)

  ggplot2::ggplot(
    data.frame(x = x_pos, y = y_val, distribution = rep(format(x), each = n), id = rep(seq_along(x), each = n)),
    ggplot2::aes_string(x = "x", y = "y", colour = "distribution", group = "id")
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(y = type)
}
