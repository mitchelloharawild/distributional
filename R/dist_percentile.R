#' Percentile distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Percentile distribution is a non-parametric distribution defined by
#' a set of quantiles at specified percentile values. This distribution is
#' useful for representing empirical distributions or elicited expert
#' knowledge when only percentile information is available. The distribution
#' uses linear interpolation between percentiles and can be used to
#' approximate complex distributions that may not have simple parametric forms.
#'
#' @param x A list of values
#' @param percentile A list of percentiles
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_percentile")`
#'
#'   In the following, let \eqn{X} be a Percentile random variable defined by
#'   values \eqn{x_1, x_2, \ldots, x_n} at percentiles
#'   \eqn{p_1, p_2, \ldots, p_n} where \eqn{0 \le p_i \le 100}.
#'
#'   **Support**: \eqn{[\min(x_i), \max(x_i)]} if \eqn{\min(p_i) > 0} or
#'   \eqn{\max(p_i) < 100}, otherwise support is approximated from the
#'   specified percentiles.
#'
#'   **Mean**: Approximated numerically using spline interpolation and
#'   numerical integration:
#'
#'   \deqn{
#'     E(X) \approx \int_0^1 Q(u) du
#'   }{
#'     E(X) ≈ integral_0^1 Q(u) du
#'   }
#'
#'   where \eqn{Q(u)} is a spline function interpolating the percentile values.
#'
#'   **Variance**: Approximated numerically.
#'
#'   **Probability density function (p.d.f)**: Approximated numerically using
#'   kernel density estimation from generated samples.
#'
#'   **Cumulative distribution function (c.d.f)**: Defined by linear
#'   interpolation:
#'
#'   \deqn{
#'     F(t) = \begin{cases}
#'       p_1/100 & \text{if } t < x_1 \\
#'       p_i/100 + \frac{(t - x_i)(p_{i+1} - p_i)}{100(x_{i+1} - x_i)} & \text{if } x_i \le t < x_{i+1} \\
#'       p_n/100 & \text{if } t \ge x_n
#'     \end{cases}
#'   }{
#'     F(t) = p_i/100 + (t - x_i)(p_{i+1} - p_i) / (100(x_{i+1} - x_i)) for x_i ≤ t < x_{i+1}
#'   }
#'
#'   **Quantile function**: Defined by linear interpolation:
#'
#'   \deqn{
#'     Q(u) = x_i + \frac{(100u - p_i)(x_{i+1} - x_i)}{p_{i+1} - p_i}
#'   }{
#'     Q(u) = x_i + (100u - p_i)(x_{i+1} - x_i) / (p_{i+1} - p_i)
#'   }
#'
#'   for \eqn{p_i/100 \le u \le p_{i+1}/100}.
#'
#' @examples
#' dist <- dist_normal()
#' percentiles <- seq(0.01, 0.99, by = 0.01)
#' x <- vapply(percentiles, quantile, double(1L), x = dist)
#' dist_percentile(list(x), list(percentiles*100))
#'
#' @export
dist_percentile <- function(x, percentile){
  x <- as_list_of(x, .ptype = double())
  percentile <- as_list_of(percentile, .ptype = double())
  new_dist(x = x, percentile = percentile, class = "dist_percentile")
}

#' @export
format.dist_percentile <- function(x, ...){
  sprintf(
    "percentile[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_percentile <- function(x, at, ...){
  d <- density(generate(x, 1000), from = min(at), to = max(at), ..., na.rm=TRUE)
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_percentile <- function(x, p, ...){
  out <- x[["x"]][match(p, x[["percentile"]])]
  out[is.na(out)] <- stats::approx(x = x[["percentile"]]/100, y = x[["x"]], xout = p[is.na(out)])$y
  out
}

#' @export
cdf.dist_percentile <- function(x, q, ...){
  stats::approx(x = x[["x"]], y = x[["percentile"]]/100, xout = q)$y
}

#' @export
generate.dist_percentile <- function(x, times, ...){
  stats::approx(x[["percentile"]], x[["x"]], xout=stats::runif(times,min(x[["percentile"]]),max(x[["percentile"]])))$y
}

#' @export
mean.dist_percentile <- function(x, ...) {
  # assumes percentile is sorted
  # probs <- x[["percentile"]]/100
  # i <- seq_along(probs)
  #
  # weights <- (probs[pmin(i+1, length(probs))] - probs[pmax(i-1, 1)]) / 2
  # sum(x[["x"]] * weights)


  # Fit a spline to the percentiles
  spline_fit <- stats::splinefun(x[["percentile"]], x[["x"]])

  # Use numerical integration to estimate the mean
  stats::integrate(spline_fit, lower = 0, upper = 1)$value
}

#' @export
support.dist_percentile <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x[["x"]], n = 0L)),
    list(range(x[["x"]])),
    list(!near(range(x[["percentile"]]), 0))
  )
}
