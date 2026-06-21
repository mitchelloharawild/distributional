#' Quantile distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Quantile distribution is a non-parametric distribution defined by
#' a set of values at specified quantile probabilities. This distribution is
#' useful for representing empirical distributions or elicited expert
#' knowledge when only quantile information is available. The distribution
#' uses linear interpolation between quantiles and can be used to
#' approximate complex distributions that may not have simple parametric forms.
#'
#' @param x A list of values
#' @param quantile A list of quantile probabilities (between 0 and 1)
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_quantile")`
#'
#'   In the following, let \eqn{X} be a Quantile random variable defined by
#'   values \eqn{x_1, x_2, \ldots, x_n} at quantile probabilities
#'   \eqn{q_1, q_2, \ldots, q_n} where \eqn{0 \le q_i \le 1}.
#'
#'   **Support**: \eqn{[\min(x_i), \max(x_i)]} if \eqn{\min(q_i) > 0} or
#'   \eqn{\max(q_i) < 1}, otherwise support is approximated from the
#'   specified quantiles.
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
#'   where \eqn{Q(u)} is a spline function interpolating the quantile values.
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
#'       q_1 & \text{if } t < x_1 \\
#'       q_i + \frac{(t - x_i)(q_{i+1} - q_i)}{x_{i+1} - x_i} & \text{if } x_i \le t < x_{i+1} \\
#'       q_n & \text{if } t \ge x_n
#'     \end{cases}
#'   }{
#'     F(t) = q_i + (t - x_i)(q_{i+1} - q_i) / (x_{i+1} - x_i) for x_i ≤ t < x_{i+1}
#'   }
#'
#'   **Quantile function**: Defined by linear interpolation:
#'
#'   \deqn{
#'     Q(u) = x_i + \frac{(u - q_i)(x_{i+1} - x_i)}{q_{i+1} - q_i}
#'   }{
#'     Q(u) = x_i + (u - q_i)(x_{i+1} - x_i) / (q_{i+1} - q_i)
#'   }
#'
#'   for \eqn{q_i \le u \le q_{i+1}}.
#'
#' @examples
#' dist <- dist_normal()
#' probs <- seq(0.01, 0.99, by = 0.01)
#' x <- vapply(probs, quantile, double(1L), x = dist)
#' dist_quantile(list(x), list(probs))
#' dist_percentile(list(x), list(probs * 100))
#'
#' @export
dist_quantile <- function(x, quantile){
  x <- as_list_of(x, .ptype = double())
  quantile <- as_list_of(quantile, .ptype = double())
  new_dist(x = x, quantile = quantile, class = "dist_quantile")
}

#' @rdname dist_quantile
#' @param percentile A list of percentiles (between 0 and 100)
#' @export
dist_percentile <- function(x, percentile){
  x <- as_list_of(x, .ptype = double())
  percentile <- as_list_of(percentile, .ptype = double())
  quantile <- lapply(percentile, function(p) p / 100)
  new_dist(x = x, quantile = quantile, class = "dist_quantile")
}

#' @export
format.dist_quantile <- function(x, ...){
  sprintf(
    "quantile[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_quantile <- function(x, at, ...){
  d <- density(generate(x, 1000), from = min(at), to = max(at), ..., na.rm=TRUE)
  stats::approx(d$x, d$y, xout = at)$y
}

#' @export
quantile.dist_quantile <- function(x, p, ...){
  out <- x[["x"]][match(p, x[["quantile"]])]
  out[is.na(out)] <- stats::approx(x = x[["quantile"]], y = x[["x"]], xout = p[is.na(out)])$y
  out
}

#' @export
cdf.dist_quantile <- function(x, q, ...){
  stats::approx(x = x[["x"]], y = x[["quantile"]], xout = q)$y
}

#' @export
generate.dist_quantile <- function(x, times, ...){
  stats::approx(x[["quantile"]], x[["x"]], xout = stats::runif(times, min(x[["quantile"]]), max(x[["quantile"]])))$y
}

#' @export
mean.dist_quantile <- function(x, ...) {
  # Fit a spline to the quantile probabilities
  spline_fit <- stats::splinefun(x[["quantile"]], x[["x"]])

  # Use numerical integration to estimate the mean
  stats::integrate(spline_fit, lower = 0, upper = 1)$value
}

#' @export
support.dist_quantile <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x[["x"]], n = 0L)),
    list(range(x[["x"]])),
    list(!near(range(x[["quantile"]]), 0))
  )
}
