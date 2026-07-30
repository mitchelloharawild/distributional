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
#' The same distribution can also be described by the cumulative probabilities
#' at a set of values rather than the values at a set of cumulative
#' probabilities, which is provided by [dist_cdf()].
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
#' @seealso [dist_cdf()], [dist_density()]
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
  interp_density(x[["x"]], x[["quantile"]], at, ...)
}

#' @export
quantile.dist_quantile <- function(x, p, ...){
  interp_quantile(x[["x"]], x[["quantile"]], p)
}

#' @export
cdf.dist_quantile <- function(x, q, ...){
  interp_cdf(x[["x"]], x[["quantile"]], q)
}

#' @export
generate.dist_quantile <- function(x, times, ...){
  interp_generate(x[["x"]], x[["quantile"]], times)
}

#' @export
mean.dist_quantile <- function(x, ...) {
  interp_mean(x[["x"]], x[["quantile"]])
}

#' @export
support.dist_quantile <- function(x, ...) {
  interp_support(x[["x"]], x[["quantile"]])
}

# Core computations shared by dist_quantile() and dist_cdf(), which both
# describe a distribution by the values `x` at the cumulative probabilities `p`.
# Linear interpolation between the given points is its own inverse, so the same
# computations describe both parameterisations.

interp_density <- function(x, p, at, ...) {
  d <- density(
    interp_generate(x, p, 1000), from = min(at), to = max(at), ..., na.rm = TRUE
  )
  stats::approx(d$x, d$y, xout = at)$y
}

interp_quantile <- function(x, p, at) {
  out <- x[match(at, p)]
  out[is.na(out)] <- stats::approx(x = p, y = x, xout = at[is.na(out)])$y
  out
}

interp_cdf <- function(x, p, q) {
  stats::approx(x = x, y = p, xout = q)$y
}

interp_generate <- function(x, p, times) {
  stats::approx(p, x, xout = stats::runif(times, min(p), max(p)))$y
}

interp_mean <- function(x, p) {
  # Fit a spline to the quantile probabilities
  spline_fit <- stats::splinefun(p, x)

  # Use numerical integration to estimate the mean
  stats::integrate(spline_fit, lower = 0, upper = 1)$value
}

interp_support <- function(x, p) {
  new_support_region(
    list(vctrs::vec_init(x, n = 0L)),
    list(range(x)),
    list(!near(range(p), 0))
  )
}
