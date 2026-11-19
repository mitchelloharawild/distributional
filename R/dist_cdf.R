#' Cumulative distribution function distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The CDF distribution is a non-parametric distribution defined by the
#' cumulative probabilities at a set of values. This distribution is useful for
#' representing empirical distributions or elicited expert knowledge when only
#' the probability of being below some values is available. The distribution
#' uses linear interpolation between the given points and can be used to
#' approximate complex distributions that may not have simple parametric forms.
#'
#' The same distribution can be described by the values at a set of cumulative
#' probabilities rather than the cumulative probabilities at a set of values,
#' which is provided by [dist_quantile()]. Since the interpolation between the
#' given points is linear, and linear interpolation is its own inverse,
#' `dist_cdf(x, p)` and `dist_quantile(x, p)` describe exactly the same
#' distribution.
#'
#' @param x A list of values
#' @param cdf A list of cumulative probabilities (between 0 and 1) at `x`
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_cdf")`
#'
#'   In the following, let \eqn{X} be a CDF random variable defined by the
#'   cumulative probabilities \eqn{p_1, p_2, \ldots, p_n} at the values
#'   \eqn{x_1, x_2, \ldots, x_n} where \eqn{0 \le p_i \le 1}.
#'
#'   **Support**: \eqn{[\min(x_i), \max(x_i)]} if \eqn{\min(p_i) > 0} or
#'   \eqn{\max(p_i) < 1}, otherwise support is approximated from the
#'   specified cumulative probabilities.
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
#'   where \eqn{Q(u)} is a spline function interpolating the values.
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
#'       p_1 & \text{if } t < x_1 \\
#'       p_i + \frac{(t - x_i)(p_{i+1} - p_i)}{x_{i+1} - x_i} & \text{if } x_i \le t < x_{i+1} \\
#'       p_n & \text{if } t \ge x_n
#'     \end{cases}
#'   }{
#'     F(t) = p_i + (t - x_i)(p_{i+1} - p_i) / (x_{i+1} - x_i) for x_i ≤ t < x_{i+1}
#'   }
#'
#'   **Quantile function**: Defined by linear interpolation:
#'
#'   \deqn{
#'     Q(u) = x_i + \frac{(u - p_i)(x_{i+1} - x_i)}{p_{i+1} - p_i}
#'   }{
#'     Q(u) = x_i + (u - p_i)(x_{i+1} - x_i) / (p_{i+1} - p_i)
#'   }
#'
#'   for \eqn{p_i \le u \le p_{i+1}}.
#'
#' @seealso [dist_quantile()], [dist_density()]
#'
#' @examples
#' # A distribution given by its cumulative distribution function over a grid
#' at <- seq(-3, 3, by = 0.1)
#' dist <- dist_cdf(list(at), list(pnorm(at)))
#'
#' dist
#' cdf(dist, 1.96)
#' quantile(dist, 0.975)
#'
#' # The same distribution described by its quantiles
#' dist_quantile(list(at), list(pnorm(at)))
#'
#' @export
dist_cdf <- function(x, cdf){
  x <- as_list_of(x, .ptype = double())
  cdf <- as_list_of(cdf, .ptype = double())
  new_dist(x = x, cdf = cdf, class = "dist_cdf")
}

#' @export
format.dist_cdf <- function(x, ...){
  sprintf(
    "cdf[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_cdf <- function(x, at, ...){
  interp_density(x[["x"]], x[["cdf"]], at, ...)
}

#' @export
quantile.dist_cdf <- function(x, p, ...){
  interp_quantile(x[["x"]], x[["cdf"]], p)
}

#' @export
cdf.dist_cdf <- function(x, q, ...){
  interp_cdf(x[["x"]], x[["cdf"]], q)
}

#' @export
generate.dist_cdf <- function(x, times, ...){
  interp_generate(x[["x"]], x[["cdf"]], times)
}

#' @export
mean.dist_cdf <- function(x, ...) {
  interp_mean(x[["x"]], x[["cdf"]])
}

#' @export
support.dist_cdf <- function(x, ...) {
  interp_support(x[["x"]], x[["cdf"]])
}
