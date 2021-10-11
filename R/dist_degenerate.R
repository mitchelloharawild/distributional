#' The degenerate distribution
#'
#' \lifecycle{stable}
#'
#' The degenerate distribution takes a single value which is certain to be
#' observed. It takes a single parameter, which is the value that is observed
#' by the distribution.
#'
#' @param x The value of the distribution.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a degenerate random variable with value
#'   `x` = \eqn{k_0}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{k_0}
#'
#'   **Variance**: \eqn{0}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = 1 for x = k_0
#'   }{
#'     f(x) = 1 for x = k_0
#'   }
#'   \deqn{
#'     f(x) = 0 for x \neq k_0
#'   }{
#'     f(x) = 0 for x \neq k_0
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(x) = 0 for x < k_0
#'   }{
#'     F(x) = 0 for x < k_0
#'   }
#'   \deqn{
#'     F(x) = 1 for x \ge k_0
#'   }{
#'     F(x) = 1 for x \ge k_0
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{k_0 t}
#'   }{
#'     E(e^(tX)) = e^(k_0 t)
#'   }
#'
#' @examples
#' dist_degenerate(x = 1:5)
#'
#' @export
dist_degenerate <- function(x){
  vec_is(x, numeric())
  new_dist(x = x, class = "dist_degenerate")
}

#' @export
format.dist_degenerate <- function(x, ...){
  format(x[["x"]], ...)
}

#' @export
density.dist_degenerate <- function(x, at, ...){
  ifelse(at == x[["x"]], 1, 0)
}

#' @export
quantile.dist_degenerate <- function(x, p, ...){
  ifelse(p < 0 | p > 1, NaN, x[["x"]])
}

#' @export
cdf.dist_degenerate <- function(x, q, ...){
  ifelse(q >= x[["x"]], 1, 0)
}

#' @export
generate.dist_degenerate <- function(x, times, ...){
  rep(x[["x"]], times)
}

#' @export
mean.dist_degenerate <- function(x, ...){
  x[["x"]]
}

#' @export
covariance.dist_degenerate <- function(x, ...){
  0
}

#' @export
skewness.dist_degenerate <- function(x, ...) NA_real_

#' @export
kurtosis.dist_degenerate <- function(x, ...) NA_real_
