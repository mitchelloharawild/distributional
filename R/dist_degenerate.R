#' The degenerate distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The degenerate distribution takes a single value which is certain to be
#' observed. It takes a single parameter, which is the value that is observed
#' by the distribution.
#'
#' @param x The value of the distribution (location parameter). Can be any real number.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_degenerate")`
#'
#'   In the following, let \eqn{X} be a degenerate random variable with value
#'   `x` = \eqn{k_0}.
#'
#'   **Support**: \eqn{\{k_0\}}, a single point
#'
#'   **Mean**: \eqn{\mu = k_0}
#'
#'   **Variance**: \eqn{\sigma^2 = 0}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = 1 \textrm{ for } x = k_0
#'   }{
#'     f(x) = 1 for x = k_0
#'   }
#'   \deqn{
#'     f(x) = 0 \textrm{ for } x \neq k_0
#'   }{
#'     f(x) = 0 for x != k_0
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(t) = 0 \textrm{ for } t < k_0
#'   }{
#'     F(t) = 0 for t < k_0
#'   }
#'   \deqn{
#'     F(t) = 1 \textrm{ for } t \ge k_0
#'   }{
#'     F(t) = 1 for t >= k_0
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
#'   **Skewness**: Undefined (NA)
#'
#'   **Excess Kurtosis**: Undefined (NA)
#'
#' @seealso [stats::Distributions]
#'
#' @examples
#' dist <- dist_degenerate(x = 1:5)
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
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
