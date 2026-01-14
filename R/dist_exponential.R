#' The Exponential Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Exponential distributions are frequently used to model waiting times and the
#' time between events in a Poisson process.
#'
#' @inheritParams stats::dexp
#'
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_exponential")`
#' 
#'
#'   In the following, let \eqn{X} be an Exponential random variable with
#'   parameter `rate` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{x \in [0, \infty)}{x >= 0}
#'
#'   **Mean**: \eqn{\frac{1}{\lambda}}{1 / \lambda}
#'
#'   **Variance**: \eqn{\frac{1}{\lambda^2}}{1 / \lambda^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \lambda e^{-\lambda x}
#'   }{
#'     f(x) = \lambda e^(-\lambda x)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - e^{-\lambda x}
#'   }{
#'     F(x) = 1 - e^(-\lambda x)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{\lambda}{\lambda - t}, \quad t < \lambda
#'   }{
#'     E(e^(tX)) = \lambda / (\lambda - t), for t < \lambda
#'   }
#'
#' @seealso [stats::Exponential]
#'
#' @examples
#' dist <- dist_exponential(rate = c(2, 1, 2/3))
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
#' @name dist_exponential
#' @export
dist_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate < 0)){
    abort("The rate parameter of an Exponential distribution must be non-negative.")
  }
  new_dist(rate = rate, class = "dist_exponential")
}

#' @export
format.dist_exponential <- function(x, digits = 2, ...){
  sprintf(
    "Exp(%s)",
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]])
}

#' @export
log_density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]], log = TRUE)
}

#' @export
quantile.dist_exponential <- function(x, p, ...){
  stats::qexp(p, x[["rate"]])
}

#' @export
cdf.dist_exponential <- function(x, q, ...){
  stats::pexp(q, x[["rate"]])
}

#' @export
generate.dist_exponential <- function(x, times, ...){
  stats::rexp(times, x[["rate"]])
}

#' @export
mean.dist_exponential <- function(x, ...){
  1/x[["rate"]]
}

#' @export
covariance.dist_exponential <- function(x, ...){
  1/x[["rate"]]^2
}

#' @export
skewness.dist_exponential <- function(x, ...) 2

#' @export
kurtosis.dist_exponential <- function(x, ...) 6
