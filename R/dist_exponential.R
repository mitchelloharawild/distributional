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
quantile.dist_exponential <- function(x, p, ..., tail = "lower"){
  stats::qexp(p, x[["rate"]], lower.tail = tail == "lower")
}

#' @export
cdf.dist_exponential <- function(x, q, ..., tail = "lower"){
  stats::pexp(q, x[["rate"]], lower.tail = tail == "lower")
}

#' @export
generate.dist_exponential <- function(x, times, ...){
  vctrs::vec_chop(
    stats::rexp(sum(times), rep(x[["rate"]], times)),
    sizes = times
  )
}

#' @export
support.dist_exponential <- function(x, ...) {
  n <- vctrs::vec_size(x)
  new_support_region(
    rep_len(list(numeric()), n),
    rep_len(list(c(0, Inf)), n),
    rep_len(list(c(TRUE, FALSE)), n)
  )
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
skewness.dist_exponential <- function(x, ...) rep_len(2, vctrs::vec_size(x))

#' @export
kurtosis.dist_exponential <- function(x, ...) rep_len(6, vctrs::vec_size(x))
