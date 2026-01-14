#' The Uniform distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A distribution with constant density on an interval.
#'
#' @inheritParams stats::dunif
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_uniform")`
#'
#'   In the following, let \eqn{X} be a Uniform random variable with parameters
#'   `min` = \eqn{a} and `max` = \eqn{b}.
#'
#'   **Support**: \eqn{[a, b]}
#'
#'   **Mean**: \eqn{\frac{a + b}{2}}
#'
#'   **Variance**: \eqn{\frac{(b - a)^2}{12}}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{b - a}
#'   }{
#'     f(x) = 1 / (b - a)
#'   }
#'
#'   for \eqn{x \in [a, b]}{x in [a, b]}, and \eqn{f(x) = 0} otherwise.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \frac{x - a}{b - a}
#'   }{
#'     F(x) = (x - a) / (b - a)
#'   }
#'
#'   for \eqn{x \in [a, b]}{x in [a, b]}, with \eqn{F(x) = 0} for \eqn{x < a}
#'   and \eqn{F(x) = 1} for \eqn{x > b}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{e^{tb} - e^{ta}}{t(b - a)}
#'   }{
#'     E(e^(tX)) = (e^(tb) - e^(ta)) / (t(b - a))
#'   }
#'
#'   for \eqn{t \neq 0}{t != 0}, and \eqn{E(e^{tX}) = 1} for \eqn{t = 0}.
#'
#'   **Skewness**: \eqn{0}
#'
#'   **Excess Kurtosis**: \eqn{-\frac{6}{5}}{-6/5}
#'
#' @seealso [stats::Uniform]
#'
#' @examples
#' dist <- dist_uniform(min = c(3, -2), max = c(5, 4))
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
#' @name dist_uniform
#' @export
dist_uniform <- function(min, max){
  min <- vec_cast(min, double())
  max <- vec_cast(max, double())
  if(any(min > max)){
    abort("The min of a Uniform distribution must be less than max.")
  }
  new_dist(l = min, u = max, class = "dist_uniform")
}

#' @export
format.dist_uniform <- function(x, digits = 2, ...){
  sprintf(
    "U(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["u"]], digits = digits, ...)
  )
}

#' @export
density.dist_uniform <- function(x, at, ...){
  stats::dunif(at, x[["l"]], x[["u"]])
}

#' @export
log_density.dist_uniform <- function(x, at, ...){
  stats::dunif(at, x[["l"]], x[["u"]], log = TRUE)
}

#' @export
quantile.dist_uniform <- function(x, p, ...){
  stats::qunif(p, x[["l"]], x[["u"]])
}

#' @export
cdf.dist_uniform <- function(x, q, ...){
  stats::punif(q, x[["l"]], x[["u"]])
}

#' @export
generate.dist_uniform <- function(x, times, ...){
  stats::runif(times, x[["l"]], x[["u"]])
}

#' @export
mean.dist_uniform <- function(x, ...){
  (x[["u"]]+x[["l"]])/2
}

#' @export
covariance.dist_uniform <- function(x, ...){
  (x[["u"]]-x[["l"]])^2/12
}

#' @export
skewness.dist_uniform <- function(x, ...) 0

#' @export
kurtosis.dist_uniform <- function(x, ...) -6/5

#' @export
has_symmetry.dist_uniform <- function(x, ...) TRUE