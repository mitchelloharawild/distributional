#' The Uniform distribution
#'
#' \lifecycle{stable}
#'
#' A distribution with constant density on an interval.
#'
#' @inheritParams stats::dunif
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Poisson random variable with parameter
#'   `lambda` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{[a,b]}{[a,b]}
#'
#'   **Mean**: \eqn{\frac{1}{2}(a+b)}
#'
#'   **Variance**: \eqn{\frac{1}{12}(b-a)^2}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{b-a} for x \in [a,b]
#'   }{
#'     f(x) = \frac{1}{b-a} for x in [a,b]
#'   }
#'   \deqn{
#'     f(x) = 0 otherwise
#'   }{
#'     f(x) = 0 otherwise
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 0 for x < a
#'   }{
#'     F(x) = 0 for x < a
#'   }
#'   \deqn{
#'     F(x) = \frac{x - a}{b-a} for x \in [a,b]
#'   }{
#'     F(x) = \frac{x - a}{b-a} for x in [a,b]
#'   }
#'   \deqn{
#'     F(x) = 1 for x > b
#'   }{
#'     F(x) = 1 for x > b
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{e^{tb} - e^{ta}}{t(b-a)} for t \neq 0
#'   }{
#'     E(e^(tX)) = \frac{e^{tb} - e^{ta}}{t(b-a)} for t \neq 0
#'   }
#'   \deqn{
#'     E(e^{tX}) = 1 for t = 0
#'   }{
#'     E(e^(tX)) = 1 for t = 0
#'   }
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
print.dist_uniform <- function(x, ...){
  cat(format(x, ...))
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
variance.dist_uniform <- function(x, ...){
  (x[["u"]]-x[["l"]])^2/12
}

#' @export
skewness.dist_uniform <- function(x, ...) 0

#' @export
kurtosis.dist_uniform <- function(x, ...) -6/5
