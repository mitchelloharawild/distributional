#' The Negative Binomial distribution
#'
#' \lifecycle{stable}
#'
#' A generalization of the geometric distribution. It is the number
#' of successes in a sequence of i.i.d. Bernoulli trials before
#' a specified number (\eqn{r}) of failures occurs.
#'
#' @inheritParams stats::NegBinomial
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Negative Binomial random variable with
#'   success probability `p` = \eqn{p}.
#'
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}
#'
#'   **Mean**: \eqn{\frac{p r}{1-p}}
#'
#'   **Variance**: \eqn{\frac{pr}{(1-p)^2}}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'      f(k) = {k + r - 1 \choose k} \cdot (1-p)^r p^k
#'   }{
#'      f(k) = (k+r-1)!/(k!(r-1)!) (1-p)^r p^k
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   Too nasty, omitted.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'      \left(\frac{1-p}{1-pe^t}\right)^r, t < -\log p
#'   }{
#'      \frac{(1-p)^r}{(1-pe^t)^r}, t < -\log p
#'   }
#'
#' @seealso [stats::NegBinomial]
#'
#' @examples
#' dist <- dist_negative_binomial(size = 10, prob = 0.5)
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
dist_negative_binomial <- function(size, prob){
  size <- vec_cast(size, double())
  prob <- vec_cast(prob, double())
  if(any(prob < 0 | prob > 1)){
    abort("Probability of success must be between 0 and 1.")
  }
  new_dist(n = size, p = prob, class = "dist_negbin")
}

#' @export
print.dist_negbin <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_negbin <- function(x, digits = 2, ...){
  sprintf(
    "NB(%s, %s)",
    format(x[["n"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_negbin <- function(x, at, ...){
  stats::dnbinom(at, x[["n"]], x[["p"]])
}

#' @export
log_density.dist_negbin <- function(x, at, ...){
  stats::dnbinom(at, x[["n"]], x[["p"]], log = TRUE)
}

#' @export
quantile.dist_negbin <- function(x, p, ...){
  stats::qnbinom(p, x[["n"]], x[["p"]])
}

#' @export
cdf.dist_negbin <- function(x, q, ...){
  stats::pnbinom(q, x[["n"]], x[["p"]])
}

#' @export
generate.dist_negbin <- function(x, times, ...){
  stats::rnbinom(times, x[["n"]], x[["p"]])
}

#' @export
mean.dist_negbin <- function(x, ...){
  x[["n"]] * (1 - x[["p"]]) / x[["p"]]
}

#' @export
variance.dist_negbin <- function(x, ...){
  x[["n"]] * (1 - x[["p"]]) / x[["p"]]^2
}

#' @export
skewness.dist_negbin <- function(x, ...) {
  (1 + x[["p"]]) / sqrt(x[["p"]] * x[["n"]])
}

#' @export
kurtosis.dist_negbin <- function(x, ...) {
  6 / x[["n"]] + (1 - x[["p"]])^2 / x[["n"]] * x[["p"]]
}
