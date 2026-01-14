#' The Logarithmic distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Logarithmic distribution is a discrete probability distribution
#' derived from the logarithmic series. It is useful in modeling the
#' abundance of species and other phenomena where the frequency of
#' an event follows a logarithmic pattern.
#'
#' @inheritParams actuar::dlogarithmic
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_logarithmic")`
#'
#'   In the following, let \eqn{X} be a Logarithmic random variable with
#'   parameter `prob` = \eqn{p}.
#'
#'   **Support**: \eqn{\{1, 2, 3, ...\}}{{1, 2, 3, ...}}
#'
#'   **Mean**: \eqn{\frac{-1}{\log(1-p)} \cdot \frac{p}{1-p}}{-1/log(1-p) * p/(1-p)}
#'
#'   **Variance**: \eqn{\frac{-(p^2 + p\log(1-p))}{[(1-p)\log(1-p)]^2}}{-(p^2 + p*log(1-p))/((1-p)*log(1-p))^2}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = k) = \frac{-1}{\log(1-p)} \cdot \frac{p^k}{k}
#'   }{
#'     P(X = k) = -1/log(1-p) * p^k/k
#'   }
#'
#'   for \eqn{k = 1, 2, 3, \ldots}{k = 1, 2, 3, ...}
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The c.d.f. does not have a simple closed form. It is computed
#'   using the recurrence relationship
#'   \eqn{P(X = k+1) = \frac{p \cdot k}{k+1} \cdot P(X = k)}{P(X = k+1) = (p*k)/(k+1) * P(X = k)}
#'   starting from \eqn{P(X = 1) = \frac{-p}{\log(1-p)}}{P(X = 1) = -p/log(1-p)}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{\log(1 - pe^t)}{\log(1-p)}
#'   }{
#'     E(e^(tX)) = log(1 - p*e^t)/log(1-p)
#'   }
#'
#'   for \eqn{pe^t < 1}{p*e^t < 1}
#'
#' @seealso [actuar::Logarithmic]
#'
#' @examples
#' dist <- dist_logarithmic(prob = c(0.33, 0.66, 0.99))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' @name dist_logarithmic
#' @export
dist_logarithmic <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The prob parameter of a Logarithmic distribution must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_logarithmic")
}

#' @export
format.dist_logarithmic <- function(x, digits = 2, ...){
  sprintf(
    "Logarithmic(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_logarithmic <- function(x, at, ...){
  require_package("actuar")
  actuar::dlogarithmic(at, x[["p"]])
}

#' @export
log_density.dist_logarithmic <- function(x, at, ...){
  require_package("actuar")
  actuar::dlogarithmic(at, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_logarithmic <- function(x, p, ...){
  require_package("actuar")
  actuar::qlogarithmic(p, x[["p"]])
}

#' @export
cdf.dist_logarithmic <- function(x, q, ...){
  require_package("actuar")
  actuar::plogarithmic(q, x[["p"]])
}

#' @export
generate.dist_logarithmic <- function(x, times, ...){
  require_package("actuar")
  actuar::rlogarithmic(times, x[["p"]])
}

#' @export
mean.dist_logarithmic <- function(x, ...){
  p <- x[["p"]]
  (-1/(log(1-p)))*(p/(1-p))
}

#' @export
covariance.dist_logarithmic <- function(x, ...){
  p <- x[["p"]]
  -(p^2 + p*log(1-p))/((1-p)*log(1-p))^2
}
