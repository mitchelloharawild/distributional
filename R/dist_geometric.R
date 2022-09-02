#' The Geometric Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Geometric distribution can be thought of as a generalization
#' of the [dist_bernoulli()] distribution where we ask: "if I keep flipping a
#' coin with probability `p` of heads, what is the probability I need
#' \eqn{k} flips before I get my first heads?" The Geometric
#' distribution is a special case of Negative Binomial distribution.
#'
#' @inheritParams stats::dgeom
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Geometric random variable with
#'   success probability `p` = \eqn{p}. Note that there are multiple
#'   parameterizations of the Geometric distribution.
#'
#'   **Support**: 0 < p < 1, \eqn{x = 0, 1, \dots}
#'
#'   **Mean**: \eqn{\frac{1-p}{p}}
#'
#'   **Variance**: \eqn{\frac{1-p}{p^2}}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = p(1-p)^x,
#'    }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) = 1 - (1-p)^{x+1}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{pe^t}{1 - (1-p)e^t}
#'   }{
#'     E(e^{tX}) = \frac{pe^t}{1 - (1-p)e^t}
#'   }
#'
#' @seealso [stats::Geometric]
#'
#' @examples
#' dist <- dist_geometric(prob = c(0.2, 0.5, 0.8))
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
#' @name dist_geometric
#' @export
dist_geometric <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The prob parameter of an Geometric distribution must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_geometric")
}

#' @export
format.dist_geometric <- function(x, digits = 2, ...){
  sprintf(
    "Geometric(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_geometric <- function(x, at, ...){
  stats::dgeom(at, x[["p"]])
}

#' @export
log_density.dist_geometric <- function(x, at, ...){
  stats::dgeom(at, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_geometric <- function(x, p, ...){
  stats::qgeom(p, x[["p"]])
}

#' @export
cdf.dist_geometric <- function(x, q, ...){
  stats::pgeom(q, x[["p"]])
}

#' @export
generate.dist_geometric <- function(x, times, ...){
  stats::rgeom(times, x[["p"]])
}

#' @export
mean.dist_geometric <- function(x, ...){
  1/x[["p"]] - 1
}

#' @export
covariance.dist_geometric <- function(x, ...){
  (1 - x[["p"]])/x[["p"]]^2
}

#' @export
skewness.dist_geometric <- function(x, ...) (2 - x[["p"]]) / sqrt(1 - x[["p"]])

#' @export
kurtosis.dist_geometric <- function(x, ...) 6 + (x[["p"]]^2 / (1 - x[["p"]]))
