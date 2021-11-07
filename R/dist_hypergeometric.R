#' The Hypergeometric distribution
#'
#' \lifecycle{stable}
#'
#' To understand the HyperGeometric distribution, consider a set of
#' \eqn{r} objects, of which \eqn{m} are of the type I and
#' \eqn{n} are of the type II. A sample with size \eqn{k} (\eqn{k<r})
#' with no replacement is randomly chosen. The number of observed
#' type I elements observed in this sample is set to be our random
#' variable \eqn{X}.
#'
#' @param m The number of type I elements available.
#' @param n The number of type II elements available.
#' @param k The size of the sample taken.
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a HyperGeometric random variable with
#'   success probability `p` = \eqn{p = m/(m+n)}.
#'
#'   **Support**: \eqn{x \in { \{\max{(0, k-n)}, \dots, \min{(k,m)}}\}}
#'
#'   **Mean**: \eqn{\frac{km}{n+m} = kp}
#'
#'   **Variance**: \eqn{\frac{km(n)(n+m-k)}{(n+m)^2 (n+m-1)} =
#'   kp(1-p)(1 - \frac{k-1}{m+n-1})}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = \frac{{m \choose x}{n \choose k-x}}{{m+n \choose k}}
#'   }{
#'     P(X = x) = \frac{{m \choose x}{n \choose k-x}}{{m+n \choose k}}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) \approx \Phi\Big(\frac{x - kp}{\sqrt{kp(1-p)}}\Big)
#'  }
#'
#' @seealso [stats::Hypergeometric]
#'
#' @examples
#' dist <- dist_hypergeometric(m = rep(500, 3), n = c(50, 60, 70), k = c(100, 200, 300))
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
#' @name dist_hypergeometric
#' @export
dist_hypergeometric <- function(m, n, k){
  m <- vec_cast(m, integer())
  n <- vec_cast(n, integer())
  k <- vec_cast(k, integer())
  new_dist(m = m, n = n, k = k, class = "dist_hypergeometric")
}

#' @export
format.dist_hypergeometric <- function(x, digits = 2, ...){
  sprintf(
    "Hypergeometric(%s, %s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["n"]], digits = digits, ...),
    format(x[["k"]], digits = digits, ...)
  )
}

#' @export
density.dist_hypergeometric <- function(x, at, ...){
  stats::dhyper(at, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
log_density.dist_hypergeometric <- function(x, at, ...){
  stats::dhyper(at, x[["m"]], x[["n"]], x[["k"]], log = TRUE)
}

#' @export
quantile.dist_hypergeometric <- function(x, p, ...){
  stats::qhyper(p, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
cdf.dist_hypergeometric <- function(x, q, ...){
  stats::phyper(q, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
generate.dist_hypergeometric <- function(x, times, ...){
  stats::rhyper(times, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
mean.dist_hypergeometric <- function(x, ...){
  p <- x[["m"]]/(x[["m"]] + x[["n"]])
  x[["k"]] * p
}

#' @export
covariance.dist_hypergeometric <- function(x, ...){
  m <- x[["m"]]
  n <- x[["n"]]
  k <- x[["k"]]
  p <- m/(m + n)
  k * p * (1 - p) * ((m + n - k) / (m + n - 1))
}

#' @export
skewness.dist_hypergeometric <- function(x, ...) {
  N <- x[["n"]] + x[["m"]]
  K <- x[["m"]]
  n <- x[["k"]]

  a <- (N - 2 * K) * (N - 1)^0.5 * (N - 2 * n)
  b <- (n * K * (N - K) * (N - n))^0.5 * (N - 2)
  a / b
}

#' @export
kurtosis.dist_hypergeometric <- function(x, ...) {
  N <- x[["n"]] + x[["m"]]
  K <- x[["m"]]
  n <- x[["k"]]

  1 / (n * K * (N - K) * (N - n) * (N - 2) * (N - 3))
}
