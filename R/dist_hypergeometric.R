#' The Hypergeometric distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
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
#' `r pkgdown_doc_link("dist_hypergeometric")`
#'
#'
#'   In the following, let \eqn{X} be a HyperGeometric random variable with
#'   success probability `p` = \eqn{p = m/(m+n)}.
#'
#'   **Support**: \eqn{x \in \{\max(0, k-n), \dots, \min(k,m)\}}{\max(0, k-n), ..., min(k,m)}
#'
#'   **Mean**: \eqn{\frac{km}{m+n} = kp}
#'
#'   **Variance**: \eqn{\frac{kmn(m+n-k)}{(m+n)^2 (m+n-1)} =
#'   kp(1-p)\left(1 - \frac{k-1}{m+n-1}\right)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = \frac{{m \choose x}{n \choose k-x}}{{m+n \choose k}}
#'   }{
#'     P(X = x) = choose(m, x) * choose(n, k-x) / choose(m+n, k)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) = \sum_{i = \max(0, k-n)}^{\lfloor x \rfloor}
#'     \frac{{m \choose i}{n \choose k-i}}{{m+n \choose k}}
#'   }{
#'     P(X <= x) = sum_{i = max(0, k-n)}^floor(x)
#'     choose(m, i) * choose(n, k-i) / choose(m+n, k)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{{m \choose k}}{{m+n \choose k}}{}_2F_1(-m, -k; m+n-k+1; e^t)
#'   }{
#'     E(e^(tX)) = choose(m, k) / choose(m+n, k) * 2F1(-m, -k; m+n-k+1; e^t)
#'   }
#'
#'   where \eqn{_2F_1} is the hypergeometric function.
#'
#'   **Skewness**:
#'
#'   \deqn{
#'     \frac{(m+n-2k)(m+n-1)^{1/2}(m+n-2n)}{[kmn(m+n-k)]^{1/2}(m+n-2)}
#'   }{
#'     [(m+n-2k) * sqrt(m+n-1) * (m+n-2n)] / [sqrt(k*m*n*(m+n-k)) * (m+n-2)]
#'   }
#'
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
