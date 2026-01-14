#' The Poisson Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Poisson distributions are frequently used to model counts. The Poisson
#' distribution is commonly used to model the number of events occurring in
#' a fixed interval of time or space when these events occur with a known
#' constant mean rate and independently of the time since the last event.
#' Examples include the number of emails received per hour, the number of
#' decay events per second from a radioactive source, or the number of
#' customers arriving at a store per day.
#'
#' @param lambda The rate parameter (mean and variance) of the distribution.
#'   Can be any positive number. This represents the expected number of events
#'   in the given interval.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_poisson")`
#' 
#'
#'   In the following, let \eqn{X} be a Poisson random variable with parameter
#'   `lambda` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}{{0, 1, 2, 3, ...}}
#'
#'   **Mean**: \eqn{\lambda}
#'
#'   **Variance**: \eqn{\lambda}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = k) = \frac{\lambda^k e^{-\lambda}}{k!}
#'   }{
#'     P(X = k) = \lambda^k e^(-\lambda) / k!
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) = e^{-\lambda}
#'     \sum_{i = 0}^{\lfloor k \rfloor} \frac{\lambda^i}{i!}
#'   }{
#'     P(X \le k) = e^(-\lambda)
#'     \sum_{i = 0}^k \lambda^i / i!
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\lambda (e^t - 1)}
#'   }{
#'     E(e^(tX)) = e^(\lambda (e^t - 1))
#'   }
#'
#'   **Skewness**:
#'
#'   \deqn{
#'     \gamma_1 = \frac{1}{\sqrt{\lambda}}
#'   }{
#'     \gamma_1 = 1 / sqrt(\lambda)
#'   }
#'
#'   **Excess kurtosis**:
#'
#'   \deqn{
#'     \gamma_2 = \frac{1}{\lambda}
#'   }{
#'     \gamma_2 = 1 / \lambda
#'   }
#'
#' @seealso [stats::Poisson]
#'
#' @examples
#' dist <- dist_poisson(lambda = c(1, 4, 10))
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
#' @name dist_poisson
#' @export
dist_poisson <- function(lambda){
  lambda <- vec_cast(lambda, double())
  if(any(lambda < 0)){
    abort("The lambda parameter of an Poisson distribution must be non-negative.")
  }
  new_dist(l = lambda, class = "dist_poisson")
}

#' @export
format.dist_poisson <- function(x, digits = 2, ...){
  sprintf(
    "Pois(%s)",
    format(x[["l"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson <- function(x, at, ...){
  stats::dpois(at, x[["l"]])
}

#' @export
log_density.dist_poisson <- function(x, at, ...){
  stats::dpois(at, x[["l"]], log = TRUE)
}

#' @export
quantile.dist_poisson <- function(x, p, ...){
  stats::qpois(p, x[["l"]])
}

#' @export
cdf.dist_poisson <- function(x, q, ...){
  stats::ppois(q, x[["l"]])
}

#' @export
generate.dist_poisson <- function(x, times, ...){
  as.integer(stats::rpois(times, x[["l"]]))
}

#' @export
mean.dist_poisson <- function(x, ...){
  x[["l"]]
}

#' @export
covariance.dist_poisson <- function(x, ...){
  x[["l"]]
}

#' @export
skewness.dist_poisson <- function(x, ...) 1 / sqrt(x[["l"]])

#' @export
kurtosis.dist_poisson <- function(x, ...) 1 / x[["l"]]
