#' The Bernoulli distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Bernoulli distributions are used to represent events like coin flips
#' when there is single trial that is either successful or unsuccessful.
#' The Bernoulli distribution is a special case of the [Binomial()]
#' distribution with `n = 1`.
#'
#' @inheritParams dist_binomial
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_bernoulli")`
#'
#'   In the following, let \eqn{X} be a Bernoulli random variable with parameter
#'   `prob` = \eqn{p}. Some textbooks also define \eqn{q = 1 - p}, or use
#'   \eqn{\pi} instead of \eqn{p}.
#'
#'   The Bernoulli probability  distribution is widely used to model
#'   binary variables, such as 'failure' and 'success'. The most
#'   typical example is the flip of a coin, when  \eqn{p} is thought as the
#'   probability of flipping a head, and \eqn{q = 1 - p} is the
#'   probability of flipping a tail.
#'
#'   **Support**: \eqn{\{0, 1\}}{{0, 1}}
#'
#'   **Mean**: \eqn{p}
#'
#'   **Variance**: \eqn{p \cdot (1 - p) = p \cdot q}{p (1 - p)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = p^x (1 - p)^{1-x} = p^x q^{1-x}
#'   }{
#'     P(X = x) = p^x (1 - p)^(1-x)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) =
#'     \left \{
#'       \begin{array}{ll}
#'         0 & x < 0 \\
#'         1 - p & 0 \leq x < 1 \\
#'         1 & x \geq 1
#'       \end{array}
#'     \right.
#'   }{
#'     P(X \le x) = (1 - p) 1_{[0, 1)}(x) + 1_{1}(x)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = (1 - p) + p e^t
#'   }{
#'     E(e^(tX)) = (1 - p) + p e^t
#'   }
#'
#'   **Skewness**:
#'
#'   \deqn{
#'     \frac{1 - 2p}{\sqrt{p(1-p)}} = \frac{q - p}{\sqrt{pq}}
#'   }{
#'     (1 - 2p) / sqrt(p(1-p))
#'   }
#'
#'   **Excess Kurtosis**:
#'
#'   \deqn{
#'     \frac{1 - 6p(1-p)}{p(1-p)} = \frac{1 - 6pq}{pq}
#'   }{
#'     (1 - 6p(1-p)) / (p(1-p))
#'   }
#'
#' @seealso [stats::Binomial]
#'
#' @examples
#' dist <- dist_bernoulli(prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
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
#' @name dist_bernoulli
#' @export
dist_bernoulli <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_bernoulli")
}

#' @export
format.dist_bernoulli <- function(x, digits = 2, ...){
  sprintf(
    "Bernoulli(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_bernoulli <- function(x, at, ...){
  stats::dbinom(at, 1, x[["p"]])
}

#' @export
log_density.dist_bernoulli <- function(x, at, ...){
  stats::dbinom(at, 1, x[["p"]], log = TRUE)
}

#' @export
quantile.dist_bernoulli <- function(x, p, ...){
  as.logical(stats::qbinom(p, 1, x[["p"]]))
}

#' @export
cdf.dist_bernoulli <- function(x, q, ...){
  stats::pbinom(q, 1, x[["p"]])
}

#' @export
generate.dist_bernoulli <- function(x, times, ...){
  as.logical(stats::rbinom(times, 1, x[["p"]]))
}

#' @export
mean.dist_bernoulli <- function(x, ...){
  x[["p"]]
}

#' @export
covariance.dist_bernoulli <- function(x, ...){
  x[["p"]]*(1-x[["p"]])
}

#' @export
skewness.dist_bernoulli <- function(x, ...) {
  p <- x[["p"]]
  q <- 1 - x[["p"]]
  (1 - (2 * p)) / sqrt(p * q)
}

#' @export
kurtosis.dist_bernoulli <- function(x, ...) {
  p <- x[["p"]]
  q <- 1 - x[["p"]]
  (1 - (6 * p * q)) / (p * q)
}
