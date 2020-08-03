#' The Binomial distribution
#'
#' \lifecycle{stable}
#'
#' Binomial distributions are used to represent situations can that can
#' be thought as the result of \eqn{n} Bernoulli experiments (here the
#' \eqn{n} is defined as the `size` of the experiment). The classical
#' example is \eqn{n} independent coin flips, where each coin flip has
#' probability `p` of success. In this case, the individual probability of
#' flipping heads or tails is given by the  Bernoulli(p) distribution,
#' and the probability of having \eqn{x} equal results (\eqn{x} heads,
#' for example), in \eqn{n} trials is given by the Binomial(n, p) distribution.
#' The equation of the Binomial distribution is directly derived from
#' the equation of the Bernoulli distribution.
#'
#' @param size The number of trials. Must be an integer greater than or equal
#'   to one. When `size = 1L`, the Binomial distribution reduces to the
#'   Bernoulli distribution. Often called `n` in textbooks.
#' @param prob The probability of success on each trial, `prob` can be any
#'   value in `[0, 1]`.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   The Binomial distribution comes up when you are interested in the portion
#'   of people who do a thing. The Binomial distribution
#'   also comes up in the sign test, sometimes called the Binomial test
#'   (see [stats::binom.test()]), where you may need the Binomial C.D.F. to
#'   compute p-values.
#'
#'   In the following, let \eqn{X} be a Binomial random variable with parameter
#'   `size` = \eqn{n} and `p` = \eqn{p}. Some textbooks define \eqn{q = 1 - p},
#'   or called \eqn{\pi} instead of \eqn{p}.
#'
#'   **Support**: \eqn{\{0, 1, 2, ..., n\}}{{0, 1, 2, ..., n}}
#'
#'   **Mean**: \eqn{np}
#'
#'   **Variance**: \eqn{np \cdot (1 - p) = np \cdot q}{np (1 - p)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = k) = {n \choose k} p^k (1 - p)^{n-k}
#'   }{
#'     P(X = k) = choose(n, k) p^k (1 - p)^(n - k)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le k) = \sum_{i=0}^{\lfloor k \rfloor} {n \choose i} p^i (1 - p)^{n-i}
#'   }{
#'     P(X \le k) = \sum_{i=0}^k choose(n, i) p^i (1 - p)^(n-i)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = (1 - p + p e^t)^n
#'   }{
#'     E(e^(tX)) = (1 - p + p e^t)^n
#'   }
#'
#' @examples
#' dist <- dist_binomial(size = 1:5, prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
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
#' @name dist_binomial
#' @export
dist_binomial <- function(size, prob){
  size <- vec_cast(size, integer())
  prob <- vec_cast(prob, double())
  if(any(size < 0)){
    abort("The number of observations cannot be negative.")
  }
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(n = size, p = prob, class = "dist_binomial")
}

#' @export
print.dist_binomial <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_binomial <- function(x, digits = 2, ...){
  sprintf(
    "B(%s, %s)",
    format(x[["n"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_binomial <- function(x, at, ...){
  stats::dbinom(at, x[["n"]], x[["p"]])
}

#' @export
log_density.dist_binomial <- function(x, at, ...){
  stats::dbinom(at, x[["n"]], x[["p"]], log = TRUE)
}

#' @export
quantile.dist_binomial <- function(x, p, ...){
  stats::qbinom(p, x[["n"]], x[["p"]])
}

#' @export
cdf.dist_binomial <- function(x, q, ...){
  stats::pbinom(q, x[["n"]], x[["p"]])
}

#' @export
generate.dist_binomial <- function(x, times, ...){
  stats::rbinom(times, x[["n"]], x[["p"]])
}

#' @export
mean.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]
}

#' @export
variance.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]*(1-x[["p"]])
}

#' @export
skewness.dist_binomial <- function(x, ...) {
  n <- x[["n"]]
  p <- x[["p"]]
  q <- 1 - p
  (1 - (2 * p)) / sqrt(n * p * q)
}

#' @export
kurtosis.dist_binomial <- function(x, ...) {
  n <- x[["n"]]
  p <- x[["p"]]
  q <- 1 - p
  (1 - (6 * p * q)) / (n * p * q)
}
