#' The Negative Binomial distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generalization of the geometric distribution. It is the number
#' of failures in a sequence of i.i.d. Bernoulli trials before
#' a specified number of successes (`size`) occur. The probability of success in
#' each trial is given by `prob`.
#'
#' @param size The number of successful trials (target number of successes).
#'   Must be a positive number. Also called the dispersion parameter.
#' @param prob The probability of success in each trial. Must be between 0 and 1.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_negative_binomial")`
#'
#'   In the following, let \eqn{X} be a Negative Binomial random variable with
#'   success probability `prob` = \eqn{p} and the number of successes `size` =
#'   \eqn{r}.
#'
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}
#'
#'   **Mean**: \eqn{\frac{r(1-p)}{p}}
#'
#'   **Variance**: \eqn{\frac{r(1-p)}{p^2}}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'      P(X = k) = \binom{k + r - 1}{k} (1-p)^r p^k
#'   }{
#'      P(X = k) = choose(k+r-1, k) * (1-p)^r * p^k
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'      F(k) = \sum_{i=0}^{\lfloor k \rfloor} \binom{i + r - 1}{i} (1-p)^r p^i
#'   }{
#'      F(k) = sum_{i=0}^floor(k) choose(i+r-1, i) * (1-p)^r * p^i
#'   }
#'
#'   This can also be expressed in terms of the regularized incomplete beta
#'   function, and is computed numerically.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'      E(e^{tX}) = \left(\frac{1-p}{1-pe^t}\right)^r, \quad t < -\log p
#'   }{
#'      E(e^(tX)) = ((1-p)/(1-p*e^t))^r, t < -log p
#'   }
#'
#'   **Skewness**:
#'
#'   \deqn{
#'      \gamma_1 = \frac{2-p}{\sqrt{r(1-p)}}
#'   }{
#'      \gamma_1 = (2-p) / sqrt(r(1-p))
#'   }
#'
#'   **Excess Kurtosis**:
#'
#'   \deqn{
#'      \gamma_2 = \frac{6}{r} + \frac{p^2}{r(1-p)}
#'   }{
#'      \gamma_2 = 6/r + p^2/(r(1-p))
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
#' support(dist)
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
covariance.dist_negbin <- function(x, ...){
  x[["n"]] * (1 - x[["p"]]) / x[["p"]]^2
}

#' @export
skewness.dist_negbin <- function(x, ...) {
  (2 - x[["p"]]) / sqrt(x[["n"]] * (1 - x[["p"]]))
}

#' @export
kurtosis.dist_negbin <- function(x, ...) {
  6 / x[["n"]] + x[["p"]]^2 / (x[["n"]] * (1 - x[["p"]]))
}
